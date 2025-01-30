{-# LANGUAGE NamedFieldPuns, FlexibleInstances, DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
module Agda2Lambox.Compile.Utils
  ( modNameToModPath
  , qnameToKName
  , qnameToName
  , dataOrRecDefMutuals
  , dataOrRecMutuals
  , toInductive
  , toConApp
  , MayBeLogical(isLogical)
  , sanitize
  , CompiledItem(..)
  , topoSort
  ) where

import Control.Monad.State
import Control.Monad.IO.Class ( liftIO )
import Data.Bifunctor ( bimap )
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe, listToMaybe, isJust, catMaybes )
import Data.Set ( Set )
import Data.Map ( Map )
import Data.Set qualified as Set
import Data.Map qualified as Map

import Agda.Syntax.Internal
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common.Pretty ( prettyShow, Doc )
import Agda.Syntax.Common ( usableModality, Arg(..) )
import Agda.TypeChecking.Datatypes ( getConstructors, getConstructorData )
import Agda.TypeChecking.Level ( isLevelType )
import Agda.TypeChecking.Monad.SizedTypes ( isSizeType )
import Agda.TypeChecking.Monad.Base ( TCM )
import Agda.Utils.Monad ( unlessM )
import Agda.Compiler.Backend 

import LambdaBox qualified as LBox
import Agda.TypeChecking.Substitute (TelV(TelV))
import Agda.TypeChecking.Telescope (telView)
import Agda.Utils.Monad (orM)
import Data.Char (isLower, isUpper, GeneralCategory (DecimalNumber), generalCategory, isAscii)


-- | Convert and Agda module name to its "equivalent" λ□ module path.
modNameToModPath :: ModuleName -> LBox.ModPath
modNameToModPath =
  LBox.MPFile . map (sanitize . prettyShow) . mnameToList


-- | Convert and Agda definition name to a λ□ kernel name.
qnameToKName :: QName -> LBox.KerName
qnameToKName qn =
  LBox.KerName
    (modNameToModPath $ qnameModule qn)
    (sanitize $ prettyShow $ qnameName qn)

qnameToName :: QName -> LBox.Name
qnameToName q = LBox.Named (sanitize $ prettyShow $ qnameName q)

dataOrRecDefMutuals :: Definition -> TCM [QName]
dataOrRecDefMutuals d = do
  case theDef d of
    Datatype{dataMutual} -> pure $ fromMaybe [] dataMutual
    Record  {recMutual}  -> pure $ fromMaybe [] recMutual
    _                    -> internalError "Not a datatype or record"

dataOrRecMutuals :: QName -> TCM [QName]
dataOrRecMutuals q = dataOrRecDefMutuals =<< getConstInfo q

-- | Fetch the λ□ inductive associated with a @QName@.
toInductive :: QName -> TCM LBox.Inductive
toInductive q = do
  names <- dataOrRecMutuals q
  let repr = fromMaybe q $ listToMaybe names
  let idx  = fromMaybe 0 $ elemIndex q names
  pure $ LBox.Inductive (qnameToKName repr) idx


-- | Compile a constructor application to λ□.
toConApp :: QName -> [LBox.Term] -> TCM LBox.Term
toConApp qn es = do
  dt   <- getConstructorData qn
  ctrs <- getConstructors dt
  ind  <- toInductive dt
  let idx = fromMaybe 0 $ qn `elemIndex` ctrs
  pure $ LBox.LConstruct ind idx es


-- | Class for things that may be considered logical, and thus erased.
-- See https://arxiv.org/pdf/2108.02995 for the precise definition.
--
class MayBeLogical a where
  isLogical :: a -> TCM Bool

-- * Logical types
--
-- Note that we may also want to consider logical products 
-- into logical types?, Say "proof builders", or "level builders", etc.

-- | Logical types.
--
-- A type is considered logical when it is a proposition
-- (its inhabitants are proofs) or when it is an arity in Prop.
--
-- @Size@ and @Level@ are also considered logical.
instance MayBeLogical Type where
  isLogical typ = orM
    [ pure $ isLogicalSort $ getSort typ
    , isLevelType typ
    , isJust <$> isSizeType typ
    , do TelV tel typ <- telView typ
         case unEl typ of
           Sort s -> pure $ isLogicalSort s
           _      -> pure False
    ]
    where
      isLogicalSort :: Sort -> Bool
      isLogicalSort = \case
        Prop{}      -> True -- Prop
        Inf UProp _ -> True -- Propw
        SizeUniv{}  -> True -- SizeUniv
        LevelUniv{} -> True -- LevelUniv
        _           -> False

-- | Additionally, we consider erased domains logical.
instance MayBeLogical a => MayBeLogical (Dom a) where
  isLogical dom | not (usableModality dom) = pure True
  isLogical dom = isLogical $ unDom dom

instance MayBeLogical a => MayBeLogical (Arg a) where
  isLogical arg | not (usableModality arg) = pure True
  isLogical arg = isLogical $ unArg arg

-- | Sanitize an agda name to something without unicode.
-- Must be injective.
-- We may require a smarter transformation later on for other targets.
sanitize :: String -> String
sanitize s = concatMap encode s
  where
  encode '$' = "$$"
  encode c
    | isAscii c -- more agressive sanitization
    , isLower c
    || isUpper c
    || c == '_'
    || generalCategory c == DecimalNumber = [c]
    | otherwise = "$" ++ show (fromEnum c)

-- * Compilation items and topological sort

-- | Named compilation item, with a set of dependencies.
data CompiledItem a = CompiledItem
  { itemName  :: QName
  , itemDeps  :: [QName]
  , itemValue :: a
  } deriving (Functor, Foldable, Traversable)

-- | Stateful monad for the topological sort.
-- State contains the list of items that have been permanently inserted,
-- along with their names.
type TopoM a = State (Set QName, [CompiledItem a])

-- | Topological sort of compiled items, based on dependencies.
-- Skipped items are required for dependency analysis, as they
-- can transively force ordering
-- (e.g constructors are skipped but force compilation of their datatype).
-- In the end, we get a list of items that are effectively compiled.
topoSort :: forall a. [CompiledItem (Maybe a)] -> [CompiledItem a]
topoSort defs = snd $ execState (traverse (visit Set.empty) defs) (Set.empty, [])
  where
    items = Map.fromList $ map (\x -> (itemName x, x)) defs

    -- | Whether an item has been permanently inserted already
    isMarked :: QName -> TopoM a Bool
    isMarked q = Set.member q <$> gets fst

    push :: CompiledItem (Maybe a) -> TopoM a ()
    push item@CompiledItem{itemName, itemValue}
      | Just value <- itemValue
      = modify $ bimap (Set.insert itemName) (item {itemValue = value}:)
      | otherwise = pure ()

    visit :: Set QName -> CompiledItem (Maybe a) -> TopoM a ()
    visit temp item@CompiledItem{..} = do
      -- NOTE(flupe): Visiting an item that has already been temporarily marked
      --  means something went wrong and we have a cycle in the graph.
      --  We could throw an error, but this should never happen.
      --  Here, we continue and pick an arbitrary order.
      unlessM ((Set.member itemName temp ||) <$> isMarked itemName) do
        let deps = catMaybes $ (`Map.lookup` items) <$> itemDeps
        traverse (visit (Set.insert itemName temp)) deps
        push item
