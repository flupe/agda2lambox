module Agda2Lambox.Convert.Function 
  ( convertFunction
  ) where

import Control.Monad.Reader ( ask, liftIO )
import Data.List ( elemIndex )

import Utils

import Agda ( liftTCM, getConstructorData, getConstructors )
import qualified Agda as A
import Agda.Lib ( )
import Agda.Utils
import Agda.Syntax.Literal
import Agda.Syntax.Abstract.Name ( qnameModule )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Env ( withCurrentModule )
import Agda.Compiler.ToTreeless ( toTreeless )
import Agda.Syntax.Treeless ( EvaluationStrategy(EagerEvaluation) )
import Agda.Syntax.Internal ( unEl )

import LambdaBox (Term(..), GlobalDecl(..))
import qualified LambdaBox as L

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class
import Agda2Lambox.Convert.Term

convertFunction :: Definition :~> Term
convertFunction Defn{..} =
  withCurrentModule (qnameModule defName) do
    Just tterm <- liftTCM $ toTreeless EagerEvaluation defName
    let Function{..} = theDef
        Just ds      = funMutual
    tm <- inMutuals ds $ convert tterm
    let tm' = case ds of []    -> tm
                         [d]   -> LFix [L.Def (L.Named $ pp defName) tm 0] 0
                         _:_:_ -> error "Mutual recursion not supported."
    return tm'
