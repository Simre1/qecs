module Qecs.Compile.Optimize where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (TExp (..))
import Debug.Trace


-- Does not work because name bindings do get different names even when their content is the same
-- Therefore expressions are not equal even if they produce the same result
optimize :: Code Q (a -> b -> c) -> Code Q a -> Code Q b -> Code Q c
optimize combineCode code1 code2 = Code $ do
  (TExp combine) <- examineCode combineCode
  (TExp exp1) <- examineCode code1
  (TExp exp2) <- examineCode code2
  TExp
    <$> if traceShow (exp1, exp2) $  (exp1 == exp2)
      then [|let x = $(pure exp1) in $(pure combine) x x|]
      else [|$(pure combine) $(pure exp1) $(pure exp2)|]
