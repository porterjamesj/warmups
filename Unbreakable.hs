import Data.Char
import Data.List.Split
import Control.Monad

-- the "alphabet" we're handling is defined by the first argument
unbreakable :: (Char -> Bool) -> String -> String
unbreakable f = join . (zipWith ($) (cycle [reverse, id])) . split (whenElt $ not .f)

-- for example, to split on anything that's not an alphanumeric character
unbreakableAlpha = unbreakable isAlphaNum
