import Data.Char

conversion :: String -> [String]
conversion s = let wx = words s
    	       in toUpper wx 
