
module PutJSON where

import Data.List (intercalate)
import SimpleJSON2

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)

-- Good Haskell style involves separating pure code from code that
-- performs I/O. (Real World Haskell) 
--
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
