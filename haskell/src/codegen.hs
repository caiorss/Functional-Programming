import qualified Data.Text as T

import System.Process

(|>) x f = f x

--- Read clipbard output
---
read_xclip = readProcess "xclip" ["-o", "-selection", "clipboard"] ""
strip_line = T.strip (T.pack "; ")

{-
    code <- readFile "rawcode.txt"
    
-}    
