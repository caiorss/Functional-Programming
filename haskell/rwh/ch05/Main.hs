
module Main (main) where

import SimpleJSON2

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
