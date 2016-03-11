- [Data.Text](#data.text)

# Data.Text<a id="sec-1" name="sec-1"></a>

Package: [text: An efficient packed Unicode text type. | Hackage](https://hackage.haskell.org/package/text-1.2.2.0)

[Module Documentation](https://hackage.haskell.org/package/text-1.2.2.0/docs/Data-Text.html)

A time and space-efficient implementation of Unicode text. Suitable
for performance critical use, both in terms of large data quantities
and high speed.

```haskell
 -- Convert String to Text 

> import qualified Data.Text as T
> 

 -- Hit tab to autocomplete after dot
 --  
> T.
T.Text            T.count           T.group           T.map             T.snoc            T.toCaseFold
T.all             T.drop            T.groupBy         T.mapAccumL       T.span            T.toLower
T.any             T.dropAround      T.head            T.mapAccumR       T.split           T.toTitle
T.append          T.dropEnd         T.index           T.maximum         T.splitAt         T.toUpper
T.break           T.dropWhile       T.init            T.minimum         T.splitOn         T.transpose
T.breakOn         T.dropWhileEnd    T.inits           T.null            T.strip           T.uncons
T.breakOnAll      T.empty           T.intercalate     T.pack            T.stripEnd        T.unfoldr
T.breakOnEnd      T.filter          T.intersperse     T.partition       T.stripPrefix     T.unfoldrN
T.center          T.find            T.isInfixOf       T.replace         T.stripStart      T.unlines
T.chunksOf        T.findIndex       T.isPrefixOf      T.replicate       T.stripSuffix     T.unpack
T.commonPrefixes  T.foldl           T.isSuffixOf      T.reverse         T.tail            T.unpackCString#
T.compareLength   T.foldl'          T.justifyLeft     T.scanl           T.tails           T.unwords
T.concat          T.foldl1          T.justifyRight    T.scanl1          T.take            T.words
T.concatMap       T.foldl1'         T.last            T.scanr           T.takeEnd         T.zip
T.cons            T.foldr           T.length          T.scanr1          T.takeWhile       T.zipWith
T.copy            T.foldr1          T.lines           T.singleton       T.takeWhileEnd


-- Converts String to Text 
-- 

> :t T.pack 
T.pack :: String -> Text

> T.pack "Hello world Haskell rocks in FP"
"Hello world Haskell rocks in FP"

> :t T.pack "Hello world Haskell rocks in FP"
T.pack "Hello world Haskell rocks in FP" :: Text
> 

-- Convert Text to String 
--
> :t T.unpack
T.unpack :: T.Text -> String
> 

> let text = T.pack "hello world"
> :t text
text :: T.Text
> let str = T.unpack text
> str
"hello world"
> :t str
str :: String
> 

--
-- Apply a a function from T.Text to a String 
--
> :t apTextf 
apTextf :: (T.Text -> t) -> String -> t
> 
> apTextf T.lines "hello\nworld\nhaskell"
["hello","world","haskell"]
> 


-- Split a Text 
--
-----------------------------------------------

> T.words (T.pack "Hello world Haskell rocks in FP")
["Hello","world","Haskell","rocks","in","FP"]
> 

> T.unwords [T.pack "string1", T.pack "string2", T.pack "string3"]
"string1 string2 string3"
> 

-- Join strings (text) and glue them 
--
----------------------------------------------
> :t T.unwords
T.unwords :: [T.Text] -> T.Text
> 


-- Lenght of each word 
--
> map T.length $ T.words (T.pack "Hello world Haskell rocks in FP") 
[5,5,7,5,2,2]
> 

-- Total length of all words
--
> sum $ map T.length $ T.words (T.pack "Hello world Haskell rocks in FP") 
26

-- Splite Lines 
---------------------------------------------------

> :t T.lines
T.lines :: T.Text -> [T.Text]
> 

> let text = T.pack "line1\nline2\nline3\nline4"

> let lines = T.lines text
> 

> :t lines
lines :: [T.Text]
> 

> lines
["line1","line2","line3","line4"]
> 

-- Convert to a list of strings 
--
> map T.unpack lines
["line1","line2","line3","line4"]
> 
> :t map T.unpack lines
map T.unpack lines :: [String]
> 


-- Split by string (Text)
--
-------------------------------------------

> T.splitOn (T.pack ",") (T.pack "10213.23,-100.23,400.0")
["10213.23","-100.23","400.0"]
> 

> map (\ s -> read (T.unpack s) :: Double) $ T.splitOn (T.pack ",") (T.pack "10213.23,-100.23,400.0")
[10213.23,-100.23,400.0]
> 

--- Removes blank space and newline chars from the beginning of the string.
--- 
---------------------------------------- 

> T.stripStart (T.pack " \n\n \r  hello world haskell\n\r \n \r\n")
"hello world haskell\n\r \n \r\n"
> 

--- Removes blank space from the end of string.
---------------------------------------- 

> T.stripEnd  (T.pack " \n\n \r  hello world haskell\n\r \n \r\n")
" \n\n \r  hello world haskell"
> 

--  Removes blank spaces and new line chars 
--  from left and right.
-------------------------------------------

> :t T.strip
T.strip :: T.Text -> T.Text
> 
> T.strip (T.pack " line \n\n\r\n")
"line"
> 


--
-- Reverse 
--------------------------------------- 

> T.reverse (T.pack "hello world")
"dlrow olleh"
> 


--- Join Strings (Data.Text)
-----------------------------

> :t T.concat
T.concat :: [T.Text] -> T.Text
> 
> T.concat [T.pack "text1", T.pack "text2", T.pack "text 3"]
"text1text2text 3"
> 


-- Predicates                    -------
----------------------------------------


-- Test if string is null 
--
> T.null $ T.pack  "Hello world"
False
> T.null $ T.pack  ""
True
> 


> :t T.isPrefixOf 
T.isPrefixOf :: T.Text -> T.Text -> Bool
> 

> T.isPrefixOf (T.pack "xyz") (T.pack "xyz-hello-world")
True
> 
> T.isPrefixOf (T.pack "xyz") (T.pack "abc-hello-world")
False
> 

> :t T.isSuffixOf 
T.isSuffixOf :: T.Text -> T.Text -> Bool
>

> T.isSuffixOf (T.pack ".pdf") (T.pack "file1.pdf")
True
> 

> :t (T.isSuffixOf (T.pack ".pdf") . T.pack)
(T.isSuffixOf (T.pack ".pdf") . T.pack) :: String -> Bool
>

> filter (T.isSuffixOf (T.pack ".pdf") . T.pack) ["file1.pdf", "dummy.org", "mrdummy.el", "file2.pdf" ]
["file1.pdf","file2.pdf"]
> 

> let isSuffixOf suffix = T.isSuffixOf (T.pack suffix) . T.pack
> 
> :t isSuffixOf 
isSuffixOf :: String -> String -> Bool
> 

> filter (isSuffixOf ".pdf") ["file1.pdf", "dummy.org", "mrdummy.el", "file2.pdf" ]
["file1.pdf","file2.pdf"]


--------------------------------------------------------------------------------------------

--- Take 
------------------

> T.take 3 (T.pack "hello world")
"hel"
> 

> T.take 10 (T.pack "hello world")
"hello worl"
> 

> T.take 100 (T.pack "hello world")
"hello world"
> 

-- Drop 
--------------

> T.drop 3 (T.pack "Hello world")
"lo world"
> 
> T.drop 10 (T.pack "Hello world")
"d"
> T.drop 10 (T.pack "Hello world")
```
