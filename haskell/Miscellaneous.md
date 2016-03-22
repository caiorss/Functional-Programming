- [Miscellaneous](#miscellaneous)
  - [Haskell IDEs and Text Editors](#haskell-ides-and-text-editors)
  - [GHCI configuration file](#ghci-configuration-file)
  - [Troubleshooting](#troubleshooting)
    - [Importing Ambigous Modules in GHCi](#importing-ambigous-modules-in-ghci)


# Miscellaneous<a id="sec-1" name="sec-1"></a>

## Haskell IDEs and Text Editors<a id="sec-1-1" name="sec-1-1"></a>

## GHCI configuration file<a id="sec-1-2" name="sec-1-2"></a>

The ghci configuration file allows the user to create custom commands and 
customize the ghci shell. The file is in the directory ~/.ghci in Unix systems 
like Linux and OSX.

Example: ~/.ghci

```
import System.Directory ( getCurrentDirectory)
import System.Process (readProcess)

:set -hide-package mtl

:set prompt "\x1b[32m>\x1b[0m " -- set the shell prompt to print λ (lambda)

:def hoogle \s -> return $ ":! hoogle --count=15 \"" ++ s ++ "\""

let xclip_ =  readProcess "xclip" ["-o", "-selection", "clipboard"] ""

let __savePasted = do {  code <-  readProcess "xclip" ["-o", "-selection", "clipboard"] ""  ;  putStrLn 

code ; writeFile "/tmp/haskTemp.hs" code}

let pwd = getCurrentDirectory >>= putStrLn

:def paste (\_ -> __savePasted >> return ":load /tmp/haskTemp.hs" )
:def pwd (\_ -> pwd >> return "")
```

It requires hoogle to be installed on Linux: (sudo apt-get install
hoogle) on Ubuntu.

Examples:

```
:paste - Paste code block in clipboard and compiles it in ghci
:pwd   - Show Current Directory
```

```haskell
> :pwd
/home/tux/PycharmProjects/Haskell
> 
> 

> :hoogle getCurrentDirectory
Directory getCurrentDirectory :: IO FilePath
System.Directory getCurrentDirectory :: IO FilePath
> 


> :paste
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    let (year, month, day) = toGregorian $ localDay zoneNow
    putStrLn $ "Year: " ++ show year
    putStrLn $ "Month: " ++ show month
    putStrLn $ "Day: " ++ show day
[1 of 1] Compiling Main             ( /tmp/haskTemp.hs, interpreted )
Ok, modules loaded: Main.
> 
> main
Year: 2015
Month: 4
Day: 26
>
```

## Troubleshooting<a id="sec-1-3" name="sec-1-3"></a>

### Importing Ambigous Modules in GHCi<a id="sec-1-3-1" name="sec-1-3-1"></a>

```haskell
> import Control.Monad.State

<no location info>:
    Ambiguous module name `Control.Monad.State':
      it was found in multiple packages: monads-tf-0.1.0.2 mtl-2.2.1
> 

> :set -hide-package mtl
> import Control.Monad.State
>
```

Solution: Add the line to ~/.ghci

```
:set -hide-package mtl
```
