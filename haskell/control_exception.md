- [Control.Exception](#control.exception)


# Control.Exception<a id="sec-1" name="sec-1"></a>

[Module Documentation](https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Exception.html)

```haskell
> import Control.Exception 
> import System.Directory as D
> 

> try (D.removeFile  "/tmp/test.dummy") :: IO (Either IOException ())
Left /tmp/test.dummy: removeLink: does not exist (No such file or directory)
> 

> :t try
try :: Exception e => IO a -> IO (Either e a)
>

> let tryIO ioact = try ioact :: IO (Either IOException ())
> :t tryIO
tryIO :: IO () -> IO (Either IOException ())
> 

> :t catch
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

Example 2: exception1.hs

```haskell
import Control.Exception 
import System.Directory as D

tryIO :: IO a ->  IO (Either IOException a)
tryIO ioact = try ioact 

-- :: IO (Either IOException a)

tryReadFile :: String ->  IO (Either IOException String)
tryReadFile filename = tryIO $ readFile filename 

tryShowFile fileName =  do
  result <- try (readFile fileName) :: IO (Either IOException String)
  case result of
    Right content -> putStrLn content
    Left  except  -> putStrLn $ "Error: " ++ show except 

tryRemoveFile fileName = do 
  result <- tryIO (D.removeFile fileName)
  case result of  
    Right ()    -> putStrLn "OK"
    Left except -> putStrLn $ "Error: " ++ show except 


{-  catch :: Exception e => IO a -> (e -> IO a) -> IO a -}    
tryReadFile2 :: String -> IO String 
tryReadFile2 fileName =
  catch (readFile fileName) errorHandler
  where
    errorHandler :: IOException -> IO String
    errorHandler error = return $ show error 


readFileSafe :: String -> IO (Either IOException String) 
readFileSafe filePath = 
   catch  (fmap Right (readFile filePath))  (\e -> return (Left e))


fileChars :: FilePath -> IO (Either IOException Int)
fileChars filePath = fmap (fmap length ) (readFileSafe filePath)

fileChars2 = fmap (fmap length) . readFileSafe 

safePrintFile :: String -> IO (Either IOException ())
safePrintFile f = readFileSafe f >>= 
                  \result -> case result of 
                   Right content ->  do 
                                       putStrLn content 
                                       return $ Right ()
                   Left  e       ->  return (Left e)
```

```haskell
> :load /tmp/exception1.hs

> tryShowFile "/etc/issue.net"
Ubuntu 15.04

> tryShowFile "/etc/issue.netasda"
Error: /etc/issue.netasda: openFile: does not exist (No such file or directory)
> 

> :t tryRemoveFile 
tryRemoveFile :: FilePath -> IO ()
> 

> :t tryShowFile 
tryShowFile :: FilePath -> IO ()
> 


> tryRemoveFile "/etc/lsb-release"
Error: /etc/lsb-release: removeLink: permission denied (Permission denied)
> 

> tryRemoveFile "/tmp/dummy"
Error: /tmp/dummy: removeLink: does not exist (No such file or directory)
> 

> let touch fname = writeFile fname ""
> 
> touch "/tmp/test1"
> 
> tryRemoveFile "/tmp/test1"
OK
> tryRemoveFile "/tmp/test1"
Error: /tmp/test1: removeLink: does not exist (No such file or directory)
> 


> :t tryReadFile 
tryReadFile :: FilePath -> IO (Either IOException String)
> 

> tryReadFile "/etc/issue.net"
Right "Ubuntu 15.04\n"
> 

> tryReadFile "/etc/issue.nasdet"
Left /etc/issue.nasdet: openFile: does not exist (No such file or directory)
>

> tryReadFile2 "/etc/issue.net"
"Ubuntu 15.04\n"
> 
> tryReadFile2 "/etc/issue.netasd"
"/etc/issue.netasd: openFile: does not exist (No such file or directory)"
> 

> :t catch
catch :: Exception e => IO a -> (e -> IO a) -> IO a
> 

> :t readFileSafe 
readFileSafe :: String -> IO (Either IOException String)
> 

> readFileSafe "/etc/issue.net"
Right "Ubuntu 15.04\n"
> 

> readFileSafe "/etc/issue.wrong"
Left /etc/issue.wrong: openFile: does not exist (No such file or directory)
> 

> fileChars :: String -> IO (Either IOException Int)
> 

> fileChars "/etc/issue.net"
Right 13
> 


> fileChars "/etc/issue.wrong"
Left /etc/issue.wrong: openFile: does not exist (No such file or directory)
> 


> :t fileChars2
fileChars2 :: String -> IO (Either IOException Int)
> 

> fileChars2 "/etc/issue.net"
Right 13
> 

> safePrintFile "/etc/issue.net"
Ubuntu 15.04

Right ()
> 

> safePrintFile "/etc/issasdsnet"
Left /etc/issasdsnet: openFile: does not exist (No such file or directory)
>
```
