- [System IO Modules](#system-io-modules)
  - [Overview](#overview)
  - [System.IO](#system.io)
    - [Overview](#overview)
    - [Read and Write File](#read-and-write-file)
    - [Explore System.IO](#explore-system.io)
    - [Read three Lines from a file.](#read-three-lines-from-a-file.)
    - [Test the module System.IO](#test-the-module-system.io)
    - [Read all lines of a file](#read-all-lines-of-a-file)
  - [System.Environment](#system.environment)
  - [System.Directory](#system.directory)
  - [System.Info](#system.info)
  - [System.Process](#system.process)

# System IO Modules<a id="sec-1" name="sec-1"></a>

## Overview<a id="sec-1-1" name="sec-1-1"></a>

## System.IO<a id="sec-1-2" name="sec-1-2"></a>

### Overview<a id="sec-1-2-1" name="sec-1-2-1"></a>

**System.IO**

Standard File Handlers: 

-   stdin
-   stdout
-   stderr

IOMode 

-   ReadMode
-   WriteMode
-   AppendMode
-   ReadWriteMode

### Read and Write File<a id="sec-1-2-2" name="sec-1-2-2"></a>

```haskell
> :t putStrLn
putStrLn :: String -> IO ()
> 

-- Bind operator
--
> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
> 

-- Reverse bind operator 
-- 
--
> :t (=<<)
(=<<) :: Monad m => (a -> m b) -> m a -> m b
> 

> :t readFile "/etc/lsb-release" 
readFile "/etc/lsb-release" :: IO String
> 
> readFile "/etc/lsb-release" 
"DISTRIB_ID=Ubuntu\nDISTRIB_RELEASE=15.04 ..."

> readFile "/etc/lsb-release" >>= putStrLn
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=15.04
DISTRIB_CODENAME=vivid
DISTRIB_DESCRIPTION="Ubuntu 15.04"

> putStrLn =<< readFile "/etc/lsb-release" 
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=15.04
DISTRIB_CODENAME=vivid
DISTRIB_DESCRIPTION="Ubuntu 15.04"

-- This code must be loaded from a file:
--
showFile fileName = do 
  content <- readFile fileName 
  putStrLn content 


-- This code can be pasted in the repl.
--
> let showFile3 fileName = do { content <- readFile fileName ; putStrLn content }
>
> :t showFile3
showFile3 :: FilePath -> IO ()

> showFile "/etc/lsb-release"
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=15.04
DISTRIB_CODENAME=vivid
DISTRIB_DESCRIPTION="Ubuntu 15.04"


-- Dessugarized.
--
> let showFile2 fileName = readFile fileName >>= \content -> putStrLn content
> 
> showFile2 "/etc/lsb-release"
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=15.04
DISTRIB_CODENAME=vivid
DISTRIB_DESCRIPTION="Ubuntu 15.04"
```

### Explore System.IO<a id="sec-1-2-3" name="sec-1-2-3"></a>

```haskell
> import qualified System.IO as SIO
> 

-- Type tab to list the module 
--
> SIO.
> 
Display all 106 possibilities? (y or n)
SIO.AbsoluteSeek                              SIO.hPutChar
SIO.AppendMode                                SIO.hPutStr
SIO.BlockBuffering                            SIO.hPutStrLn
SIO.BufferMode                                SIO.hReady
SIO.CRLF                                      SIO.hSeek

> :t SIO.readFile 
SIO.readFile :: FilePath -> IO String
> 

-- File handler / File descriptor (Unix File descriptor)
-- 
> let fd = SIO.openFile "/etc/lsb-release"  SIO.ReadMode

> :t SIO.openFile 
SIO.openFile :: FilePath -> SIO.IOMode -> IO SIO.Handle
> 

> fd
{handle: /etc/lsb-release}
> :t fd
fd :: IO SIO.Handle
> 

-- Return a line from file descritor 
--
> :t SIO.hGetLine
SIO.hGetLine :: SIO.Handle -> IO String
> 

> fd >>= SIO.hGetLine 
"DISTRIB_ID=Ubuntu"
> fd >>= SIO.hGetLine 
"DISTRIB_ID=Ubuntu"
>
```

### Read three Lines from a file.<a id="sec-1-2-4" name="sec-1-2-4"></a>

```haskell
import qualified System.IO as SIO 

readThreeLines fileName = do 
  fd    <- SIO.openFile fileName SIO.ReadMode 
  line1 <- SIO.hGetLine fd     
  line2 <- SIO.hGetLine fd 
  line3 <- SIO.hGetLine fd 
  SIO.hClose fd 
  return [line1, line2, line3]

-- --------------------------------
- ---------------------------------

> :paste

> readThreeLines "/etc/lsb-release"
["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04","DISTRIB_CODENAME=vivid"]
> 
> :t readThreeLines "/etc/lsb-release"
readThreeLines "/etc/lsb-release" :: IO [String]
```

### Test the module System.IO<a id="sec-1-2-5" name="sec-1-2-5"></a>

```haskell
import qualified System.IO as SIO 


> let fd = SIO.openFile "/etc/lsb-release" SIO.ReadMode
> :t fd
fd :: IO SIO.Handle
> 


> fd >>= SIO.hGetContents 
"DISTRIB_ID=Ubuntu\nDISTRIB_RELEASE=15.04\nDISTRIB_CODENAME ... ... "
> 

> fd >>= SIO.hGetContents >>= putStrLn
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=15.04
DISTRIB_CODENAME=vivid
DISTRIB_DESCRIPTION="Ubuntu 15.04"



let readLine fd =  SIO.hGetLine fd >>= \line -> return (line, fd)

> :t readLine 
readLine :: SIO.Handle -> IO (String, SIO.Handle)
> 

> fd >>= readLine 
("DISTRIB_ID=Ubuntu",{handle: /etc/lsb-release})
> 
> fd >>= readLine 
("DISTRIB_ID=Ubuntu",{handle: /etc/lsb-release})
> 

> let readTwoLines fd = readLine fd >>= \(line1, fd) -> readLine fd >>= \(line2, fd) -> return ([line1, line2], fd)

> fd >>= readTwoLines 
(["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04"],{handle: /etc/lsb-release})
>
```

### Read all lines of a file<a id="sec-1-2-6" name="sec-1-2-6"></a>

```haskell
import qualified System.IO as SIO 

fdtest = SIO.openFile "/etc/lsb-release" SIO.ReadMode

readLineState :: SIO.Handle -> IO (String, SIO.Handle)
readLineState fd = do 
  line <- SIO.hGetLine fd 
  return (line, fd)

readLinesAux :: SIO.Handle -> [String] -> IO [String] 
readLinesAux fd lines = do
   (lin, fdnext) <- readLineState fd 
   isEOF         <- SIO.hIsEOF fdnext
   if isEOF 
   then return (reverse lines)
   else readLinesAux fdnext (lin:lines)



readLines :: SIO.Handle -> IO [String]
readLines fd = readLinesAux fd []

readLinesFromFile :: String -> IO [String]
readLinesFromFile fileName = do 
  handle    <- SIO.openFile fileName SIO.ReadMode 
  lines     <- readLines handle 
  SIO.hClose handle 
  return  lines 


makeReader  :: (SIO.Handle ->  IO out) -> (SIO.Handle -> IO (out, SIO.Handle))
makeReader reader fd = do 
  out <- reader fd 
  return (out, fd)
  

applyFileReaderAux :: (SIO.Handle -> IO (out, SIO.Handle)) ->  SIO.Handle -> [out] -> IO [out]
applyFileReaderAux fileReader fd acc = do
   (output, fdnext) <- fileReader fd 
   isEOF            <- SIO.hIsEOF fdnext 
   if isEOF 
   then return (reverse acc)
   else applyFileReaderAux fileReader fdnext (output:acc)

applyFileReader ::  (SIO.Handle -> IO (out, SIO.Handle)) -> SIO.Handle -> IO [out]
applyFileReader fileReader fd =   applyFileReaderAux fileReader fd []
```

Repl tests:

```haskell
> :t fdtest 
fdtest :: IO SIO.Handle
> 

> :t readLineState 
readLineState :: SIO.Handle -> IO (String, SIO.Handle)
> 

> fdtest >>= readLineState 
("DISTRIB_ID=Ubuntu",{handle: /etc/lsb-release})
> 

> readLineState =<< fdtest
("DISTRIB_ID=Ubuntu",{handle: /etc/lsb-release})
> 

> fdtest >>= \fd -> readLinesAux fd []
["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04","DISTRIB_CODENAME=vivid"]
> 
> :t fdtest >>= \fd -> readLinesAux fd []
fdtest >>= \fd -> readLinesAux fd [] :: IO [String]
> 


> fdtest >>= readLines 
["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04","DISTRIB_CODENAME=vivid"]
> 


> readLinesFromFile "/etc/lsb-release"
["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04","DISTRIB_CODENAME=vivid"]
> 

> :t SIO.withFile
SIO.withFile
  :: FilePath -> SIO.IOMode -> (SIO.Handle -> IO r) -> IO r
> 

> SIO.withFile "/etc/lsb-release" SIO.ReadMode readLines
["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04","DISTRIB_CODENAME=vivid"]
>

> :t SIO.withFile "/etc/lsb-release" SIO.ReadMode readLines
SIO.withFile "/etc/lsb-release" SIO.ReadMode readLines
  :: IO [String]


 -- ------------------------
> 
> fd <- SIO.openFile "/etc/lsb-release" SIO.ReadMode
> :t fd
fd :: SIO.Handle
> 
> fd
{handle: /etc/lsb-release}
> 

> :t readLineState 
readLineState :: SIO.Handle -> IO (String, SIO.Handle)

> applyFileReaderAux readLineState fd []
["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04","DISTRIB_CODENAME=vivid"]
> 

> let charReader = makeReader SIO.hGetChar 

> :t charReader 
charReader :: SIO.Handle -> IO (Char, SIO.Handle)
> 

> applyFileReader charReader fd
"DISTRIB_ID=Ubuntu\nDISTRIB_RELEASE=15.04\nDISTRIB_CODENAME= ..."
> 


> fd <- SIO.openFile "/etc/lsb-release" SIO.ReadMode
> applyFileReader (makeReader SIO.hGetLine) fd
["DISTRIB_ID=Ubuntu","DISTRIB_RELEASE=15.04","DISTRIB_CODENAME=vivid"]
> 
> applyFileReader (makeReader SIO.hGetLine) fd
 *** Exception: /etc/lsb-release: hGetLine: end of file
>
```

## System.Environment<a id="sec-1-3" name="sec-1-3"></a>

[Module Documentation](https://hackage.haskell.org/package/base-4.8.2.0/docs/System-Environment.html) 

Miscellaneous information about the system environment.

```haskell
> import qualified System.Environment as E
> 

--  returns a list of the program's command line arguments 
--  (not including the program name).
-- 

> E.getArgs 
[]

> :t E.getArgs 
E.getArgs :: IO [String]
> 

-- Computation getProgName returns the name of the program as it was invoked.
--

> :t E.getProgName 
E.getProgName :: IO String
> 
> E.getProgName 
"<interactive>"
> 


-- Returns the absolute pathname of the current executable.
-- Note that for scripts and interactive sessions, this is 
-- the path to the interpreter (e.g. ghci.)


> :t E.getExecutablePath 
E.getExecutablePath :: IO FilePath
> 

> E.getExecutablePath 
"/usr/lib/ghc/lib/ghc"
> 

> path <- E.getExecutablePath 
> :t path
path :: FilePath
> path
"/usr/lib/ghc/lib/ghc"
> 

-- Computation getEnv var returns the value of the environment 
-- variable var. For the inverse, POSIX users can use putEnv.
--
-- 

> :t E.getEnv
E.getEnv :: String -> IO String
> 

> E.getEnv "HOME"
"/home/tux"
> 

> E.getEnv "PATH"
"/home/tux/.cask/bin:/opt/bin:/home/tux/.opam/4.02.1/bin:/home/tux/bin ..."

> E.getEnv "DONTEXISTS"
 *** Exception: DONTEXISTS: getEnv: does not exist (no environment variable)
> 

--  Return the value of the environment variable var, 
--  or Nothing if there is no such value.
--

> :t E.lookupEnv 
E.lookupEnv :: String -> IO (Maybe String)
> 
> E.lookupEnv "JDK_HOME"
Just "/opt/java"
> 

> E.lookupEnv "JDK_HOMEasdasd"
Nothing
> 


-- getEnvironment retrieves the entire environment as a list of (key,value) pairs.

> :t E.getEnvironment 
E.getEnvironment :: IO [(String, String)]
> 


> E.getEnvironment >>= \lst -> return (take 10 lst)
[("USER","tux"),("LANGUAGE","en_US"),("LC_TIME","pt_BR.UTF-8"),("COMP_WORDBREAKS"," \t\n\"'><;|&(:"),("XDG_SEAT","seat0"),("SSH_AGENT_PID","15614"),("XDG_SESSION_TYPE","x11"),("SHLVL","1"),("HOME","/home/tux"),("QT4_IM_MODULE","xim")]
> 

> :t mapM_
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
> 

> :t E.getEnvironment >>= \rows ->  mapM_ (\row -> putStrLn (show row)) rows
E.getEnvironment >>= \rows ->  mapM_ (\row -> putStrLn (show row)) rows
  :: IO ()
> 

> E.getEnvironment >>= \rows ->  mapM_ (\row -> putStrLn (show row)) rows

("USER","tux")
("LANGUAGE","en_US")
("LC_TIME","pt_BR.UTF-8")
("XDG_SEAT","seat0")
("SSH_AGENT_PID","15614")
("XDG_SESSION_TYPE","x11")
("SHLVL","1")
...
```

## System.Directory<a id="sec-1-4" name="sec-1-4"></a>

-   [Module Documentation](https://hackage.haskell.org/package/directory-1.2.5.1/docs/System-Directory.html)

```haskell
> import qualified System.Directory as D
> 


-- createDirectory dir creates a new directory dir which is initially
-- empty, or as near to empty as the operating system allows.


> D.createDirectory "/tmp/testdir"
> D.createDirectory "/tmp/testdir"
 *** Exception: /tmp/testdir: createDirectory: already exists (File exists)
> 

{-
createDirectoryIfMissing Source
  :: Bool	 Create its parents too?
  -> FilePath	 The path to the directory you want to make
  -> IO ()

-}


> D.createDirectoryIfMissing True "/tmp/tree1/1/2/3/4"
> 
> D.createDirectoryIfMissing True "/tmp/tree1/1/2/3/4"
> 
> D.createDirectoryIfMissing True "/tree1/1/2/3/4"
 *** Exception: /tree1: createDirectory: permission denied (Permission denied)
> 


-- removeDirectory dir removes an existing directory dir. 

> D.removeDirectory "/tmp/testdir"
> D.removeDirectory "/tmp/testdir"
 *** Exception: /tmp/testdir: removeDirectory: does not exist (No such file or directory)
> 

--  Similar to listDirectory, but always includes the special entries
--  (. and ..). (This applies to Windows as well.)

> :t D.getDirectoryContents "/boot" 
D.getDirectoryContents "/boot" :: IO [FilePath]
> 


> fmap (take 2) (D.getDirectoryContents "/boot" )
["initrd.img-3.19.0-37-generic","config-3.19.0-21-generic"]
> 

> take 2 <$> D.getDirectoryContents "/boot" 
["initrd.img-3.19.0-37-generic","config-3.19.0-21-generic"]
> 

> D.getDirectoryContents "/boot" >>= mapM_ putStrLn
initrd.img-3.19.0-37-generic
config-3.19.0-21-generic
System.map-3.19.0-39-generic
abi-3.19.0-18-generic
...

> D.getDirectoryContents "/boot" >>= \rows -> return (take 10 rows) >>= mapM_ putStrLn
initrd.img-3.19.0-37-generic
config-3.19.0-21-generic
System.map-3.19.0-39-generic
... 
> 

> let displayDir path = D.getDirectoryContents path >>= \rows -> return (take 10 rows) >>= mapM_ putStrLn
> 
> displayDir "/boot"
initrd.img-3.19.0-37-generic
config-3.19.0-21-generic
System.map-3.19.0-39-generic
...


--  Obtain the current working directory as an absolute path.
--

> :t D.getCurrentDirectory 
D.getCurrentDirectory :: IO FilePath
> 
> D.getCurrentDirectory 
"/home/tux"
> 


-- Change the working directory to the given path.
--

> :t D.setCurrentDirectory 
D.setCurrentDirectory :: FilePath -> IO ()
> 

--  Returns the current user's home direct
-- 

> D.getHomeDirectory 
"/home/tux"
> 

> D.getUserDocumentsDirectory 
"/home/tux"
> 
> D.getTemporaryDirectory 
"/tmp"
> 


--  copyFile old new copies the existing file from old to new. If the
-- new file already exists, it is atomically replaced by the old
--  file. Neither path may refer to an existing directory. The
--  permissions of old are copied to new, if possible.  

> :t D.copyFile
D.copyFile :: FilePath -> FilePath -> IO () 
> 
> 
> D.copyFile "/etc/lsb-release" "/tmp/test.txt" 
>
> readFile "/tmp/test.txt" >>= putStrLn
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=15.04
DISTRIB_CODENAME=vivid
DISTRIB_DESCRIPTION="Ubuntu 15.04"

{-
 removeFile file removes the directory entry for an existing file file,
 where file is not itself a directory. The implementation may specify
 additional constraints which must be satisfied before a file can be
 removed (e.g. the file may not be in use by other processes).

-}

> :t D.removeFile
D.removeFile :: FilePath -> IO ()
> 

> D.removeFile "/tmp/test.txt"
> D.removeFile "/tmp/test.txt"
 *** Exception: /tmp/test.txt: removeLink: does not exist (No such file or directory)
> 

{-
 Given an executable file name, searches for such file in the
 directories listed in system PATH. The returned value is the path to
 the found executable or Nothing if an executable with the given name
 was not found. For example (findExecutable "ghc") gives you the path
 to GHC.

-}

> D.findExecutable "java"
Just "/opt/bin/java"
> 
> D.findExecutable "javac"
Just "/opt/java/bin/javac"
> D.findExecutable "javaasdas"
Nothing
> 

{- 

Given a file name, searches for the file and returns a list of all
occurences that are executable.

-}

> D.findExecutables "java"
["/opt/bin/java","/opt/bin/java","/usr/bin/java","/opt/java/bin/java"]
> 

> D.findExecutables "ghci"
["/usr/bin/ghci"]
>

{- 

Given a file name, searches for the file on the given paths and
returns a list of all occurences that are executable.
-}

> :t D.findFile
D.findFile :: [FilePath] -> String -> IO (Maybe FilePath)
> 

> D.findFiles ["/opt/bin", "/usr/bin"]  "java"
["/opt/bin/java","/usr/bin/java"]
> 

> D.findFiles ["/opt/bin", "/usr/bin"]  "jasadva"
[]

-- The operation doesFileExist returns True if the argument file
-- exists and is not a directory, and False otherwise.

> D.doesFileExist "/etc/fstab"
True
> 
> D.doesFileExist "/etc/fstaASDb"
False
> 

{-
The operation doesDirectoryExist returns True if the argument file
exists and is either a directory or a symbolic link to a directory,
and False otherwise.

-}

> 
> D.doesDirectoryExist "/etc"
True
> D.doesDirectoryExist "/etc/Dummy"
False
> 

-- The getPermissions operation returns the permissions for the file or directory.
--

> D.getPermissions "/etc/fstab"
Permissions {readable = True, writable = False, executable = False, searchable = False}
>
```

## System.Info<a id="sec-1-5" name="sec-1-5"></a>

-   [Module Documentation](https://hackage.haskell.org/package/base-4.8.2.0/docs/System-Info.html)

Information about the characteristics of the host system lucky enough
to run your program.

```haskell
> import qualified System.Info as I
> 
> I.os
"linux"
> 
> I.arch
"i386"
> 
> I.compilerName 
"ghc"
> 
> I.compilerVersion 
Version {versionBranch = [7,6], versionTags = []}
> 
> :t I.compilerVersion 
I.compilerVersion :: Data.Version.Version
>
```

## System.Process<a id="sec-1-6" name="sec-1-6"></a>

-   [Module Documentation](https://hackage.haskell.org/package/process-1.4.2.0/docs/System-Process.html)

```haskell
> import qualified System.Process as P
> 


{-
Creates a new process to run the specified command with the given
arguments, and wait for it to finish. If the command returns a
non-zero exit code, an exception is raised.

-}

> P.callProcess "date" []
Ter Fev 23 12:29:42 BRT 2016
> :t P.callProcess "date" []
P.callProcess "date" [] :: IO ()
> 

> P.callProcess "dadsfte" []
 *** Exception: dadsfte: callProcess: runInteractiveProcess: exec: does not exist (No such file or directory)
> 

> P.callProcess "uname" ["-a"]
Linux tuxhorse 3.19.0-39-generic #44-Ubuntu SMP Tue Dec 1 14:38:23 UTC 2015 i686 i686 i686 GNU/Linux
>

{-
Creates a new process to run the specified shell command. If the
command returns a non-zero exit code, an exception is raised.
-}

> P.callCommand "uname -a"
Linux tuxhorse 3.19.0-39-generic #44-Ubuntu SMP Tue Dec 1 14:38:23 UTC 2015 i686 i686 i686 GNU/Linux
> 

> :t P.callCommand 
P.callCommand :: String -> IO ()
> 

{-  
readProcess forks an external process, reads its standard output
strictly, blocking until the process terminates, and returns the
output string. The external process inherits the standard error.

readProcess Source
  :: FilePath	 Filename of the executable (see RawCommand for details)
  -> [String]	 any arguments
  -> String	     standard input
  -> IO String	 stdout

-}

> :t P.readProcess
P.readProcess :: FilePath -> [String] -> String -> IO String
> 

> P.readProcess "uname" ["-r"] ""
"3.19.0-39-generic\n"
> 

> kernelVersion <- P.readProcess "uname" ["-r"] ""
> kernelVersion 
"3.19.0-39-generic\n"
> :t kernelVersion 
kernelVersion :: String
> 

{-
readProcessWithExitCode Source
   :: FilePath	                     Filename of the executable (see RawCommand for details)
   -> [String]	                     any arguments
   -> String	                     standard input
   -> IO (ExitCode, String, String)	 exitcode, stdout, stderr
-}

> 
> P.readProcessWithExitCode "uname" ["-a"] ""
(ExitSuccess,"Linux tuxhorse 3.19.0-39-generic #44-Ubuntu SMP Tue Dec 1 14:38:23 UTC 2015 i686 i686 i686 GNU/Linux\n","")
> 
> P.readProcessWithExitCode "uname" ["-x"] ""
(ExitFailure 1,"","uname: invalid option -- 'x'\nTry 'uname --help' for more information.\n")
> 

> P.readProcessWithExitCode "unasdame" ["-x"] ""
 *** Exception: unasdame: readCreateProcessWithExitCode: runInteractiveProcess: exec: does not exist (No such file or directory)
>
```
