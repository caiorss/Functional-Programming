
-- file: ch13/passwdmap.hs
import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf(printf)
import System.Environment(getArgs)
import System.Exit
import Control.Monad(when)


{- | The primary piece of data this program will store.
   It represents the fields in a POSIX /etc/passwd file -}
data PasswdEntry = PasswdEntry {
    userName :: String,
    password :: String,
    uid :: Integer,
    gid :: Integer,
    gecos :: String,
    homeDir :: String,
    shell :: String}
    deriving (Eq, Ord)

{- | Define how we get data to a 'PasswdEntry'. -}
instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s" 
                (userName pe) (password pe) (uid pe) (gid pe)
                (gecos pe) (homeDir pe) (shell pe)

{- | Converting data back out of a 'PasswdEntry'. -}
instance Read PasswdEntry where
    readsPrec _ value =
        case split ':' value of
             [f1, f2, f3, f4, f5, f6, f7] ->
                 -- Generate a 'PasswdEntry' the shorthand way:
                 -- using the positional fields.  We use 'read' to convert
                 -- the numeric fields to Integers.
                 [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
             x -> error $ "Invalid number of fields in input: " ++ show x
        where 
        {- | Takes a delimiter and a list.  Break up the list based on the
        -  delimiter. -}
        split :: Eq a => a -> [a] -> [[a]]
        -- If the input is empty, the result is a list of empty lists.
        split _ [] = [[]]
        split delim str =
            let -- Find the part of the list before delim and put it in
                -- "before".  The rest of the list, including the leading 
                -- delim, goes in "remainder".
                (before, remainder) = span (/= delim) str
                in
                before : case remainder of
                              [] -> []
                              x -> -- If there is more data to process,
                                   -- call split recursively to process it
                                   split delim (tail x)


-- Convenience aliases; we'll have two maps: one from UID to entries
-- and the other from username to entries
type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

{- | Converts input data to maps.  Returns UID and User maps. -}
inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp =
    (uidmap, usermap)
    where
    -- fromList converts a [(key, value)] list into a Map
    uidmap = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
    usermap = Map.fromList . 
              map (\pe -> (userName pe, pe)) $ entries
    -- Convert the input String to [PasswdEntry]
    entries = map read (lines inp)


main = do
    -- Load the command-line arguments
    args <- getArgs
    -- If we don't have the right number of args,
    -- give an error and abort
    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure
    -- Read the file lazily
    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps

mainMenu maps@(uidmap, usermap) = do
    putStr optionText
    hFlush stdout
    sel <- getLine
    -- See what they want to do.  For every option except 4,
    -- return them to the main menu afterwards by calling
    -- mainMenu recursively
    case sel of
         "1" -> lookupUserName >> mainMenu maps
         "2" -> lookupUID >> mainMenu maps
         "3" -> displayFile >> mainMenu maps
         "4" -> return ()
         _ -> putStrLn "Invalid selection" >> mainMenu maps
    where 
    lookupUserName = do
        putStrLn "Username: "
        username <- getLine
        case Map.lookup username usermap of
             Nothing -> putStrLn "Not found."
             Just x -> print x
   
    lookupUID = do
        putStrLn "UID: "
        uidstring <- getLine
        case Map.lookup (read uidstring) uidmap of
             Nothing -> putStrLn "Not found."
             Just x -> print x

    displayFile = 
        putStr . unlines . map (show . snd) . Map.toList $ uidmap

    optionText = 
          "\npasswdmap options:\n\
           \\n\
           \1   Look up a user name\n\
           \2   Look up a UID\n\
           \3   Display entire file\n\
           \4   Quit\n\n\
           \Your selection: "
