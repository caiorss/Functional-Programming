  - [Libraries](#libraries)
    - [System Programming in Haskell](#system-programming-in-haskell)
    - [Data.Time](#data.time)
    - [Bytestring](#bytestring)
    - [Parse Binary Files](#parse-binary-files)
- [](#)
    - [GnuPlot](#gnuplot)

**Table of Contents**  **generated with [DocToc](https://github.com/thlorenz/doctoc)**

## Libraries<a id="sec-0-1" name="sec-0-1"></a>

### System Programming in Haskell<a id="sec-0-1-1" name="sec-0-1-1"></a>

**Directory**

Get current Directory

```
> import System.Directory
> 
> getCurrentDirectory 
"/home/tux/PycharmProjects/Haskell"
>
```

Change Current Directory

```haskell
> import System.Directory
> 
> setCurrentDirectory "/"
> 
> getCurrentDirectory 
"/"
> 
getCurrentDirectory :: IO FilePath
> 
> 
> fmap (=="/") getCurrentDirectory 
True
> 
> liftM (=="/") getCurrentDirectory 
True
>
```

List Directory Contents

```haskell
> import System.Directory
>
>  getDirectoryContents "/usr"
[".","include","src","local","bin","games","share","sbin","lib",".."]
> 
> :t getDirectoryContents 
getDirectoryContents :: FilePath -> IO [FilePath]
>
```

Special Directories Location

```haskell
> getHomeDirectory
"/home/tux"
> 
> getAppUserDataDirectory "myApp"
"/home/tux/.myApp"
> 
> getUserDocumentsDirectory
"/home/tux"
>
```

**Running External Commands**

```haskell
> import System.Cmd
> 
> :t rawSystem
rawSystem :: String -> [String] -> IO GHC.IO.Exception.ExitCode
> 
> rawSystem "ls" ["-l", "/usr"]
total 260
drwxr-xr-x   2 root root 118784 Abr 26 03:38 bin
drwxr-xr-x   2 root root   4096 Abr 10  2014 games
drwxr-xr-x 131 root root  36864 Mar 11 01:38 include
drwxr-xr-x 261 root root  53248 Abr 14 16:46 lib
drwxr-xr-x  10 root root   4096 Dez  2 18:55 local
drwxr-xr-x   2 root root  12288 Abr  3 13:28 sbin
drwxr-xr-x 460 root root  20480 Abr 26 03:38 share
drwxr-xr-x  13 root root   4096 Jan 13 21:03 src
ExitSuccess
>
```

**Reading input from a system command in Haskell**

This command executes ls -la and gets the output.

```haskell
import System.Process
test = readProcess "ls" ["-a"] ""


> import System.Process
> let test = readProcess "ls" ["-a"] ""
> 
> :t test
test :: IO String
> 
> test >>= putStrLn 
.
..
adt2.py
adt.py
build.sh
build_zeromq.sh
clean.sh
codes
comparison.ods
dict.sh
ffi
figure1.png
.git
```

### Data.Time<a id="sec-0-1-2" name="sec-0-1-2"></a>

1.  System

2.  Get current year / month / day in Haskell

    UTC time:
    
    Note that the UTC time might differ from your local time depending on the timezone.
    
    ```haskell
    import Data.Time.Clock
    import Data.Time.Calendar
    
    main = do
        now <- getCurrentTime
        let (year, month, day) = toGregorian $ utctDay now
        putStrLn $ "Year: " ++ show year
        putStrLn $ "Month: " ++ show month
        putStrLn $ "Day: " ++ show day
    ```

3.  Get Current Time

    Local time:
    
    It is also possible to get your current local time using your system’s default timezone:
    
    ```haskell
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
    ```
    
    ```haskell
    import Data.Time.Clock
    import Data.Time.LocalTime
    
    main = do
        now <- getCurrentTime
        timezone <- getCurrentTimeZone
        let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
        putStrLn $ "Hour: " ++ show hour
        putStrLn $ "Minute: " ++ show minute
        -- Note: Second is of type @Pico@: It contains a fractional part.
        -- Use @fromIntegral@ to convert it to a plain integer.
        putStrLn $ "Second: " ++ show second
    ```

4.  Documentation By Examples - GHCI shell

    1.  Date Time Manipulation
    
        ```haskell
        import Data.Time
        
        > :t getCurrentTime
        getCurrentTime :: IO UTCTime
        > 
        
        
        > t <- getCurrentTime
        > t
        2015-03-04 23:22:39.046752 UTC
        > 
        
        > :t t
        t :: UTCTime
        
        
        > today <- fmap utctDay getCurrentTime 
        > today
        2015-03-04
        > :t today
        today :: Day
        > 
        > 
        
        >  let (year, _, _) = toGregorian today
        > year
        2015
        > 
        
        > :t fromGregorian 2015 0 0
        fromGregorian 2015 0 0 :: Day
        
        > fromGregorian 2015 0 0
        2015-01-01
        > 
        
        > diffDays today (fromGregorian year 0 0)
        62
        > 
        
        > import Text.Printf
        >
        > tm <- getCurrentTime
        >  let (year, month, day) = toGregorian (utctDay tm)
        > year
        2015
        > month
        3
        > day
        4
        > 
        
        > printf "The current date is %04d %02d %02d\n" year month day
        The current date is 2015 03 04
        
        
        > import System.Locale
        > 
        > fmap (formatTime defaultTimeLocale "%Y-%m-%d") getCurrentTime
        "2015-03-04"
        > 
        >
        ```
    
    2.  Difference between two dates
    
        ```haskell
        > import Data.Time
        > import Data.Time.Clock.POSIX
        > 
        
        > let bree = UTCTime (fromGregorian 1981 6 16) (timeOfDayToTime $ TimeOfDay 4 35 25) -- 1981-06-16 04:35:25 UTC
        > bree
        1981-06-16 04:35:25 UTC
        > 
        
        
        > let nat  = UTCTime (fromGregorian 1973 1 18) (timeOfDayToTime $ TimeOfDay 3 45 50) -- 1973-01-18 03:45:50 UTC
        > nat
        1973-01-18 03:45:50 UTC
        > 
        
        
        > 
        > let bree' = read "1981-06-16 04:35:25" :: UTCTime
        > bree'
        1981-06-16 04:35:25 UTC
        > :t bree'
        bree' :: UTCTime
        > 
        > let nat'  = read "1973-01-18 03:45:50" :: UTCTime
        > 
        > nat'
        1973-01-18 03:45:50 UTC
        > 
        
        
        > difference = diffUTCTime bree nat / posixDayLength
        > difference 
        3071.03443287037s
        > 
        
        >  "There were " ++ (show $ round difference) ++ " days between Nat and Bree"
        "There were 3071 days between Nat and Bree"
        >
        ```
    
    3.  Day in a Week/Month/Year or Week Number
    
        ```haskell
        > import Data.Time
        > import Data.Time.Calendar.MonthDay
        > import Data.Time.Calendar.OrdinalDate
        > import System.Locale
        
        > :t fromGregorian
        fromGregorian :: Integer -> Int -> Int -> Day
        
        > let (year, month, day) = (1981, 6, 16) :: (Integer , Int , Int )
        > 
        > let date = (fromGregorian year month day)
        > date
        1981-06-16
        > 
        > let (week, week_day) = sundayStartWeek date
        > week
        24
        > week_day
        2
        > 
        
        > let (year_, year_day) = toOrdinalDate date
        > year_
        1981
        > year_day
        167
        > 
        
        
        > let (week_day_name, _) = wDays defaultTimeLocale !! week_day
        > week_day_name
        "Tuesday"
        > 
        
        > :t defaultTimeLocale 
        defaultTimeLocale :: TimeLocale
        > 
        > defaultTimeLocale 
        TimeLocale {wDays = [("Sunday","Sun"),("Monday","Mon"),("Tuesday","Tue"),("Wednesday","Wed"),("Thursday","Thu"),("Friday","Fri"),("Saturday","Sat")], months = [("January","Jan"),("February","Feb"),("March","Mar"),("April","Apr"),("May","May"),("June","Jun"),("July","Jul"),("August","Aug"),("September","Sep"),("October","Oct"),("November","Nov"),("December","Dec")], intervals = [("year","years"),("month","months"),("day","days"),("hour","hours"),("min","mins"),("sec","secs"),("usec","usecs")], amPm = ("AM","PM"), dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y", dateFmt = "%m/%d/%y", timeFmt = "%H:%M:%S", time12Fmt = "%I:%M:%S %p"}
        > 
        >
        ```
    
    4.  Parsing Dates and Times from Strings
    
        ```haskell
        > import Data.Time
        > import Data.Time.Format
        > import Data.Time.Clock.POSIX
        > import System.Locale
        
        > let day:: Day ; day = readTime defaultTimeLocale "%F" "1998-06-03"
        > 
        > day
        1998-06-03
        >
        ```
    
    5.  Printing a Date
    
        ```haskell
        > import Data.Time
        > import Data.Time.Format
        > import System.Locale
        > 
        > now <- getCurrentTime
        > :t now
        now :: UTCTime
        > 
        > formatTime defaultTimeLocale "The date is %A (%a) %d/%m/%Y" now
        "The date is Wednesday (Wed) 04/03/2015"
        > 
        > 
        
        > let t = do now <- getCurrentTime ; return $ formatTime defaultTimeLocale "The date is %A (%a) %d/%m/%Y" now
        > t
        "The date is Wednesday (Wed) 04/03/2015"
        >
        ```
        
        Credits: 
        
        -   <http://techoverflow.net/blog/2014/06/13/get-current-year-month-day-in-haskell/>
        -   <http://techoverflow.net/blog/2014/08/24/get-current-hour-minute-second-in-haskell/>
        -   <http://pleac.sourceforge.net/pleac_haskell/datesandtimes.html>

### Bytestring<a id="sec-0-1-3" name="sec-0-1-3"></a>

1.  Overview

    Bytestring is a built in library for fast and efficient string and binary data processing. ByteStrings are not designed for Unicode. For Unicode strings the Text type must be used from the text package. 
    
    There are two flavours of bytestring, the lazy and strict.
    
    
    -   Strict Bytestring: Data.ByteString
    
    String as a single large array, suitable for passing data between C and Haskell. 
    
    -   Lazy   Bytestring: Data.ByteString.Lazy
    
    Lazy list of strict chunks which makes it suitable for I/O streaming tasks
    
    **Import Bytestring**
    
    To avoid name conflicts the modules must be imported as qualified.
    
    ```haskell
    import qualified Data.ByteString as B
    import qualified Data.ByteString.Char8 as BC
    
    import qualified Data.ByteString.Lazy as BL
    import qualified Data.ByteString.Lazy.Char8 as BLC
    ```

2.  Example

    ```haskell
        > import qualified Data.ByteString as B
        > import qualified Data.ByteString.Char8 as BC
        > 
        > import qualified Data.ByteString.Lazy as BL
        > import qualified Data.ByteString.Lazy.Char8 as BLC
        > 
        > let (|>) x f = f x
        > 
    
    
    {- String to Bytestring -}
    {---------------------------------------------}
    
        > BC.pack "hello world" 
        "hello world"
    
        > BC.pack "čušpajž日本語"
        "\ruapaj~\229,\158"
        > 
        > :t BC.pack "čušpajž日本語"
        BC.pack "čušpajž日本語" :: B.ByteString
        > 
    
    {- Strict to Lazy Conversion -}
    {---------------------------------------------}
    
        > let s_strict = BC.pack  "čušpajž日本語"
        > s_strict 
        "\ruapaj~\229,\158"
        > 
    
        > let s_lazy = BLC.pack  "čušpajž日本語" 
        > s_lazy 
        "\ruapaj~\229,\158"
        > 
        > :t s_
        s_lazy    s_strict
        > :t s_lazy 
        s_lazy :: BL.ByteString
        > 
    
        {- Strict to Lazy -}
           
        > :t BL.fromChunks
        BL.fromChunks :: [B.ByteString] -> BL.ByteString
        > 
    
        > let new_lazy = BL.fromChunks [s_strict]
        > new_lazy 
        "\ruapaj~\229,\158"
        > :t new_lazy
        new_lazy :: BL.ByteString
        > 
    
    
    {- Lazy to Strict -}
    {---------------------------------------------}
    
        > let new_strict = B.concat $ BL.toChunks s_lazy 
        > new_strict 
        "\ruapaj~\229,\158"
        > :t new_strict
        new_strict :: B.ByteString
        > 
    
        > BC.putStrLn new_strict 
        uapaj~�,�
        > 
    
        > BLC.putStrLn new_lazy 
        uapaj~�,�
        > 
    
    
    {- Serialization -}
    {---------------------------------------------}
    
        > import qualified Data.Binary as D
        > {- Serializes Haskell Data to Lazy Bytestring -}
        > 
    
        > :t D.encode
        D.encode :: D.Binary a => a -> BL.ByteString
        > 
    
        > :t D.decode
        D.decode :: D.Binary a => BL.ByteString -> a
        >
    
    
        > let a = D.encode 10 
        > a
        "\NUL\NUL\NUL\NUL\n"
        > :t a
        a :: BL.ByteString
        > 
        > D.decode a
        ()
        > D.decode a :: Int
        *** Exception: Data.Binary.Get.runGet at position 0: demandInput: not enough bytes
        > D.decode a :: Integer
        10
        > 
    
    
    
        > let b = D.encode (Just 10 :: Maybe Int)
        > :t b
        b :: BL.ByteString
        > b
        "\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n"
        > 
    
        > D.decode b :: Maybe Int
        Just 10
        >
    
        > let c = D.encode (Nothing :: Maybe Int)
        > c
        "\NUL"
        >
        > D.decode c :: Maybe Int
        Nothing
        >
    
    
        > let d = D.encode (3.11415e4 :: Double)
        > d
        "\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\NUL\NUL\NUL`i\RS\255\255\255\255\255\255\255\218"
        > 
    
    {- Bytes to Bytestring -}
    {---------------------------------------------}
    
        {- Bytes are encoded as Word8 -}
    
        > :t B.pack
        B.pack :: [GHC.Word.Word8] -> B.ByteString
        > :t B.unpack
        B.unpack :: B.ByteString -> [GHC.Word.Word8]
        > 
        > :t BL.pack
        BL.pack :: [GHC.Word.Word8] -> BL.ByteString
        > :t BL.unpack
        BL.unpack :: BL.ByteString -> [GHC.Word.Word8]
        > 
    
        > let msg = B.pack [72,101,108,108,111,32,119,111,114,108,100]
        > msg
        "Hello world"
    
        > let bytes = B.unpack msg
        > bytes
        [72,101,108,108,111,32,119,111,114,108,100]
        > 
    
        > :t msg
        msg :: B.ByteString
    
        > :t bytes
        bytes :: [GHC.Word.Word8]
        > 
    
        {- Word8 to Int -}
    
        > :t fromIntegral 
        fromIntegral :: (Num b, Integral a) => a -> b
        > 
    
        > let x = 100 :: GHC.Word.Word8
        > x
        100
        > :t x
        x :: GHC.Word.Word8
        > 
    
        > let y = fromIntegral x :: Int
        > y
        100
        > :t y
        y :: Int
        > 
    
        > fromIntegral ( 100 :: GHC.Word.Word8) :: Int
        100
        > 
    
        > let bytes_int = map (\x -> fromIntegral x :: Int) bytes
        > bytes_int 
        [72,101,108,108,111,32,119,111,114,108,100]
        > :t bytes_int 
        bytes_int :: [Int]
        > 
    
        > let bytes_word8 = map (\x -> fromIntegral x ::  GHC.Word.Word8) bytes_int 
        > bytes_word8 
        [72,101,108,108,111,32,119,111,114,108,100]
        > :t bytes_word8 
        bytes_word8 :: [GHC.Word.Word8]
        > 
    
    {- Bytes in Hexadecimal and Binary Format -}
    
        > import Numeric (showHex, showIntAtBase)
        > import Data.Char (intToDigit)
        > import Data.Bits (shiftL, shiftR, (.&.), (.|.))
        > import Data.Word (Word8)
    
        > 
            {- Bytes in Hexadecimal Format Low Endian -}
        > map (\n -> showHex n "") bytes
        ["48","65","6c","6c","6f","20","77","6f","72","6c","64"]
        > 
        > 
    
        {- Bytes in Hexadecimal Format Big Endian -}
    
        > reverse $ map (\n -> showHex n "") bytes
        ["64","6c","72","6f","77","20","6f","6c","6c","65","48"]
        > 
    
        {- Hexadecimal String Low Endian -}
    
        > concat $ map (\n -> showHex n "") bytes
        "48656c6c6f20776f726c64"
        > 
    
        {- Hexadecimal String Bing Ending -}
        > concat $ reverse $ map (\n -> showHex n "") bytes
        "646c726f77206f6c6c6548"
        > 
    
        {- Hexadecimal number to bytes -}
    
        
        > 
        > let num2hex = \n -> showHex n ""
        > 
        > let x = 0x646c726f77206f6c6c6548
        > x
        121404708502361365413651784
        > 
        > num2hex x
        "646c726f77206f6c6c6548"
        > 
        
        {- Split Bytes -}
        {-------------------------------------------}
        
        > iterate (\x -> shiftR x 8) 0x646c726f77206f6c6c6548 |> takeWhile ((/=) 0) |> map (.&. 0xFF)
        [72,101,108,108,111,32,119,111,114,108,100]
        > 
        >  map num2hex .  map (.&. 0xFF) . takeWhile ((/=) 0) .  iterate (\x -> shiftR x 8) $  0x646c726f77206f6c6c6548
        ["48","65","6c","6c","6f","20","77","6f","72","6c","64"]
        >
    
        > iterate (\x -> shiftR x 8) 0x646c726f77206f6c6c6548 |> takeWhile ((/=) 0) |> map (.&. 0xFF) |> map num2hex
        ["48","65","6c","6c","6f","20","77","6f","72","6c","64"]
        > 
    
        > iterate (\x -> shiftR x 8) 0x646c726f77206f6c6c6548 |> takeWhile ((/=) 0) |> map (.&. 0xFF) |> map num2hex |> concat
        "48656c6c6f20776f726c64"
        > 
        > iterate (\x -> shiftR x 8) 0x646c726f77206f6c6c6548 |> takeWhile ((/=) 0) |> map (.&. 0xFF) |> map num2hex |> reverse |> concat
        "646c726f77206f6c6c6548"
        > 
    
        > let int2word8 = \x -> fromIntegral x :: Word8
        > 
        > let num2bytes = map int2word8 . map (.&. 0xFF) . takeWhile ((/=) 0) .  iterate (\x -> shiftR x 8)
    
        > :t num2bytes
        num2bytes :: (Data.Bits.Bits a, Integral a) => a -> [Word8]
        > 
        > 
        > num2bytes 0x646c726f77206f6c6c6548
        [72,101,108,108,111,32,119,111,114,108,100]
        > 
        > num2bytes 0x646c726f77206f6c6c6548 |> map num2hex
        ["48","65","6c","6c","6f","20","77","6f","72","6c","64"]
        > 
    
        > num2bytes 0x646c726f77206f6c6c6548 |> BL.pack 
        "Hello world"
        > 
        > 
        
        {- Bytes to Int -}
        
        > let bytes = [72,101,108,108,111,32,119,111,114,108,100]
        > foldr (\b0 b1 -> (shiftL b1 8) + b0) 0 bytes
        121404708502361365413651784
    
        > num2hex . foldr (\b0 b1 -> (shiftL b1 8) + b0) 0 $ bytes 
        "646c726f77206f6c6c6548"
        > 
    
        > let bytes2int =  foldr (\b0 b1 -> (shiftL b1 8) + b0) 0
        > bytes2int bytes
        121404708502361365413651784
        >
    ```
    
    **See also**
    
    **Base64 Algorithm**
    
    -   [RFC4648 - The Base16, Base32, and Base64 Data Encodings](http://web.archive.org/web/20131125120025/http://www.apps.ietf.org/rfc/rfc4648.html)
    
    -   [Ten Minute Tutor - Base64 encoding](http://tenminutetutor.com/base64-encoding)

3.  Documentation in Hackage

    **ByteString**
    
    **An efficient compact, immutable byte string type (both strict and lazy) suitable for binary or 8-bit character data.**
    
    **The ByteString type represents sequences of bytes or 8-bit characters. It is suitable for high performance use, both in terms of large data quantities, or high speed requirements. The ByteString functions follow the same style as Haskell's ordinary lists, so it is easy to convert code from using String to ByteString.**
    
    -   [Bytestring Package](http://hackage.haskell.org/package/bytestring)
    -   [Data.ByteString](http://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString.html)
    -   [Data.ByteString.Char8](http://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Char8.html)
    -   [Data.ByteString.Lazy.Char8](http://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Lazy-Char8.html)
    
    **Data.Word**
    
    **Unsigned integer types: Word8, Word16, Word32, Word64**
    
    [Data.Word](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Word.html)
    
    **Data.Binary package**
    
    **Binary serialisation of Haskell values to and from lazy ByteStrings. The Binary library provides methods for encoding Haskell values as streams of bytes directly in memory. The resulting ByteString can then be written to disk, sent over the network, or futher processed (for example, compressed with gzip).**
    
    -   [Data.Binary](http://hackage.haskell.org/package/binary-0.5.0.2)
    
    **Data.Bits package**
    
    **This module defines bitwise operations for signed and unsigned integers. Instances of the class Bits for the Int and Integer types are available from this module, and instances for explicitly sized integral types are available from the Data.Int and Data.Word modules.**
    
    -   [Data.Bits](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base/Data-Bits.html)
    
    **Numeric**
    
    **Odds and ends, mostly functions for reading and showing RealFloat-like kind of values.**
    
    [Numeric](http://hackage.haskell.org/package/base-4.7.0.1/docs/Numeric.html)
    
    **Data.Char**
    
    **The Char type and associated operations.**
    
    [Data.Char](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Char.html)

### Parse Binary Files<a id="sec-0-1-4" name="sec-0-1-4"></a>

To decode a binary file it is necessary to know:

-   The Binary file format or specification
-   File Signature that comes in the file header.
-   Data Size (8 bits signed, 8 bits unsigned, &#x2026;.)
-   Byte Endianess or Byte order (Little Endian or Big Endian)

# <a id="sec-1" name="sec-1"></a>

This section will show how to parse the windows executable, PE32 executable.

**PE32 Layout Specification**

-   [Undocumented PECOFF - Reversing Labs](https://media.blackhat.com/bh-us-11/Vuksan/BH_US_11_VuksanPericin_PECOFF_WP.pdf)

-   <https://en.wikipedia.org/wiki/Portable_Executable>

-   <http://www.csn.ul.ie/~caolan/publink/winresdump/winresdump/doc/pefile.html>

```haskell
import Data.Word --- Unsigned integer types: Word8, Word16, Word32, Word64 

import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as BC
import Data.Binary
import Data.Binary.Get

> let (|>) x f = f x
> 

> :t BL.readFile 
BL.readFile :: FilePath -> IO BL.ByteString

> fdata <- BL.readFile "notepad.exe" 
> 
> :t fdata
fdata :: BL.ByteString
> 

> :t BL.take 
BL.take :: GHC.Int.Int64 -> BL.ByteString -> BL.ByteString


{- Read File Signare 2 bytes, always 0x5A4D OR ("MZ")
 - 
 -}
> BL.take 2 fdata
"MZ"
> 

> let toHex :: [Word8] -> [String] ;toHex = Prelude.map ( (\n -> showHex n "") . (\x -> fromIntegral x :: Int) )
> 
>  BL.take 2 fdata |> BL.unpack |> toHex 
["4d","5a"]
> 

read_dosHeader = do
```

**PE32 Documentation**

-   <https://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files#MS-DOS_COM_Files>

![img](https://commons.wikimedia.org/wiki/File:RevEngPEFile.JPG)

Dos Header Structure

From:

-   <http://www.joachim-bauch.de/tutorials/loading-a-dll-from-memory/>

```C
typedef struct _IMAGE_DOS_HEADER {      // DOS .EXE header
    WORD   e_magic;                     // Magic number
    WORD   e_cblp;                      // Bytes on last page of file
    WORD   e_cp;                        // Pages in file
    WORD   e_crlc;                      // Relocations
    WORD   e_cparhdr;                   // Size of header in paragraphs
    WORD   e_minalloc;                  // Minimum extra paragraphs needed
    WORD   e_maxalloc;                  // Maximum extra paragraphs needed
    WORD   e_ss;                        // Initial (relative) SS value
    WORD   e_sp;                        // Initial SP value
    WORD   e_csum;                      // Checksum
    WORD   e_ip;                        // Initial IP value
    WORD   e_cs;                        // Initial (relative) CS value
    WORD   e_lfarlc;                    // File address of relocation table
    WORD   e_ovno;                      // Overlay number
    WORD   e_res[4];                    // Reserved words
    WORD   e_oemid;                     // OEM identifier (for e_oeminfo)
    WORD   e_oeminfo;                   // OEM information; e_oemid specific
    WORD   e_res2[10];                  // Reserved words
    LONG   e_lfanew;                    // File address of new exe header
  } IMAGE_DOS_HEADER, *PIMAGE_DOS_HEADER;
```

COFF Header

```
 struct COFFHeader
 {
    short Machine;
    short NumberOfSections;
    long TimeDateStamp;
    long PointerToSymbolTable;
    long NumberOfSymbols;
    short SizeOfOptionalHeader;
    short Characteristics;
 }
```

### GnuPlot<a id="sec-1-0-1" name="sec-1-0-1"></a>

Installation:

```
$ cabal install gnuplot
$ sudo apt-get install gnuplot-x11 # Ubuntu/ Debian
```

Examples:

```haskell
> import Graphics.Gnuplot.Simple
> plotList [] [(1, 1), (2, 2), (3, 3)]
```

```haskell
> import Graphics.Gnuplot.Simple
> plotFunc [] (linearScale 1000 (-20,20)) (\x -> sin x / x)
```

```haskell
> plotList [Title "A title", XLabel "x label"] [(2,10),(3,15),(4,14),(5,19)]
> plotList [Title "A title", XLabel "x label", YLabel "the y label"] [(2,10),(3,15),(4,14),(5,19)]
```

**Doucumentation**

**This is a wrapper to gnuplot which lets you create 2D and 3D plots.**
**Start a simple session with make ghci. This loads the module Graphics.Gnuplot.Simple which is ready for use in GHCi. It does not address all fancy gnuplot features in order to stay simple. For more sophisticated plots, especially batch generated graphics, I recommend Graphics.Gnuplot.Advanced. This module contains also an overview of the hierarchy of objects.**

[Graphics.Gnuplot](https://hackage.haskell.org/package/gnuplot)
