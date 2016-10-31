{-
-  App to decode Binary File PE32 windows executable format.
-
    http://wiki.osdev.org/PE
    http://www.csn.ul.ie/~caolan/publink/winresdump/winresdump/doc/pefile.html
    https://code.google.com/p/corkami/wiki/PE101?show=content
    https://code.google.com/p/corkami/wiki/PE?show=content

    http://edc.tversu.ru/elib/inf/0028/app01lev1sec3.html
    http://www.ntcore.com/files/inject2exe.htm
    
    https://www.simple-talk.com/blogs/2011/03/15/anatomy-of-a-net-assembly-pe-headers/
    
    http://www.geekdroppings.com/2014/03/26/useful-dll-utilities-on-windows/

> fdata  <- BL.readFile "notepad.exe"

> let (dos_header, coff_header, pe_signature, option_header) = runGet pe32_decoder fdata 


> machine coff_header |> findCpu
"Intel 386"

> pe_signature
"PE\NUL\NUL"
> 


> magic option_header |> w16toHex 
"10b"
> 


> ppshow coff_header 
ImageHeader {machine = 332
 numberOfSections = 3
 timeDateStamp = 0
 pointerToSymbolTable = 0
 numberOfSymbols = 0
 sizeOfOptionalHeader = 224
 characteristics = 258}


> dos_header 
DosHeader {e_magic = 23117, e_cblp = 64, e_cp = 1, e_crlc = 0, e_cparhdr = 6, e_minalloc = 0, e_maxalloc = 65535, e_ss = 0, e_sp = 184, e_csum = 0, e_ip = 0, e_cs = 0, e_lfarlc = 96, e_ovno = 0, e_res = [0,0,0,0], e_oemid = 0, e_oeminfo = 0, e_res2 = [0,0,0,0,0,0,0,0,0,0], e_lfanew = 96}




> ppshow option_header 
OptionalHeader {magic = 267
 majorLinkerVersion = 0
 minorLinkerVersion = 0
 sizeOfCode = 8
 sizeOfInitializedData = 0
 sizeOfUninitializedData = 0
 addressOfEntryPoint = 4096
 baseOfCode = 4096
 baseOfData = 0
 imageBase = 268435456
 sectionAlignment = 4096
 fiAlignment_ = 512
 majorOperatingSystemVersion = 1
 minorOperatingSystemVersion = 0
 majorImageVersion = 0
 minorImageVersion = 0
 majorSubsystemVersion = 4
 minorSubsystemVersion = 0
 win32VersionValue = 0
 sizeOfImage = 192512
 sizeOfHeaders = 512
 checkSum = 0
 subsystem = 2
 dllCharacteristics = 256
 sizeOfStackReserve = 1048576
 sizeOfStackCommit = 4096
 sizeOfHeapReserve = 1048576
 sizeOfHeapCommit = 4096
 loaderFlags = 0
 numberOfRvaAndSizes = 16}




    > fdata <- BL.readFile "notepad.exe"
    > let header = runGet headerDecoder fdata

    > e_magic header |> w16toHex
    "5a4d"
    >
http://pnyf.inf.elte.hu/haskell/package/binary/Data-Binary-Get.html#v:getWord32le

* http://www.rebeccaskinner.net/blog/2013/09/07/understanding-the-haskell-ffi/

Sources:

    - http://www.joachim-bauch.de/tutorials/loading-a-dll-from-memory/

    - http://www.skyfree.org/linux/references/coff.pdf
    - http://wiki.yobi.be/wiki/Reverse-Engineering
    - https://media.blackhat.com/bh-us-11/Vuksan/BH_US_11_VuksanPericin_PECOFF_WP.pdf
    - https://msdn.microsoft.com/en-us/library/ms809762.aspx
    - http://www.csn.ul.ie/~caolan/pub/winresdump/winresdump/doc/pefile2.html

    - https://hackage.haskell.org/package/peparser-0.6/docs/src/Data-PE-Parser.html

    -- https://drive.google.com/file/d/0B3_wGJkuWLytbnIxY1J5WUs4MEk/view?pli=1
    -- https://drive.google.com/file/d/0B3_wGJkuWLytQmc2di0wajB1Xzg/view?pli=1

    --  https://code.google.com/p/corkami/wiki/PE

    --- http://d.hatena.ne.jp/hh04/20121103/1351945956


    -- http://stackoverflow.com/questions/23311559/writing-structured-binary-files-in-haskell


 Portable Executable (PE) file format. The new PE file format draws primarily from the COFF (Common Object File Format) specification that is common to UNIX® operating systems. Yet, to remain compatible with previous versions of the MS-DOS® and Windows operating systems, the PE file format also retains the old familiar MZ header from MS-DOS.



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

-}

import Data.Word --- Unsigned integer types: Word8, Word16, Word32, Word64

import qualified  Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Binary
import Data.Binary.Get

import Data.Int (Int64, Int32, Int16, Int8, Int)
import Numeric
import Control.Monad (replicateM)

import qualified Data.List.Split as S

(|>) x f =  f x


intToHex n =  showHex n ""

w8toHex :: Word8 -> String
w8toHex = (\n -> showHex n "") . (\x -> fromIntegral x :: Int)

w16toHex :: Word16 -> String
w16toHex = (\n -> showHex n "") . (\x -> fromIntegral x :: Int)

w32toHex :: Word32 -> String
w32toHex = (\n -> showHex n "") . (\x -> fromIntegral x :: Int)

w16toInt :: Word16 -> Int
w16toInt x = fromIntegral x :: Int

w32toInt :: Word32 -> Int
w32toInt x = fromIntegral x :: Int

intToInt64 :: Int -> Int64
intToInt64 x = fromIntegral x :: Int64

int64toInt :: Int64 -> Int
int64toInt x = fromIntegral x :: Int

-- show |> S.splitOn "," |> unlines |> putStrLn
--
-- 
ppshow x = x |> show |> S.splitOn ", " |> unlines 
--putStrLn . unlines . S.splitOn . show $ x

--ppformat x = show ( ppshow x)

data DosHeader = DosHeader
  {
    e_magic     :: Word16,   -- Magic number
    e_cblp      :: Word16,   -- Bytes on last page of file
    e_cp        :: Word16,   -- Pages in file
    e_crlc      :: Word16,   -- Relocations
    e_cparhdr   :: Word16,   -- Size of header in paragraphs
    e_minalloc  :: Word16,   -- Minimum extra paragraphs needed
    e_maxalloc  :: Word16,   -- Maximum extra paragraphs needed
    e_ss        :: Word16,   -- Initial (relative) SS value
    e_sp        :: Word16,   -- Initial SP value
    e_csum      :: Word16,   -- Checksum
    e_ip        :: Word16,   -- Initial IP value
    e_cs        :: Word16,   -- Initial (relative) CS value
    e_lfarlc    :: Word16,   -- File address of relocation table
    e_ovno      :: Word16,   -- Overlay number

    e_res       :: [Word16], -- Reserved words
    e_oemid     :: Word16,   -- OEM identifier (for e_oeminfo)
    e_oeminfo   :: Word16,   -- OEM information; e_oemid specific
    e_res2      :: [Word16], -- Reserved words
    e_lfanew    :: Word32    -- File address of new exe header

  } deriving (Show)

{-

typedef struct _IMAGE_FILE_HEADER {
    WORD    Machine;
    WORD    NumberOfSections;
    DWORD   TimeDateStamp;
    DWORD   PointerToSymbolTable;
    DWORD   NumberOfSymbols;
    WORD    SizeOfOptionalHeader;
    WORD    Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;

-}


--  This is also knwon as COFF header
--
--  https://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files#MS-DOS_COM_Files
--
--
data ImageHeader = ImageHeader
    {
    machine                 :: Word16, -- 2
    numberOfSections        :: Word16, -- 2
    timeDateStamp           :: Word32, -- 4
    pointerToSymbolTable    :: Word32, -- 4
    numberOfSymbols         :: Word32, -- 4
    sizeOfOptionalHeader    :: Word16, -- 2
    characteristics         :: Word16  -- 2  => 8 + 12 = 20 = 14h

    } deriving (Show)



data OptionalHeader = OptionalHeader
    {
        magic                       :: Word16,
        majorLinkerVersion          :: Word8,
        minorLinkerVersion          :: Word8,
        sizeOfCode                  :: Word32,
        sizeOfInitializedData       :: Word32,
        sizeOfUninitializedData     :: Word32,
        addressOfEntryPoint         :: Word32,
        baseOfCode                  :: Word32,
        baseOfData                  :: Word32,
        imageBase                   :: Word32,
        sectionAlignment            :: Word32,
        fiAlignment_                :: Word32,
        majorOperatingSystemVersion :: Word16,
        minorOperatingSystemVersion :: Word16,
        majorImageVersion           :: Word16,
        minorImageVersion           :: Word16,
        majorSubsystemVersion       :: Word16,
        minorSubsystemVersion       :: Word16,
        win32VersionValue           :: Word32,
        sizeOfImage                 :: Word32,
        sizeOfHeaders               :: Word32,
        checkSum                    :: Word32,
        subsystem                   :: Word16,
        dllCharacteristics          :: Word16,
        sizeOfStackReserve          :: Word32,
        sizeOfStackCommit           :: Word32,
        sizeOfHeapReserve           :: Word32,
        sizeOfHeapCommit            :: Word32,
        loaderFlags                 :: Word32,
        numberOfRvaAndSizes         :: Word32
    } deriving (Show)




headerDecoder = do
    e_magic'     <- getWord16le        -- Magic number
    e_cblp'      <- getWord16le        -- Bytes on last page of file
    e_cp'        <- getWord16le        -- Pages in file
    e_crlc'      <- getWord16le        -- Relocations
    e_cparhdr'   <- getWord16le        -- Size of header in paragraphs
    e_minalloc'  <- getWord16le        -- Minimum extra paragraphs needed
    e_maxalloc'  <- getWord16le        -- Maximum extra paragraphs needed
    e_ss'        <- getWord16le        -- Initial (relative) SS value
    e_sp'        <- getWord16le        -- Initial SP value
    e_csum'      <- getWord16le        -- Checksum
    e_ip'        <- getWord16le        -- Initial IP value
    e_cs'        <- getWord16le        -- Initial (relative) CS value
    e_lfarlc'    <- getWord16le        -- File address of relocation table
    e_ovno'      <- getWord16le        -- Overlay number

    e_res'       <- replicateM 4 getWord16le      --  WORD   e_res[4] -- Reserved words
    e_oemid'     <- getWord16le                   --  OEM identifier (for e_oeminfo)
    e_oeminfo'   <- getWord16le                   --  OEM information; e_oemid specific
    e_res2'      <- replicateM  10 getWord16le    --  Word e_res2[10]  Reserved words
    e_lfanew'    <- getWord32le

    let header = DosHeader {
        e_magic = e_magic',
        e_cblp = e_cblp',
        e_cp = e_cp',
        e_crlc = e_crlc',
        e_cparhdr = e_cparhdr',
        e_minalloc = e_minalloc',
        e_maxalloc = e_maxalloc',
        e_ss = e_ss',
        e_sp = e_sp',
        e_csum = e_csum',
        e_ip = e_ip',
        e_cs = e_cs',
        e_lfarlc = e_lfarlc',
        e_ovno = e_ovno',
        e_res = e_res',
        e_oemid = e_oemid',
        e_oeminfo = e_oeminfo',
        e_res2 = e_res2',
        e_lfanew = e_lfanew'
    }

    return header

imageDecoder = do
    machine'              <- getWord16le
    numberOfSections'     <- getWord16le
    timeDateStamp'        <- getWord32le
    pointerToSymbolTable' <- getWord32le
    numberOfSymbols'      <- getWord32le
    sizeOfOptionalHeader' <- getWord16le
    characteristics'      <- getWord16le

    let header = ImageHeader {
        machine = machine',
        numberOfSections  = numberOfSections',
        timeDateStamp = timeDateStamp',
        pointerToSymbolTable = pointerToSymbolTable',
        numberOfSymbols = numberOfSymbols',
        sizeOfOptionalHeader = sizeOfOptionalHeader',
        characteristics = characteristics'
        }

    return header


optionHeader_decoder = do
    ---
    --- Referece:  http://www.joachim-bauch.de/tutorials/loading-a-dll-from-memory/
    ---
    ----- Standard Fields
    ----
    -- Magic Number always: 0x10b = 267
    magic'                       <- getWord16le

    majorLinkerVersion'          <- getWord8
    minorLinkerVersion'          <- getWord8
    sizeOfCode'                  <- getWord32le
    sizeOfInitializedData'       <- getWord32le
    sizeOfUninitializedData'     <- getWord32le
    addressOfEntryPoint'         <- getWord32le
    baseOfCode'                  <- getWord32le
    baseOfData'                  <- getWord32le

    ---' NT additional fields

    imageBase'                   <- getWord32le
    sectionAlignment'            <- getWord32le
    fileAlignment'               <- getWord32le
    majorOperatingSystemVersion' <- getWord16le
    minorOperatingSystemVersion' <- getWord16le
    majorImageVersion'           <- getWord16le
    minorImageVersion'           <- getWord16le
    majorSubsystemVersion'       <- getWord16le
    minorSubsystemVersion'       <- getWord16le
    win32VersionValue'           <- getWord32le
    sizeOfImage'                 <- getWord32le
    sizeOfHeaders'               <- getWord32le
    checkSum'                    <- getWord32le
    subsystem'                   <- getWord16le
    dllCharacteristics'          <- getWord16le
    sizeOfStackReserve'          <- getWord32le
    sizeOfStackCommit'           <- getWord32le
    sizeOfHeapReserve'           <- getWord32le
    sizeOfHeapCommit'            <- getWord32le
    loaderFlags'                 <- getWord32le
    numberOfRvaAndSizes'         <- getWord32le


    let header = OptionalHeader {
        magic = magic',
        majorLinkerVersion = majorLinkerVersion',
        minorLinkerVersion = minorLinkerVersion',
        sizeOfCode = sizeOfCode',
        sizeOfInitializedData = sizeOfInitializedData',
        sizeOfUninitializedData = sizeOfUninitializedData',
        addressOfEntryPoint = addressOfEntryPoint',
        baseOfCode = baseOfCode',
        baseOfData = baseOfData',
        imageBase = imageBase',
        sectionAlignment = sectionAlignment',
        fiAlignment_ = fileAlignment',
        majorOperatingSystemVersion = majorOperatingSystemVersion',
        minorOperatingSystemVersion = minorOperatingSystemVersion',
        majorImageVersion = majorImageVersion',
        minorImageVersion = minorImageVersion',
        majorSubsystemVersion = majorSubsystemVersion',
        minorSubsystemVersion = minorSubsystemVersion',
        win32VersionValue = win32VersionValue',
        sizeOfImage = sizeOfImage',
        sizeOfHeaders = sizeOfHeaders',
        checkSum = checkSum',
        subsystem = subsystem',
        dllCharacteristics = dllCharacteristics',
        sizeOfStackReserve = sizeOfStackReserve',
        sizeOfStackCommit = sizeOfStackCommit',
        sizeOfHeapReserve = sizeOfHeapReserve',
        sizeOfHeapCommit = sizeOfHeapCommit',
        loaderFlags = loaderFlags',
        numberOfRvaAndSizes = numberOfRvaAndSizes'
    }

    return header
    ---
    ---  NT additional Fields
    ---


--
--
-- https://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files#MS-DOS_COM_Files
--
findCpu machine_code =
    case machine_code of
        0x14c -> "Intel 386"
        0x14d -> "Intel i860"
        0x162 -> "MIPS R3000"
        0x166 -> "MIPS little endian (R4000)"
        0x168 -> "MIPS R10000"
        0x169 -> "MIPS little endian WCI v2"
        0x183 -> "old Alpha AXP"
        0x184 -> "Alpha AXP"
        0x1a2 -> "Hitachi SH3"
        0x1a3 -> "Hitachi SH3 DSP"
        0x1a6 -> "Hitachi SH4"
        0x1a8 -> "Hitachi SH5"
        0x1c0 -> "ARM little endian"
        0x1c2 -> "Thumb"
        0x1d3 -> "Matsushita AM33"
        0x1f0 -> "PowerPC little endian"
        0x1f1 -> "PowerPC with floating point support"
        0x200 -> "Intel IA64"
        0x266 -> "MIPS16"
        0x268 -> "Motorola 68000 series"
        0x284 -> "Alpha AXP 64-bit"
        0x366 -> "MIPS with FPU"
        0x466 -> "MIPS16 with FPU"
        0xebc -> "EFI Byte Code"
        0x8664 -> "AMD AMD64"
        0x9041 -> "Mitsubishi M32R little endian"
        0xc0ee -> "clr pure MSIL"

{-

    > fdata  <- BL.readFile "notepad.exe"
    >
    > let
    > let (dos_header, coff_header, pe_signature) = runGet pe32_decoder fdata
    > dos_header
    DosHeader {e_magic = 23117, e_cblp = 64, e_cp = 1, e_crlc = 0, e_cparhdr = 6, e_minalloc = 0, e_maxalloc = 65535, e_ss = 0, e_sp = 184, e_csum = 0, e_ip = 0, e_cs = 0, e_lfarlc = 96, e_ovno = 0, e_res = [0,0,0,0], e_oemid = 0, e_oeminfo = 0, e_res2 = [0,0,0,0,0,0,0,0,0,0], e_lfanew = 96}
    >
    > coff_header
    ImageHeader {machine = 332, numberOfSections = 3, timeDateStamp = 0, pointerToSymbolTable = 0, numberOfSymbols = 0, sizeOfOptionalHeader = 224, characteristics = 258}
    >
    > pe_signature
    "PE\NUL\NUL"
    >
    > machine coff_header |> findCpu
    "Intel 386"
    >
    > :t pe32_decoder
    pe32_decoder :: Get (DosHeader, ImageHeader, BC.ByteString)
    >
-}
--
--  Get State Monad
--
--  pe32_decoder :: Get (DosHeader, ImageHeader, BC.ByteString)
pe32_decoder = do

    dos_header <- headerDecoder
    let offset = w32toInt (e_lfanew dos_header)
    
    pos0 <-  fmap int64toInt bytesRead
    
    dos_stub <- getByteString  (offset - pos0)


    pos <- fmap int64toInt bytesRead

    -- The offset inside a Get state moand is
    -- rlative to the current position
    --
    pe_signature <- skip (offset - pos) >> getByteString 4

    coff_header <- imageDecoder

    option_header <- optionHeader_decoder
    --magic <- getWord16le

    --return header
    return (dos_header, dos_stub, coff_header, pe_signature, option_header)


testDedcoder = do
    fdata  <- BL.readFile "notepad.exe"
    let header = runGet headerDecoder fdata
    let offset = w32toInt (e_lfanew header)
    
    
    
    let (pe_signature, _, offset_image) =  runGetState (skip offset >> getByteString 4)  fdata   1

    let image_data = runGet (skip (int64toInt offset_image  - 1) >> imageDecoder) fdata

    --return header
    return (header, pe_signature, image_data)



do_report filename = do
    fdata <- BL.readFile filename
    
    let (dos_header, 
         dos_stub, 
         coff_header, 
         pe_signature, 
         option_header) = runGet pe32_decoder fdata
    
    let file_signature = w16toHex (e_magic dos_header)

    putStrLn ("\nFile Signature: 0x" ++ file_signature)


    putStrLn "\nDos Header :\n---------------\n"
    putStrLn  (ppshow  dos_header)

    putStrLn "\nDos Stub"
    putStrLn "------------------------------"
    
    putStrLn (BC.unpack dos_stub)

    putStrLn "PE Section"
    putStrLn "------------------------------\n"
    
    putStrLn $ "PE Signature : " ++ (BC.unpack pe_signature) 
    
    putStrLn "\nCoff Header \n"
    
    putStrLn  (ppshow  coff_header)
    
    
    putStrLn $ "Machine : " ++ (findCpu (machine coff_header))
    
    putStrLn "\nOption Header\n"
    
    putStrLn (ppshow option_header)
