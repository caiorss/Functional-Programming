{-

http://foswiki.cs.uu.nl/foswiki/pub/TC/CourseAssignments/1Part1-DateTime-iCal-AST.pdf

λ > Date 2012 5 10
Date {date_year = 2012, date_month = 5, date_day = 10}
λ > 

λ > today
DateTime {datetime_date = Date {date_year = 2012, date_month = 5, date_day = 10}, datetime_time = Time {time_hour = 10, time_minute = 25, time_second = 0}, datetime_utc = True}
λ > 
λ > 


λ > let today = DateTime (Date 2012 5 10) (Time 10 25 0) True
λ > 
λ > today
DateTime {datetime_date = Date {date_year = 2012, date_month = 5, date_day = 10}, datetime_time = Time {time_hour = 10, time_minute = 25, time_second = 0}, datetime_utc = True}
λ > 
λ > 


-}

import Text.Printf --(printf)





data Date = Date {
                    date_year  :: Int,
                    date_month :: Int,
                    date_day   :: Int
                  } 
                  deriving (Show, Eq, Read)
                  

data Time = Time {
                    time_hour   :: Int,
                    time_minute :: Int,
                    time_second :: Int
                  }
                  deriving (Show, Eq, Read)                  

data DateTime = DateTime {
                          datetime_date :: Date,
                          datetime_time  :: Time,
                          datetime_utc   :: Bool                         
                          }
                          deriving (Show, Eq, Read)


data Month =  Jan | Feb | Mar | Apr | May 
              | June | July | Aug | Sept | Oct | Nov | Dec 
              deriving (Eq, Show, Read, Ord, Enum)


data Week = Mon | Tue | Wed | Thu | Fri | Sat | Sun
            deriving (Eq, Show, Read, Ord, Enum)

datetimeDate y m d = DateTime (Date y m d) (Time 0 0 0) True


data Reminder = Reminder DateTime String
                deriving (Eq, Show)

data REM =
      onDate   Int Int Int    -- Year Month Day Message
    | RemDayAt (Int, Int, Int) (Int, Int, Int) 
    | RemYear   Int  Month Int  
    deriving (Eq, Show, Read)



remind  (onDate  year month day) message = Reminder (datetimeDate year month day) message


-- 1 January 1970
zerodate =  datetimeDate 1970 1  1  

-- show(year) ++ "-" ++ show(month) ++ "-" ++ show(day)

--print_ymd :: DateTime -> PrintfType
print_ymd dtime = printf "%d %d %d" year month date 
                  where 
                  date   =  datetime_date  dtime
                  year   =  date_year  date
                  month  =  date_month date
                  day    =  date_day   date


isLeapYear year | (mod year 4)    /=  0 = False
                | (mod year 100)  ==0   = (mod year 400) == 0
                | otherwise             =  True


daysToMonth365 = [ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]

daysToMonth366 = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ]

ticksInMillisecond = 10000 :: Int
ticksInSecond = ticksInMillisecond * 1000 :: Int

--timeToticks :: Int t => t -> t -> t -> t
--timeToticks :: Int t => t -> t -> t -> t
timeToticks h m s  = 3600*h + 60*m + s

dateToTicks year month day = ticks
                
                where 
                --day_ = fromInteger day
                --month_ = fromInteger month
                --year_ = fromInteger year
                
                daysToMonth = if isLeapYear(year) 
                              then daysToMonth366 
                              else daysToMonth365
                
                previousYear = year - 1
                
                daysInPreviousYears = ((((previousYear * 365) + (previousYear `div` 4)) - (previousYear `div` 100)) + (previousYear `div` 400))
                
                totalDays = ((daysInPreviousYears + daysToMonth !! (month - 1)) + day) - 1
                
                ticks = totalDays * 0xc92a69c000

getTimeStamp year month day hour minute second milliseconds =
    dateToTicks year month day 
    + timeToticks hour minute second
    + milliseconds * ticksInMillisecond


unixTimestamp datetime = getTimeStamp  year month day hour minute second 0 
                where
                year    = date_year  $ datetime_date datetime
                month   = date_month $ datetime_date datetime
                day     = date_day   $ datetime_date datetime
                
                hour    = time_hour    $ datetime_time datetime
                minute  = time_minute  $ datetime_time datetime
                second  = time_second  $ datetime_time datetime
                

cases = [ (2004, True),  
          (2005, False), 
          (2009, False), 
          (2012, True), 
          (2124, True), 
          (2003, False) ]

testfunction func cases = (map isLeapYear $ fst $ unzip cases) == (snd $ unzip cases)
                    
isLeap_test = testfunction isLeapYear cases
