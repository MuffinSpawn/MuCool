module MuCool.Time where

import Data.Fixed (Pico)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.Word

getTimestamp :: IO (String)
getTimestamp = do
  datetime <- getZonedTime
  let daysOfTheWeek = ["", "Monday", "Tuesday", "Wednesday", "Thursday",
                           "Friday", "Saturday", "Sunday"]
      months = ["", "January", "February", "March", "April",
                    "May", "June", "July", "August",
                    "September", "October", "November", "December"]
      periods = ["AM", "PM"]
      date = localDay (zonedTimeToLocalTime datetime)
      (_, _, dayOfTheWeek) = toWeekDate date
      (year, month, day) = toGregorian date
      time = localTimeOfDay (zonedTimeToLocalTime datetime)
      (hour, minute, second) = (todHour time, todMin time,
                                simplifySecond(todSec time))
      period = periods !! (quot hour 12)
  return ((daysOfTheWeek !! dayOfTheWeek) ++ ", " ++ (months !! month) ++ " "
         ++ (show day) ++ ", " ++ (show year) ++ " " ++ (show (mod hour 12))
         ++ ":" ++ (padDigits minute) ++ ":" ++ (padDigits second) ++ " "
         ++ period)

simplifySecond :: Pico -> Word8
simplifySecond second
  | secondWord > 59 = 59
  | otherwise = secondWord
  where secondWord = round second

padDigits :: (Num a, Ord a, Show a) => a -> String
padDigits digits
  | digits < 10 = "0" ++ (show digits)
  | otherwise = show digits