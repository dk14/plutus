module Language.Marlowe.ACTUS.Utility.YearFraction where

import Data.Time
import GHC.Stack
import Debug.Trace

import Language.Marlowe.ACTUS.ContractTerms

yearFraction :: DCC -> Day -> Day -> Day -> Double
yearFraction DCC_A_AISDA startDay endDay _
  | startDay <= endDay =
    let (d1Year, _, _) = toGregorian startDay
        (d2Year, _, _) = toGregorian endDay
        d1YearFraction = (if isLeapYear d1Year then 366
                         else 365) :: Double
    in
      if d1Year == d2Year then fromIntegral(diffDays endDay startDay) / d1YearFraction
      else let
        d2YearFraction = (if isLeapYear d2Year then 366
                         else 365) :: Double
        d1YearLastDay = fromGregorian d1Year 12 31
        firstFractionDays = fromIntegral(diffDays d1YearLastDay startDay)
        secondFractionDays = fromIntegral(diffDays endDay d1YearLastDay)

        in
          (firstFractionDays / d1YearFraction) + (secondFractionDays / d2YearFraction)
  | otherwise = error "Year fraction error: second date must be more recent"

yearFraction DCC_A_360 startDay endDay _
  | startDay <= endDay =
    let daysDiff = fromIntegral(diffDays endDay startDay)
    in
      daysDiff / 360.0
  | otherwise = error "Year fraction error: second date must be more recent"

yearFraction DCC_A_365 startDay endDay _
  | startDay <= endDay =
    let daysDiff = fromIntegral(diffDays endDay startDay)
    in
      daysDiff / 365.0
  | otherwise = error "Year fraction error: second date must be more recent"

yearFraction DCC_E30_360ISDA startDay endDay maturityDate
  | startDay <= endDay =
    let (d1Year, d1Month, d1Day) = toGregorian startDay
        (d2Year, d2Month, d2Day) = toGregorian endDay
        d1ChangedDay = if isLastDayOfMonth d1Year d1Month d1Day then 30
                else d1Day
        d2ChangedDay = if isLastDayOfMonth d2Year d2Month d2Day && not (endDay == maturityDate && d2Month == 2) then 30
                else d2Day
    in
      (360.0 * fromIntegral(d2Year - d1Year) +
       30.0 * fromIntegral(d2Month - d1Month) +
       fromIntegral(d2ChangedDay - d1ChangedDay)) / 360.0
  | otherwise = error "Year fraction error: second date must be more recent"

yearFraction DCC_E30_360 startDay endDay _
  | startDay <= endDay =
    let (d1Year, d1Month, d1Day) = toGregorian startDay
        (d2Year, d2Month, d2Day) = toGregorian endDay
        d1ChangedDay = if d1Day == 31 then 30
                else d1Day
        d2ChangedDay = if d2Day == 31 then 30
                else d2Day
    in
      (360.0 * fromIntegral(d2Year - d1Year) +
       30.0 * fromIntegral(d2Month - d1Month) +
       fromIntegral(d2ChangedDay - d1ChangedDay)) / 360.0
  | otherwise =
    (traceShow "YEAR FRACTION:")
    (traceShow startDay)
    (traceShow endDay)
    error ("Year fraction error: second date must be more recent")

isLastDayOfMonth :: Integer -> Int -> Int -> Bool
isLastDayOfMonth year month day =
  day == gregorianMonthLength year month