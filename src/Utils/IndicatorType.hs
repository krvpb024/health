{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Utils.IndicatorType where

import GHC.Generics
import Data.Aeson
import qualified Data.Text.Lazy as TL
import Data.Range
import Data.Bool

-- Indicator

class Indicator a where
  getIndicatorTitle :: a -> TL.Text
  getIndicatorLevels :: Bool -> Double -> a -> [Level]
  getIndicatorRanges :: Bool -> Double -> a -> [Range Double]

data HealthIndicator = Bmi
                     | BodyFatPercentage
                     | WaistlineCm
                     | Weight
                     deriving (Eq, Show, Generic, ToJSON)

instance Indicator HealthIndicator where
  getIndicatorTitle :: HealthIndicator -> TL.Text
  getIndicatorTitle Bmi               = TL.pack "BMI"
  getIndicatorTitle BodyFatPercentage = TL.pack "體脂率"
  getIndicatorTitle WaistlineCm       = TL.pack "腰團"
  getIndicatorTitle Weight            = TL.pack "體重"

  getIndicatorLevels :: Bool -> Double -> HealthIndicator -> [Level]
  getIndicatorLevels gender age Bmi
    | age >= 18                                   = [BmiUnderWeightAdult .. BmiFatAdult]
    | gender     && age >= 17.5                   = [BmiUnderWeightMale175 .. BmiFatMale175]
    | gender     && age >= 17                     = [BmiUnderWeightMale170 .. BmiFatMale170]
    | gender     && age >= 16.5                   = [BmiUnderWeightMale165 .. BmiFatMale165]
    | gender     && age >= 16                     = [BmiUnderWeightMale160 .. BmiFatMale160]
    | gender     && age >= 15.5                   = [BmiUnderWeightMale155 .. BmiFatMale155]
    | gender     && age >= 15                     = [BmiUnderWeightMale150 .. BmiFatMale150]
    | gender     && age >= 14.5                   = [BmiUnderWeightMale145 .. BmiFatMale145]
    | gender     && age >= 14                     = [BmiUnderWeightMale140 .. BmiFatMale140]
    | gender     && age >= 13.5                   = [BmiUnderWeightMale135 .. BmiFatMale135]
    | gender     && age >= 13                     = [BmiUnderWeightMale130 .. BmiFatMale130]
    | gender     && age >= 12.5                   = [BmiUnderWeightMale125 .. BmiFatMale125]
    | gender     && age >= 12                     = [BmiUnderWeightMale120 .. BmiFatMale120]
    | gender     && age >= 11.5                   = [BmiUnderWeightMale115 .. BmiFatMale115]
    | gender     && age >= 11                     = [BmiUnderWeightMale110 .. BmiFatMale110]
    | gender     && age >= 10.5                   = [BmiUnderWeightMale105 .. BmiFatMale105]
    | gender     && age >= 10                     = [BmiUnderWeightMale100 .. BmiFatMale100]
    | gender     && age >= 9.5                    = [BmiUnderWeightMale95 .. BmiFatMale95]
    | gender     && age >= 9                      = [BmiUnderWeightMale90 .. BmiFatMale90]
    | gender     && age >= 8.5                    = [BmiUnderWeightMale85 .. BmiFatMale85]
    | gender     && age >= 8                      = [BmiUnderWeightMale80 .. BmiFatMale80]
    | gender     && age >= 7.5                    = [BmiUnderWeightMale75 .. BmiFatMale75]
    | gender     && age >= 7                      = [BmiUnderWeightMale70 .. BmiFatMale70]
    | gender     && age >= 6.5                    = [BmiUnderWeightMale65 .. BmiFatMale65]
    | gender     && age >= 6                      = [BmiUnderWeightMale60 .. BmiFatMale60]
    | gender     && age >= 5.5                    = [BmiUnderWeightMale55 .. BmiFatMale55]
    | gender     && age >= 5                      = [BmiUnderWeightMale50 .. BmiFatMale50]
    | gender     && age >= 4.5                    = [BmiUnderWeightMale45 .. BmiFatMale45]
    | gender     && age >= 4                      = [BmiUnderWeightMale40 .. BmiFatMale40]
    | gender     && age >= 3.5                    = [BmiUnderWeightMale35 .. BmiFatMale35]
    | gender     && age >= 3                      = [BmiUnderWeightMale30 .. BmiFatMale30]
    | gender     && age >= 2.5                    = [BmiUnderWeightMale25 .. BmiFatMale25]
    | gender     && age >= 2                      = [BmiUnderWeightMale20 .. BmiFatMale20]
    | gender     && age >= 1.5                    = [BmiUnderWeightMale15 .. BmiFatMale15]
    | gender     && age >= 1                      = [BmiUnderWeightMale10 .. BmiFatMale10]
    | gender     && age >= 0.5                    = [BmiUnderWeightMale5 .. BmiFatMale5]
    | gender     && age >= 0                      = [BmiUnderWeightMale0 .. BmiFatMale0]
    | not gender && age >= 17.5                   = [BmiUnderWeightFemale175 .. BmiFatFemale175]
    | not gender && age >= 17                     = [BmiUnderWeightFemale170 .. BmiFatFemale170]
    | not gender && age >= 16.5                   = [BmiUnderWeightFemale165 .. BmiFatFemale165]
    | not gender && age >= 16                     = [BmiUnderWeightFemale160 .. BmiFatFemale160]
    | not gender && age >= 15.5                   = [BmiUnderWeightFemale155 .. BmiFatFemale155]
    | not gender && age >= 15                     = [BmiUnderWeightFemale150 .. BmiFatFemale150]
    | not gender && age >= 14.5                   = [BmiUnderWeightFemale145 .. BmiFatFemale145]
    | not gender && age >= 14                     = [BmiUnderWeightFemale140 .. BmiFatFemale140]
    | not gender && age >= 13.5                   = [BmiUnderWeightFemale135 .. BmiFatFemale135]
    | not gender && age >= 13                     = [BmiUnderWeightFemale130 .. BmiFatFemale130]
    | not gender && age >= 12.5                   = [BmiUnderWeightFemale125 .. BmiFatFemale125]
    | not gender && age >= 12                     = [BmiUnderWeightFemale120 .. BmiFatFemale120]
    | not gender && age >= 11.5                   = [BmiUnderWeightFemale115 .. BmiFatFemale115]
    | not gender && age >= 11                     = [BmiUnderWeightFemale110 .. BmiFatFemale110]
    | not gender && age >= 10.5                   = [BmiUnderWeightFemale105 .. BmiFatFemale105]
    | not gender && age >= 10                     = [BmiUnderWeightFemale100 .. BmiFatFemale100]
    | not gender && age >= 9.5                    = [BmiUnderWeightFemale95 .. BmiFatFemale95]
    | not gender && age >= 9                      = [BmiUnderWeightFemale90 .. BmiFatFemale90]
    | not gender && age >= 8.5                    = [BmiUnderWeightFemale85 .. BmiFatFemale85]
    | not gender && age >= 8                      = [BmiUnderWeightFemale80 .. BmiFatFemale80]
    | not gender && age >= 7.5                    = [BmiUnderWeightFemale75 .. BmiFatFemale75]
    | not gender && age >= 7                      = [BmiUnderWeightFemale70 .. BmiFatFemale70]
    | not gender && age >= 6.5                    = [BmiUnderWeightFemale65 .. BmiFatFemale65]
    | not gender && age >= 6                      = [BmiUnderWeightFemale60 .. BmiFatFemale60]
    | not gender && age >= 5.5                    = [BmiUnderWeightFemale55 .. BmiFatFemale55]
    | not gender && age >= 5                      = [BmiUnderWeightFemale50 .. BmiFatFemale50]
    | not gender && age >= 4.5                    = [BmiUnderWeightFemale45 .. BmiFatFemale45]
    | not gender && age >= 4                      = [BmiUnderWeightFemale40 .. BmiFatFemale40]
    | not gender && age >= 3.5                    = [BmiUnderWeightFemale35 .. BmiFatFemale35]
    | not gender && age >= 3                      = [BmiUnderWeightFemale30 .. BmiFatFemale30]
    | not gender && age >= 2.5                    = [BmiUnderWeightFemale25 .. BmiFatFemale25]
    | not gender && age >= 2                      = [BmiUnderWeightFemale20 .. BmiFatFemale20]
    | not gender && age >= 1.5                    = [BmiUnderWeightFemale15 .. BmiFatFemale15]
    | not gender && age >= 1                      = [BmiUnderWeightFemale10 .. BmiFatFemale10]
    | not gender && age >= 0.5                    = [BmiUnderWeightFemale5 .. BmiFatFemale5]
    | not gender && age >= 0                      = [BmiUnderWeightFemale0 .. BmiFatFemale0]
    | otherwise                                   = []
  getIndicatorLevels gender _   BodyFatPercentage = bool [BodyFatPercentageNormalFemale]
                                                         [BodyFatPercentageNormalMale]
                                                         gender
  getIndicatorLevels gender _   WaistlineCm       = bool [WaistlineCmNormalFemale]
                                                         [WaistlineCmNormalMale]
                                                         gender
  getIndicatorLevels _      _   Weight            = []

  getIndicatorRanges :: Bool -> Double -> HealthIndicator -> [Range Double]
  getIndicatorRanges gender age = fmap getLvRange . getIndicatorLevels gender age

-- Level

class Lv a where
  getLvTitle :: a -> TL.Text
  getLvRange :: a -> Range Double
  getLvColor :: a -> TL.Text

data Level = BmiUnderWeightMale0
           | BmiNormalMale0
           | BmiOverWeightMale0
           | BmiFatMale0
           | BmiUnderWeightFemale0
           | BmiNormalFemale0
           | BmiOverWeightFemale0
           | BmiFatFemale0
           | BmiUnderWeightMale5
           | BmiNormalMale5
           | BmiOverWeightMale5
           | BmiFatMale5
           | BmiUnderWeightFemale5
           | BmiNormalFemale5
           | BmiOverWeightFemale5
           | BmiFatFemale5
           | BmiUnderWeightMale10
           | BmiNormalMale10
           | BmiOverWeightMale10
           | BmiFatMale10
           | BmiUnderWeightFemale10
           | BmiNormalFemale10
           | BmiOverWeightFemale10
           | BmiFatFemale10
           | BmiUnderWeightMale15
           | BmiNormalMale15
           | BmiOverWeightMale15
           | BmiFatMale15
           | BmiUnderWeightFemale15
           | BmiNormalFemale15
           | BmiOverWeightFemale15
           | BmiFatFemale15
           | BmiUnderWeightMale20
           | BmiNormalMale20
           | BmiOverWeightMale20
           | BmiFatMale20
           | BmiUnderWeightFemale20
           | BmiNormalFemale20
           | BmiOverWeightFemale20
           | BmiFatFemale20
           | BmiUnderWeightMale25
           | BmiNormalMale25
           | BmiOverWeightMale25
           | BmiFatMale25
           | BmiUnderWeightFemale25
           | BmiNormalFemale25
           | BmiOverWeightFemale25
           | BmiFatFemale25
           | BmiUnderWeightMale30
           | BmiNormalMale30
           | BmiOverWeightMale30
           | BmiFatMale30
           | BmiUnderWeightFemale30
           | BmiNormalFemale30
           | BmiOverWeightFemale30
           | BmiFatFemale30
           | BmiUnderWeightMale35
           | BmiNormalMale35
           | BmiOverWeightMale35
           | BmiFatMale35
           | BmiUnderWeightFemale35
           | BmiNormalFemale35
           | BmiOverWeightFemale35
           | BmiFatFemale35
           | BmiUnderWeightMale40
           | BmiNormalMale40
           | BmiOverWeightMale40
           | BmiFatMale40
           | BmiUnderWeightFemale40
           | BmiNormalFemale40
           | BmiOverWeightFemale40
           | BmiFatFemale40
           | BmiUnderWeightMale45
           | BmiNormalMale45
           | BmiOverWeightMale45
           | BmiFatMale45
           | BmiUnderWeightFemale45
           | BmiNormalFemale45
           | BmiOverWeightFemale45
           | BmiFatFemale45
           | BmiUnderWeightMale50
           | BmiNormalMale50
           | BmiOverWeightMale50
           | BmiFatMale50
           | BmiUnderWeightFemale50
           | BmiNormalFemale50
           | BmiOverWeightFemale50
           | BmiFatFemale50
           | BmiUnderWeightMale55
           | BmiNormalMale55
           | BmiOverWeightMale55
           | BmiFatMale55
           | BmiUnderWeightFemale55
           | BmiNormalFemale55
           | BmiOverWeightFemale55
           | BmiFatFemale55
           | BmiUnderWeightMale60
           | BmiNormalMale60
           | BmiOverWeightMale60
           | BmiFatMale60
           | BmiUnderWeightFemale60
           | BmiNormalFemale60
           | BmiOverWeightFemale60
           | BmiFatFemale60
           | BmiUnderWeightMale65
           | BmiNormalMale65
           | BmiOverWeightMale65
           | BmiFatMale65
           | BmiUnderWeightFemale65
           | BmiNormalFemale65
           | BmiOverWeightFemale65
           | BmiFatFemale65
           | BmiUnderWeightMale70
           | BmiNormalMale70
           | BmiOverWeightMale70
           | BmiFatMale70
           | BmiUnderWeightFemale70
           | BmiNormalFemale70
           | BmiOverWeightFemale70
           | BmiFatFemale70
           | BmiUnderWeightMale75
           | BmiNormalMale75
           | BmiOverWeightMale75
           | BmiFatMale75
           | BmiUnderWeightFemale75
           | BmiNormalFemale75
           | BmiOverWeightFemale75
           | BmiFatFemale75
           | BmiUnderWeightMale80
           | BmiNormalMale80
           | BmiOverWeightMale80
           | BmiFatMale80
           | BmiUnderWeightFemale80
           | BmiNormalFemale80
           | BmiOverWeightFemale80
           | BmiFatFemale80
           | BmiUnderWeightMale85
           | BmiNormalMale85
           | BmiOverWeightMale85
           | BmiFatMale85
           | BmiUnderWeightFemale85
           | BmiNormalFemale85
           | BmiOverWeightFemale85
           | BmiFatFemale85
           | BmiUnderWeightMale90
           | BmiNormalMale90
           | BmiOverWeightMale90
           | BmiFatMale90
           | BmiUnderWeightFemale90
           | BmiNormalFemale90
           | BmiOverWeightFemale90
           | BmiFatFemale90
           | BmiUnderWeightMale95
           | BmiNormalMale95
           | BmiOverWeightMale95
           | BmiFatMale95
           | BmiUnderWeightFemale95
           | BmiNormalFemale95
           | BmiOverWeightFemale95
           | BmiFatFemale95
           | BmiUnderWeightMale100
           | BmiNormalMale100
           | BmiOverWeightMale100
           | BmiFatMale100
           | BmiUnderWeightFemale100
           | BmiNormalFemale100
           | BmiOverWeightFemale100
           | BmiFatFemale100
           | BmiUnderWeightMale105
           | BmiNormalMale105
           | BmiOverWeightMale105
           | BmiFatMale105
           | BmiUnderWeightFemale105
           | BmiNormalFemale105
           | BmiOverWeightFemale105
           | BmiFatFemale105
           | BmiUnderWeightMale110
           | BmiNormalMale110
           | BmiOverWeightMale110
           | BmiFatMale110
           | BmiUnderWeightFemale110
           | BmiNormalFemale110
           | BmiOverWeightFemale110
           | BmiFatFemale110
           | BmiUnderWeightMale115
           | BmiNormalMale115
           | BmiOverWeightMale115
           | BmiFatMale115
           | BmiUnderWeightFemale115
           | BmiNormalFemale115
           | BmiOverWeightFemale115
           | BmiFatFemale115
           | BmiUnderWeightMale120
           | BmiNormalMale120
           | BmiOverWeightMale120
           | BmiFatMale120
           | BmiUnderWeightFemale120
           | BmiNormalFemale120
           | BmiOverWeightFemale120
           | BmiFatFemale120
           | BmiUnderWeightMale125
           | BmiNormalMale125
           | BmiOverWeightMale125
           | BmiFatMale125
           | BmiUnderWeightFemale125
           | BmiNormalFemale125
           | BmiOverWeightFemale125
           | BmiFatFemale125
           | BmiUnderWeightMale130
           | BmiNormalMale130
           | BmiOverWeightMale130
           | BmiFatMale130
           | BmiUnderWeightFemale130
           | BmiNormalFemale130
           | BmiOverWeightFemale130
           | BmiFatFemale130
           | BmiUnderWeightMale135
           | BmiNormalMale135
           | BmiOverWeightMale135
           | BmiFatMale135
           | BmiUnderWeightFemale135
           | BmiNormalFemale135
           | BmiOverWeightFemale135
           | BmiFatFemale135
           | BmiUnderWeightMale140
           | BmiNormalMale140
           | BmiOverWeightMale140
           | BmiFatMale140
           | BmiUnderWeightFemale140
           | BmiNormalFemale140
           | BmiOverWeightFemale140
           | BmiFatFemale140
           | BmiUnderWeightMale145
           | BmiNormalMale145
           | BmiOverWeightMale145
           | BmiFatMale145
           | BmiUnderWeightFemale145
           | BmiNormalFemale145
           | BmiOverWeightFemale145
           | BmiFatFemale145
           | BmiUnderWeightMale150
           | BmiNormalMale150
           | BmiOverWeightMale150
           | BmiFatMale150
           | BmiUnderWeightFemale150
           | BmiNormalFemale150
           | BmiOverWeightFemale150
           | BmiFatFemale150
           | BmiUnderWeightMale155
           | BmiNormalMale155
           | BmiOverWeightMale155
           | BmiFatMale155
           | BmiUnderWeightFemale155
           | BmiNormalFemale155
           | BmiOverWeightFemale155
           | BmiFatFemale155
           | BmiUnderWeightMale160
           | BmiNormalMale160
           | BmiOverWeightMale160
           | BmiFatMale160
           | BmiUnderWeightFemale160
           | BmiNormalFemale160
           | BmiOverWeightFemale160
           | BmiFatFemale160
           | BmiUnderWeightMale165
           | BmiNormalMale165
           | BmiOverWeightMale165
           | BmiFatMale165
           | BmiUnderWeightFemale165
           | BmiNormalFemale165
           | BmiOverWeightFemale165
           | BmiFatFemale165
           | BmiUnderWeightMale170
           | BmiNormalMale170
           | BmiOverWeightMale170
           | BmiFatMale170
           | BmiUnderWeightFemale170
           | BmiNormalFemale170
           | BmiOverWeightFemale170
           | BmiFatFemale170
           | BmiUnderWeightMale175
           | BmiNormalMale175
           | BmiOverWeightMale175
           | BmiFatMale175
           | BmiUnderWeightFemale175
           | BmiNormalFemale175
           | BmiOverWeightFemale175
           | BmiFatFemale175
           | BmiUnderWeightAdult
           | BmiNormalAdult
           | BmiOverWeightAdult
           | BmiFatAdult
           -- BodyFatPercentage
           | BodyFatPercentageNormalMale
           | BodyFatPercentageNormalFemale
           -- WaistlineCm
           | WaistlineCmNormalMale
           | WaistlineCmNormalFemale
           deriving (Eq, Show, Generic, Enum, Bounded, ToJSON)

underWeight = TL.pack "過輕"
normal = TL.pack "正常"
overWeight = TL.pack "過重"
fat = TL.pack "肥胖"

underWeightColor = TL.pack "#dadada"
normalColor = TL.pack "#c6c6c6"
overWeightColor = TL.pack "#b7b7b7"
fatColor = TL.pack "#ababab"

instance Lv Level where
  getLvTitle :: Level -> TL.Text
  getLvTitle l
    | l `elem` [BmiUnderWeightMale0, BmiUnderWeightFemale0, BmiUnderWeightMale5, BmiUnderWeightFemale5, BmiUnderWeightMale10, BmiUnderWeightFemale10, BmiUnderWeightMale15, BmiUnderWeightFemale15, BmiUnderWeightMale20, BmiUnderWeightFemale20, BmiUnderWeightMale25, BmiUnderWeightFemale25, BmiUnderWeightMale30, BmiUnderWeightFemale30, BmiUnderWeightMale35, BmiUnderWeightFemale35, BmiUnderWeightMale40, BmiUnderWeightFemale40, BmiUnderWeightMale45, BmiUnderWeightFemale45, BmiUnderWeightMale50, BmiUnderWeightFemale50, BmiUnderWeightMale55, BmiUnderWeightFemale55, BmiUnderWeightMale60, BmiUnderWeightFemale60, BmiUnderWeightMale65, BmiUnderWeightFemale65, BmiUnderWeightMale70, BmiUnderWeightFemale70, BmiUnderWeightMale75, BmiUnderWeightFemale75, BmiUnderWeightMale80, BmiUnderWeightFemale80, BmiUnderWeightMale85, BmiUnderWeightFemale85, BmiUnderWeightMale90, BmiUnderWeightFemale90, BmiUnderWeightMale95, BmiUnderWeightFemale95, BmiUnderWeightMale100, BmiUnderWeightFemale100, BmiUnderWeightMale105, BmiUnderWeightFemale105, BmiUnderWeightMale110, BmiUnderWeightFemale110, BmiUnderWeightMale115, BmiUnderWeightFemale115, BmiUnderWeightMale120, BmiUnderWeightFemale120, BmiUnderWeightMale125, BmiUnderWeightFemale125, BmiUnderWeightMale130, BmiUnderWeightFemale130, BmiUnderWeightMale135, BmiUnderWeightFemale135, BmiUnderWeightMale140, BmiUnderWeightFemale140, BmiUnderWeightMale145, BmiUnderWeightFemale145, BmiUnderWeightMale150, BmiUnderWeightFemale150, BmiUnderWeightMale155, BmiUnderWeightFemale155, BmiUnderWeightMale160, BmiUnderWeightFemale160, BmiUnderWeightMale165, BmiUnderWeightFemale165, BmiUnderWeightMale170, BmiUnderWeightFemale170, BmiUnderWeightMale175, BmiUnderWeightFemale175, BmiUnderWeightAdult] = underWeight
    | l `elem` [BmiOverWeightMale0, BmiOverWeightFemale0, BmiOverWeightMale5, BmiOverWeightFemale5, BmiOverWeightMale10, BmiOverWeightFemale10, BmiOverWeightMale15, BmiOverWeightFemale15, BmiOverWeightMale20, BmiOverWeightFemale20, BmiOverWeightMale25, BmiOverWeightFemale25, BmiOverWeightMale30, BmiOverWeightFemale30, BmiOverWeightMale35, BmiOverWeightFemale35, BmiOverWeightMale40, BmiOverWeightFemale40, BmiOverWeightMale45, BmiOverWeightFemale45, BmiOverWeightMale50, BmiOverWeightFemale50, BmiOverWeightMale55, BmiOverWeightFemale55, BmiOverWeightMale60, BmiOverWeightFemale60, BmiOverWeightMale65, BmiOverWeightFemale65, BmiOverWeightMale70, BmiOverWeightFemale70, BmiOverWeightMale75, BmiOverWeightFemale75, BmiOverWeightMale80, BmiOverWeightFemale80, BmiOverWeightMale85, BmiOverWeightFemale85, BmiOverWeightMale90, BmiOverWeightFemale90, BmiOverWeightMale95, BmiOverWeightFemale95, BmiOverWeightMale100, BmiOverWeightFemale100, BmiOverWeightMale105, BmiOverWeightFemale105, BmiOverWeightMale110, BmiOverWeightFemale110, BmiOverWeightMale115, BmiOverWeightFemale115, BmiOverWeightMale120, BmiOverWeightFemale120, BmiOverWeightMale125, BmiOverWeightFemale125, BmiOverWeightMale130, BmiOverWeightFemale130, BmiOverWeightMale135, BmiOverWeightFemale135, BmiOverWeightMale140, BmiOverWeightFemale140, BmiOverWeightMale145, BmiOverWeightFemale145, BmiOverWeightMale150, BmiOverWeightFemale150, BmiOverWeightMale155, BmiOverWeightFemale155, BmiOverWeightMale160, BmiOverWeightFemale160, BmiOverWeightMale165, BmiOverWeightFemale165, BmiOverWeightMale170, BmiOverWeightFemale170, BmiOverWeightMale175, BmiOverWeightFemale175, BmiOverWeightAdult] = overWeight
    | l `elem` [BmiFatMale0, BmiFatFemale0, BmiFatMale5, BmiFatFemale5, BmiFatMale10, BmiFatFemale10, BmiFatMale15, BmiFatFemale15, BmiFatMale20, BmiFatFemale20, BmiFatMale25, BmiFatFemale25, BmiFatMale30, BmiFatFemale30, BmiFatMale35, BmiFatFemale35, BmiFatMale40, BmiFatFemale40, BmiFatMale45, BmiFatFemale45, BmiFatMale50, BmiFatFemale50, BmiFatMale55, BmiFatFemale55, BmiFatMale60, BmiFatFemale60, BmiFatMale65, BmiFatFemale65, BmiFatMale70, BmiFatFemale70, BmiFatMale75, BmiFatFemale75, BmiFatMale80, BmiFatFemale80, BmiFatMale85, BmiFatFemale85, BmiFatMale90, BmiFatFemale90, BmiFatMale95, BmiFatFemale95, BmiFatMale100, BmiFatFemale100, BmiFatMale105, BmiFatFemale105, BmiFatMale110, BmiFatFemale110, BmiFatMale115, BmiFatFemale115, BmiFatMale120, BmiFatFemale120, BmiFatMale125, BmiFatFemale125, BmiFatMale130, BmiFatFemale130, BmiFatMale135, BmiFatFemale135, BmiFatMale140, BmiFatFemale140, BmiFatMale145, BmiFatFemale145, BmiFatMale150, BmiFatFemale150, BmiFatMale155, BmiFatFemale155, BmiFatMale160, BmiFatFemale160, BmiFatMale165, BmiFatFemale165, BmiFatMale170, BmiFatFemale170, BmiFatMale175, BmiFatFemale175, BmiFatAdult] = fat
    | otherwise = normal

  getLvRange :: Level -> Range Double
  getLvRange BmiUnderWeightMale0           = ube 11.5
  getLvRange BmiNormalMale0                = 11.5 +=* 14.8
  getLvRange BmiOverWeightMale0            = 14.8 +=* 15.8
  getLvRange BmiFatMale0                   = lbi 15.8
  getLvRange BmiUnderWeightFemale0         = ube 11.5
  getLvRange BmiNormalFemale0              = 11.5 +=* 14.7
  getLvRange BmiOverWeightFemale0          = 14.7 +=* 15.5
  getLvRange BmiFatFemale0                 = lbi 15.5
  getLvRange BmiUnderWeightMale5           = ube 15.2
  getLvRange BmiNormalMale5                = 15.2 +=* 18.9
  getLvRange BmiOverWeightMale5            = 18.9 +=* 19.9
  getLvRange BmiFatMale5                   = lbi 19.9
  getLvRange BmiUnderWeightFemale5         = ube 14.6
  getLvRange BmiNormalFemale5              = 14.6 +=* 18.6
  getLvRange BmiOverWeightFemale5          = 18.8 +=* 19.6
  getLvRange BmiFatFemale5                 = lbi 19.6
  getLvRange BmiUnderWeightMale10          = ube 14.8
  getLvRange BmiNormalMale10               = 14.8 +=* 18.3
  getLvRange BmiOverWeightMale10           = 18.3 +=* 19.2
  getLvRange BmiFatMale10                  = lbi 19.2
  getLvRange BmiUnderWeightFemale10        = ube 14.2
  getLvRange BmiNormalFemale10             = 14.2 +=* 17.9
  getLvRange BmiOverWeightFemale10         = 17.9 +=* 19
  getLvRange BmiFatFemale10                = lbi 19
  getLvRange BmiUnderWeightMale15          = ube 14.2
  getLvRange BmiNormalMale15               = 14.2 +=* 17.5
  getLvRange BmiOverWeightMale15           = 17.5 +=* 18.5
  getLvRange BmiFatMale15                  = lbi 18.5
  getLvRange BmiUnderWeightFemale15        = ube 13.7
  getLvRange BmiNormalFemale15             = 13.7 +=* 17.2
  getLvRange BmiOverWeightFemale15         = 17.2 +=* 18.2
  getLvRange BmiFatFemale15                = lbi 18.2
  getLvRange BmiUnderWeightMale20          = ube 14.2
  getLvRange BmiNormalMale20               = 14.2 +=* 17.4
  getLvRange BmiOverWeightMale20           = 17.4 +=* 18.3
  getLvRange BmiFatMale20                  = lbi 18.3
  getLvRange BmiUnderWeightFemale20        = ube 13.7
  getLvRange BmiNormalFemale20             = 13.7 +=* 17.2
  getLvRange BmiOverWeightFemale20         = 17.2 +=* 18.1
  getLvRange BmiFatFemale20                = lbi 18.1
  getLvRange BmiUnderWeightMale25          = ube 13.9
  getLvRange BmiNormalMale25               = 13.9 +=* 17.2
  getLvRange BmiOverWeightMale25           = 17.2 +=* 18
  getLvRange BmiFatMale25                  = lbi 18
  getLvRange BmiUnderWeightFemale25        = ube 13.6
  getLvRange BmiNormalFemale25             = 13.6 +=* 17
  getLvRange BmiOverWeightFemale25         = 17 +=* 17.9
  getLvRange BmiFatFemale25                = lbi 17.9
  getLvRange BmiUnderWeightMale30          = ube 13.7
  getLvRange BmiNormalMale30               = 13.7 +=* 17
  getLvRange BmiOverWeightMale30           = 17 +=* 17.8
  getLvRange BmiFatMale30                  = lbi 17.8
  getLvRange BmiUnderWeightFemale30        = ube 13.5
  getLvRange BmiNormalFemale30             = 13.5 +=* 16.9
  getLvRange BmiOverWeightFemale30         = 16.9 +=* 17.8
  getLvRange BmiFatFemale30                = lbi 17.8
  getLvRange BmiUnderWeightMale35          = ube 13.6
  getLvRange BmiNormalMale35               = 13.6 +=* 16.8
  getLvRange BmiOverWeightMale35           = 16.8 +=* 17.7
  getLvRange BmiFatMale35                  = lbi 17.7
  getLvRange BmiUnderWeightFemale35        = ube 13.3
  getLvRange BmiNormalFemale35             = 13.3 +=* 16.9
  getLvRange BmiOverWeightFemale35         = 16.9 +=* 17.8
  getLvRange BmiFatFemale35                = lbi 17.8
  getLvRange BmiUnderWeightMale40          = ube 13.4
  getLvRange BmiNormalMale40               = 13.4 +=* 16.7
  getLvRange BmiOverWeightMale40           = 16.7 +=* 17.6
  getLvRange BmiFatMale40                  = lbi 17.6
  getLvRange BmiUnderWeightFemale40        = ube 13.2
  getLvRange BmiNormalFemale40             = 13.2 +=* 16.8
  getLvRange BmiOverWeightFemale40         = 16.8 +=* 17.9
  getLvRange BmiFatFemale40                = lbi 17.9
  getLvRange BmiUnderWeightMale45          = ube 13.3
  getLvRange BmiNormalMale45               = 13.3 +=* 16.7
  getLvRange BmiOverWeightMale45           = 16.7 +=* 17.6
  getLvRange BmiFatMale45                  = lbi 17.6
  getLvRange BmiUnderWeightFemale45        = ube 13.1
  getLvRange BmiNormalFemale45             = 13.1 +=* 16.9
  getLvRange BmiOverWeightFemale45         = 16.9 +=* 18
  getLvRange BmiFatFemale45                = lbi 18
  getLvRange BmiUnderWeightMale50          = ube 13.3
  getLvRange BmiNormalMale50               = 13.3 +=* 16.7
  getLvRange BmiOverWeightMale50           = 16.7 +=* 17.7
  getLvRange BmiFatMale50                  = lbi 17.7
  getLvRange BmiUnderWeightFemale50        = ube 13.1
  getLvRange BmiNormalFemale50             = 13.1 +=* 17
  getLvRange BmiOverWeightFemale50         = 17 +=* 18.1
  getLvRange BmiFatFemale50                = lbi 18.1
  getLvRange BmiUnderWeightMale55          = ube 13.4
  getLvRange BmiNormalMale55               = 13.4 +=* 16.7
  getLvRange BmiOverWeightMale55           = 16.7 +=* 18
  getLvRange BmiFatMale55                  = lbi 18
  getLvRange BmiUnderWeightFemale55        = ube 13.1
  getLvRange BmiNormalFemale55             = 13.1 +=* 17
  getLvRange BmiOverWeightFemale55         = 17 +=* 18.3
  getLvRange BmiFatFemale55                = lbi 18.3
  getLvRange BmiUnderWeightMale60          = ube 13.5
  getLvRange BmiNormalMale60               = 13.5 +=* 16.9
  getLvRange BmiOverWeightMale60           = 16.9 +=* 18.5
  getLvRange BmiFatMale60                  = lbi 18.5
  getLvRange BmiUnderWeightFemale60        = ube 13.1
  getLvRange BmiNormalFemale60             = 13.1 +=* 17.2
  getLvRange BmiOverWeightFemale60         = 17.2 +=* 18.8
  getLvRange BmiFatFemale60                = lbi 18.8
  getLvRange BmiUnderWeightMale65          = ube 13.6
  getLvRange BmiNormalMale65               = 13.6 +=* 17.3
  getLvRange BmiOverWeightMale65           = 17.3 +=* 19.2
  getLvRange BmiFatMale65                  = lbi 19.2
  getLvRange BmiUnderWeightFemale65        = ube 13.2
  getLvRange BmiNormalFemale65             = 13.2 +=* 17.5
  getLvRange BmiOverWeightFemale65         = 17.5 +=* 19.2
  getLvRange BmiFatFemale65                = lbi 19.2
  getLvRange BmiUnderWeightMale70          = ube 13.8
  getLvRange BmiNormalMale70               = 13.8 +=* 17.9
  getLvRange BmiOverWeightMale70           = 17.9 +=* 20.3
  getLvRange BmiFatMale70                  = lbi 20.3
  getLvRange BmiUnderWeightFemale70        = ube 13.4
  getLvRange BmiNormalFemale70             = 13.4 +=* 17.7
  getLvRange BmiOverWeightFemale70         = 17.7 +=* 19.6
  getLvRange BmiFatFemale70                = lbi 19.6
  getLvRange BmiUnderWeightMale75          = ube 14
  getLvRange BmiNormalMale75               = 14 +=* 18.6
  getLvRange BmiOverWeightMale75           = 18.6 +=* 21.2
  getLvRange BmiFatMale75                  = lbi 21.2
  getLvRange BmiUnderWeightFemale75        = ube 13.7
  getLvRange BmiNormalFemale75             = 13.7 +=* 18
  getLvRange BmiOverWeightFemale75         = 18 +=* 20.3
  getLvRange BmiFatFemale75                = lbi 20.3
  getLvRange BmiUnderWeightMale80          = ube 14.1
  getLvRange BmiNormalMale80               = 14.1 +=* 19
  getLvRange BmiOverWeightMale80           = 19 +=* 21.6
  getLvRange BmiFatMale80                  = lbi 21.6
  getLvRange BmiUnderWeightFemale80        = ube 13.8
  getLvRange BmiNormalFemale80             = 13.8 +=* 18.4
  getLvRange BmiOverWeightFemale80         = 18.4 +=* 20.7
  getLvRange BmiFatFemale80                = lbi 20.7
  getLvRange BmiUnderWeightMale85          = ube 14.2
  getLvRange BmiNormalMale85               = 14.2 +=* 19.3
  getLvRange BmiOverWeightMale85           = 19.3 +=* 22
  getLvRange BmiFatMale85                  = lbi 22
  getLvRange BmiUnderWeightFemale85        = ube 13.9
  getLvRange BmiNormalFemale85             = 13.9 +=* 18.8
  getLvRange BmiOverWeightFemale85         = 18.8 +=* 21
  getLvRange BmiFatFemale85                = lbi 21
  getLvRange BmiUnderWeightMale90          = ube 14.3
  getLvRange BmiNormalMale90               = 14.3 +=* 19.5
  getLvRange BmiOverWeightMale90           = 19.5 +=* 22.3
  getLvRange BmiFatMale90                  = lbi 22.3
  getLvRange BmiUnderWeightFemale90        = ube 14
  getLvRange BmiNormalFemale90             = 14 +=* 19.1
  getLvRange BmiOverWeightFemale90         = 19.1 +=* 21.3
  getLvRange BmiFatFemale90                = lbi 21.3
  getLvRange BmiUnderWeightMale95          = ube 14.4
  getLvRange BmiNormalMale95               = 14.4 +=* 19.7
  getLvRange BmiOverWeightMale95           = 19.7 +=* 22.5
  getLvRange BmiFatMale95                  = lbi 22.5
  getLvRange BmiUnderWeightFemale95        = ube 14.1
  getLvRange BmiNormalFemale95             = 14.1 +=* 19.3
  getLvRange BmiOverWeightFemale95         = 19.3 +=* 21.6
  getLvRange BmiFatFemale95                = lbi 21.6
  getLvRange BmiUnderWeightMale100         = ube 14.5
  getLvRange BmiNormalMale100              = 14.2 +=* 20
  getLvRange BmiOverWeightMale100          = 20 +=* 22.7
  getLvRange BmiFatMale100                 = lbi 22.7
  getLvRange BmiUnderWeightFemale100       = ube 14.3
  getLvRange BmiNormalFemale100            = 14.3 +=* 19.7
  getLvRange BmiOverWeightFemale100        = 19.7 +=* 22
  getLvRange BmiFatFemale100               = lbi 22
  getLvRange BmiUnderWeightMale105         = ube 14.6
  getLvRange BmiNormalMale105              = 14.6 +=* 20.3
  getLvRange BmiOverWeightMale105          = 20.3 +=* 22.9
  getLvRange BmiFatMale105                 = lbi 22.9
  getLvRange BmiUnderWeightFemale105       = ube 14.4
  getLvRange BmiNormalFemale105            = 14.4 +=* 20.1
  getLvRange BmiOverWeightFemale105        = 20.1 +=* 22.3
  getLvRange BmiFatFemale105               = lbi 22.3
  getLvRange BmiUnderWeightMale110         = ube 14.8
  getLvRange BmiNormalMale110              = 14.8 +=* 20.7
  getLvRange BmiOverWeightMale110          = 20.7 +=* 23.2
  getLvRange BmiFatMale110                 = lbi 23.2
  getLvRange BmiUnderWeightFemale110       = ube 14.7
  getLvRange BmiNormalFemale110            = 14.7 +=* 20.5
  getLvRange BmiOverWeightFemale110        = 20.5 +=* 22.7
  getLvRange BmiFatFemale110               = lbi 22.7
  getLvRange BmiUnderWeightMale115         = ube 15
  getLvRange BmiNormalMale115              = 15 +=* 21
  getLvRange BmiOverWeightMale115          = 21 +=* 23.5
  getLvRange BmiFatMale115                 = lbi 23.5
  getLvRange BmiUnderWeightFemale115       = ube 14.9
  getLvRange BmiNormalFemale115            = 14.9 +=* 20.9
  getLvRange BmiOverWeightFemale115        = 20.9 +=* 23.1
  getLvRange BmiFatFemale115               = lbi 23.1
  getLvRange BmiUnderWeightMale120         = ube 15.2
  getLvRange BmiNormalMale120              = 15.2 +=* 21.3
  getLvRange BmiOverWeightMale120          = 21.3 +=* 23.9
  getLvRange BmiFatMale120                 = lbi 23.9
  getLvRange BmiUnderWeightFemale120       = ube 15.2
  getLvRange BmiNormalFemale120            = 15.2 +=* 21.3
  getLvRange BmiOverWeightFemale120        = 21.3 +=* 23.5
  getLvRange BmiFatFemale120               = lbi 23.5
  getLvRange BmiUnderWeightMale125         = ube 15.4
  getLvRange BmiNormalMale125              = 15.4 +=* 21.5
  getLvRange BmiOverWeightMale125          = 21.5 +=* 24.2
  getLvRange BmiFatMale125                 = lbi 24.2
  getLvRange BmiUnderWeightFemale125       = ube 15.4
  getLvRange BmiNormalFemale125            = 15.4 +=* 21.6
  getLvRange BmiOverWeightFemale125        = 21.6 +=* 23.9
  getLvRange BmiFatFemale125               = lbi 23.9
  getLvRange BmiUnderWeightMale130         = ube 15.7
  getLvRange BmiNormalMale130              = 15.7 +=* 21.9
  getLvRange BmiOverWeightMale130          = 21.9 +=* 24.5
  getLvRange BmiFatMale130                 = lbi 24.5
  getLvRange BmiUnderWeightFemale130       = ube 15.7
  getLvRange BmiNormalFemale130            = 15.7 +=* 21.9
  getLvRange BmiOverWeightFemale130        = 21.9 +=* 24.3
  getLvRange BmiFatFemale130               = lbi 24.3
  getLvRange BmiUnderWeightMale135         = ube 16
  getLvRange BmiNormalMale135              = 16 +=* 22.2
  getLvRange BmiOverWeightMale135          = 22.2 +=* 24.8
  getLvRange BmiFatMale135                 = lbi 24.8
  getLvRange BmiUnderWeightFemale135       = ube 16
  getLvRange BmiNormalFemale135            = 16 +=* 22.2
  getLvRange BmiOverWeightFemale135        = 22.2 +=* 24.6
  getLvRange BmiFatFemale135               = lbi 24.6
  getLvRange BmiUnderWeightMale140         = ube 16.3
  getLvRange BmiNormalMale140              = 16.3 +=* 22.5
  getLvRange BmiOverWeightMale140          = 22.5 +=* 25
  getLvRange BmiFatMale140                 = lbi 25
  getLvRange BmiUnderWeightFemale140       = ube 16.3
  getLvRange BmiNormalFemale140            = 16.3 +=* 22.5
  getLvRange BmiOverWeightFemale140        = 22.5 +=* 24.9
  getLvRange BmiFatFemale140               = lbi 24.9
  getLvRange BmiUnderWeightMale145         = ube 16.6
  getLvRange BmiNormalMale145              = 16.6 +=* 22.7
  getLvRange BmiOverWeightMale145          = 22.7 +=* 25.2
  getLvRange BmiFatMale145                 = lbi 25.2
  getLvRange BmiUnderWeightFemale145       = ube 16.5
  getLvRange BmiNormalFemale145            = 16.5 +=* 22.7
  getLvRange BmiOverWeightFemale145        = 22.7 +=* 25.1
  getLvRange BmiFatFemale145               = lbi 25.1
  getLvRange BmiUnderWeightMale150         = ube 16.9
  getLvRange BmiNormalMale150              = 16.9 +=* 22.9
  getLvRange BmiOverWeightMale150          = 22.9 +=* 25.4
  getLvRange BmiFatMale150                 = lbi 25.4
  getLvRange BmiUnderWeightFemale150       = ube 16.7
  getLvRange BmiNormalFemale150            = 16.7 +=* 22.7
  getLvRange BmiOverWeightFemale150        = 22.7 +=* 25.2
  getLvRange BmiFatFemale150               = lbi 25.2
  getLvRange BmiUnderWeightMale155         = ube 17.2
  getLvRange BmiNormalMale155              = 17.2 +=* 23.1
  getLvRange BmiOverWeightMale155          = 23.1 +=* 25.5
  getLvRange BmiFatMale155                 = lbi 25.5
  getLvRange BmiUnderWeightFemale155       = ube 16.9
  getLvRange BmiNormalFemale155            = 16.9 +=* 22.7
  getLvRange BmiOverWeightFemale155        = 22.7 +=* 25.3
  getLvRange BmiFatFemale155               = lbi 25.3
  getLvRange BmiUnderWeightMale160         = ube 17.4
  getLvRange BmiNormalMale160              = 17.4 +=* 23.3
  getLvRange BmiOverWeightMale160          = 23.3 +=* 25.6
  getLvRange BmiFatMale160                 = lbi 25.6
  getLvRange BmiUnderWeightFemale160       = ube 17.1
  getLvRange BmiNormalFemale160            = 17.1 +=* 22.7
  getLvRange BmiOverWeightFemale160        = 22.7 +=* 25.3
  getLvRange BmiFatFemale160               = lbi 25.3
  getLvRange BmiUnderWeightMale165         = ube 17.6
  getLvRange BmiNormalMale165              = 17.6 +=* 23.4
  getLvRange BmiOverWeightMale165          = 23.4 +=* 25.6
  getLvRange BmiFatMale165                 = lbi 25.6
  getLvRange BmiUnderWeightFemale165       = ube 17.2
  getLvRange BmiNormalFemale165            = 17.2 +=* 22.7
  getLvRange BmiOverWeightFemale165        = 22.7 +=* 25.3
  getLvRange BmiFatFemale165               = lbi 25.3
  getLvRange BmiUnderWeightMale170         = ube 17.8
  getLvRange BmiNormalMale170              = 17.8 +=* 23.5
  getLvRange BmiOverWeightMale170          = 23.5 +=* 25.6
  getLvRange BmiFatMale170                 = lbi 25.6
  getLvRange BmiUnderWeightFemale170       = ube 17.3
  getLvRange BmiNormalFemale170            = 17.3 +=* 22.7
  getLvRange BmiOverWeightFemale170        = 22.7 +=* 25.3
  getLvRange BmiFatFemale170               = lbi 25.3
  getLvRange BmiUnderWeightMale175         = ube 18
  getLvRange BmiNormalMale175              = 18 +=* 23.6
  getLvRange BmiOverWeightMale175          = 23.6 +=* 25.6
  getLvRange BmiFatMale175                 = lbi 25.6
  getLvRange BmiUnderWeightFemale175       = ube 17.3
  getLvRange BmiNormalFemale175            = 17.3 +=* 22.7
  getLvRange BmiOverWeightFemale175        = 22.7 +=* 25.3
  getLvRange BmiFatFemale175               = lbi 25.3
  getLvRange BmiUnderWeightAdult           = ube 18.5
  getLvRange BmiNormalAdult                = 18.5 +=* 24
  getLvRange BmiOverWeightAdult            = 24 +=* 27
  getLvRange BmiFatAdult                   = lbi 27
  -- BodyFatPercentage
  getLvRange BodyFatPercentageNormalMale   = 15 +=* 25
  getLvRange BodyFatPercentageNormalFemale = 20 +=* 30
  -- WaistlineCm
  getLvRange WaistlineCmNormalMale         = ube 90
  getLvRange WaistlineCmNormalFemale       = ube 80

  getLvColor :: Level -> TL.Text
  getLvColor l
    | l `elem` [BmiUnderWeightMale0, BmiUnderWeightFemale0, BmiUnderWeightMale5, BmiUnderWeightFemale5, BmiUnderWeightMale10, BmiUnderWeightFemale10, BmiUnderWeightMale15, BmiUnderWeightFemale15, BmiUnderWeightMale20, BmiUnderWeightFemale20, BmiUnderWeightMale25, BmiUnderWeightFemale25, BmiUnderWeightMale30, BmiUnderWeightFemale30, BmiUnderWeightMale35, BmiUnderWeightFemale35, BmiUnderWeightMale40, BmiUnderWeightFemale40, BmiUnderWeightMale45, BmiUnderWeightFemale45, BmiUnderWeightMale50, BmiUnderWeightFemale50, BmiUnderWeightMale55, BmiUnderWeightFemale55, BmiUnderWeightMale60, BmiUnderWeightFemale60, BmiUnderWeightMale65, BmiUnderWeightFemale65, BmiUnderWeightMale70, BmiUnderWeightFemale70, BmiUnderWeightMale75, BmiUnderWeightFemale75, BmiUnderWeightMale80, BmiUnderWeightFemale80, BmiUnderWeightMale85, BmiUnderWeightFemale85, BmiUnderWeightMale90, BmiUnderWeightFemale90, BmiUnderWeightMale95, BmiUnderWeightFemale95, BmiUnderWeightMale100, BmiUnderWeightFemale100, BmiUnderWeightMale105, BmiUnderWeightFemale105, BmiUnderWeightMale110, BmiUnderWeightFemale110, BmiUnderWeightMale115, BmiUnderWeightFemale115, BmiUnderWeightMale120, BmiUnderWeightFemale120, BmiUnderWeightMale125, BmiUnderWeightFemale125, BmiUnderWeightMale130, BmiUnderWeightFemale130, BmiUnderWeightMale135, BmiUnderWeightFemale135, BmiUnderWeightMale140, BmiUnderWeightFemale140, BmiUnderWeightMale145, BmiUnderWeightFemale145, BmiUnderWeightMale150, BmiUnderWeightFemale150, BmiUnderWeightMale155, BmiUnderWeightFemale155, BmiUnderWeightMale160, BmiUnderWeightFemale160, BmiUnderWeightMale165, BmiUnderWeightFemale165, BmiUnderWeightMale170, BmiUnderWeightFemale170, BmiUnderWeightMale175, BmiUnderWeightFemale175, BmiUnderWeightAdult] = underWeightColor
    | l `elem` [BmiOverWeightMale0, BmiOverWeightFemale0, BmiOverWeightMale5, BmiOverWeightFemale5, BmiOverWeightMale10, BmiOverWeightFemale10, BmiOverWeightMale15, BmiOverWeightFemale15, BmiOverWeightMale20, BmiOverWeightFemale20, BmiOverWeightMale25, BmiOverWeightFemale25, BmiOverWeightMale30, BmiOverWeightFemale30, BmiOverWeightMale35, BmiOverWeightFemale35, BmiOverWeightMale40, BmiOverWeightFemale40, BmiOverWeightMale45, BmiOverWeightFemale45, BmiOverWeightMale50, BmiOverWeightFemale50, BmiOverWeightMale55, BmiOverWeightFemale55, BmiOverWeightMale60, BmiOverWeightFemale60, BmiOverWeightMale65, BmiOverWeightFemale65, BmiOverWeightMale70, BmiOverWeightFemale70, BmiOverWeightMale75, BmiOverWeightFemale75, BmiOverWeightMale80, BmiOverWeightFemale80, BmiOverWeightMale85, BmiOverWeightFemale85, BmiOverWeightMale90, BmiOverWeightFemale90, BmiOverWeightMale95, BmiOverWeightFemale95, BmiOverWeightMale100, BmiOverWeightFemale100, BmiOverWeightMale105, BmiOverWeightFemale105, BmiOverWeightMale110, BmiOverWeightFemale110, BmiOverWeightMale115, BmiOverWeightFemale115, BmiOverWeightMale120, BmiOverWeightFemale120, BmiOverWeightMale125, BmiOverWeightFemale125, BmiOverWeightMale130, BmiOverWeightFemale130, BmiOverWeightMale135, BmiOverWeightFemale135, BmiOverWeightMale140, BmiOverWeightFemale140, BmiOverWeightMale145, BmiOverWeightFemale145, BmiOverWeightMale150, BmiOverWeightFemale150, BmiOverWeightMale155, BmiOverWeightFemale155, BmiOverWeightMale160, BmiOverWeightFemale160, BmiOverWeightMale165, BmiOverWeightFemale165, BmiOverWeightMale170, BmiOverWeightFemale170, BmiOverWeightMale175, BmiOverWeightFemale175, BmiOverWeightAdult] = overWeightColor
    | l `elem` [BmiFatMale0, BmiFatFemale0, BmiFatMale5, BmiFatFemale5, BmiFatMale10, BmiFatFemale10, BmiFatMale15, BmiFatFemale15, BmiFatMale20, BmiFatFemale20, BmiFatMale25, BmiFatFemale25, BmiFatMale30, BmiFatFemale30, BmiFatMale35, BmiFatFemale35, BmiFatMale40, BmiFatFemale40, BmiFatMale45, BmiFatFemale45, BmiFatMale50, BmiFatFemale50, BmiFatMale55, BmiFatFemale55, BmiFatMale60, BmiFatFemale60, BmiFatMale65, BmiFatFemale65, BmiFatMale70, BmiFatFemale70, BmiFatMale75, BmiFatFemale75, BmiFatMale80, BmiFatFemale80, BmiFatMale85, BmiFatFemale85, BmiFatMale90, BmiFatFemale90, BmiFatMale95, BmiFatFemale95, BmiFatMale100, BmiFatFemale100, BmiFatMale105, BmiFatFemale105, BmiFatMale110, BmiFatFemale110, BmiFatMale115, BmiFatFemale115, BmiFatMale120, BmiFatFemale120, BmiFatMale125, BmiFatFemale125, BmiFatMale130, BmiFatFemale130, BmiFatMale135, BmiFatFemale135, BmiFatMale140, BmiFatFemale140, BmiFatMale145, BmiFatFemale145, BmiFatMale150, BmiFatFemale150, BmiFatMale155, BmiFatFemale155, BmiFatMale160, BmiFatFemale160, BmiFatMale165, BmiFatFemale165, BmiFatMale170, BmiFatFemale170, BmiFatMale175, BmiFatFemale175, BmiFatAdult] = fatColor
    | otherwise = normalColor

-- Value

class HValue a where
  getBelongedLevel :: Bool -> Double -> HealthIndicator -> a -> Level

instance HValue Double where
  getBelongedLevel :: Bool -> Double -> HealthIndicator -> Double -> Level
  getBelongedLevel gender age idc v = head $ filter ((`inRange` v) . getLvRange) $ getIndicatorLevels gender age idc
