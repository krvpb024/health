{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Utils.StandardType where

import GHC.Generics
import Data.Aeson
import qualified Data.Text.Lazy as TL
import Data.Range

-- Standard

class Std a where
  getStdTitle :: a -> TL.Text
  getStdLevels :: a -> [Level]
  getStdRanges :: a -> [Range Double]

data HealthStandard = Bmi
                    | MaleBodyFatPercentage
                    | FemaleBodyFatPercentage
                    | MaleWaistline
                    | FemaleWaistline
                    | Weight
                    deriving (Eq, Show, Generic, ToJSON)

instance Std HealthStandard where
  getStdTitle :: HealthStandard -> TL.Text
  getStdTitle Bmi                     = TL.pack "BMI"
  getStdTitle MaleBodyFatPercentage   = TL.pack "體脂率"
  getStdTitle FemaleBodyFatPercentage = TL.pack "體脂率"
  getStdTitle MaleWaistline           = TL.pack "腰團"
  getStdTitle FemaleWaistline         = TL.pack "腰團"
  getStdTitle Weight                  = TL.pack "體重"

  getStdLevels :: HealthStandard -> [Level]
  getStdLevels Bmi = enumFromTo BmiUnderWeight BmiFat
  getStdLevels MaleBodyFatPercentage = [MaleWaistlineNormal]
  getStdLevels FemaleBodyFatPercentage = [FemaleBodyFatPercentageNormal]
  getStdLevels MaleWaistline = [MaleWaistlineNormal]
  getStdLevels FemaleWaistline = [FemaleWaistlineNormal]
  getStdLevels Weight = []

  getStdRanges :: HealthStandard -> [Range Double]
  getStdRanges = fmap getLvRange . getStdLevels

-- Level

class Lv a where
  getLvTitle :: a -> TL.Text
  getLvRange :: a -> Range Double
  getLvColor :: a -> TL.Text

data Level = BmiUnderWeight
           | BmiNormal
           | BmiOverWeight
           | BmiFat
           | MaleBodyFatPercentageNormal
           | FemaleBodyFatPercentageNormal
           | MaleWaistlineNormal
           | FemaleWaistlineNormal
           deriving (Eq, Show, Generic, Enum, Bounded, ToJSON)

instance Lv Level where
  getLvTitle :: Level -> TL.Text
  getLvTitle BmiUnderWeight = TL.pack "過輕"
  getLvTitle BmiNormal      = TL.pack "正常"
  getLvTitle BmiOverWeight  = TL.pack "過重"
  getLvTitle BmiFat         = TL.pack "肥胖"
  getLvTitle MaleBodyFatPercentageNormal = TL.pack "正常"
  getLvTitle FemaleBodyFatPercentageNormal = TL.pack "正常"
  getLvTitle MaleWaistlineNormal = TL.pack "正常"
  getLvTitle FemaleWaistlineNormal = TL.pack "正常"

  getLvRange :: Level -> Range Double
  getLvRange BmiUnderWeight = ube 18.5
  getLvRange BmiNormal      = 18.5 +=* 24
  getLvRange BmiOverWeight  = 24 +=* 27
  getLvRange BmiFat         = lbi 27
  getLvRange MaleBodyFatPercentageNormal = 15 +=* 25
  getLvRange FemaleBodyFatPercentageNormal = 20 +=* 30
  getLvRange MaleWaistlineNormal = ube 90
  getLvRange FemaleWaistlineNormal = ube 80

  getLvColor :: Level -> TL.Text
  getLvColor BmiUnderWeight = TL.pack "#dadada"
  getLvColor BmiNormal      = TL.pack "#c6c6c6"
  getLvColor BmiOverWeight  = TL.pack "#b7b7b7"
  getLvColor BmiFat         = TL.pack "#ababab"
  getLvColor MaleBodyFatPercentageNormal = TL.pack "#c6c6c6"
  getLvColor FemaleBodyFatPercentageNormal = TL.pack "#c6c6c6"
  getLvColor MaleWaistlineNormal = TL.pack "#c6c6c6"
  getLvColor FemaleWaistlineNormal = TL.pack "#c6c6c6"

-- Value

class HValue a where
  getBelongedLevel :: HealthStandard -> a -> Level

instance HValue Double where
  getBelongedLevel :: HealthStandard -> Double -> Level
  getBelongedLevel std a = head $ filter ((`inRange` a) . getLvRange) $ getStdLevels std
