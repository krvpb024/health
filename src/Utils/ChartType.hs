{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Utils.ChartType where
import GHC.Generics
import Data.Aeson
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
import Data.Time
import qualified Data.Text.Lazy as TL
import Utils.StandardType
import Data.Range
import Servant
import Control.Exception
import Data.ByteString.Lazy.UTF8 as BLU

data Chart = Chart { chartWidth   :: Double
                   , chartHeight  :: Double
                   , chartPadding :: Double
                   } deriving (Eq, Show, Generic, ToJSON)

data ChartCanvas = ChartCanvas { canvasWidth       :: Double
                               , canvasHeight      :: Double
                               , canvasXStartPoint :: Double
                               , canvasXEndPoint   :: Double
                               , canvasYStartPoint :: Double
                               , canvasYEndPoint   :: Double
                               , canvasXLength     :: Double
                               , canvasYLength     :: Double
                               } deriving (Eq, Show, Generic, ToJSON)

getChartCanvas :: Chart -> ChartCanvas
getChartCanvas (Chart w h p) = ChartCanvas {
    canvasWidth = w
  , canvasHeight = h
  , canvasXStartPoint = xStartPoint
  , canvasXEndPoint = xEndPoint
  , canvasYStartPoint = yStartPoint
  , canvasYEndPoint = yEndPoint
  , canvasXLength = xEndPoint - xStartPoint
  , canvasYLength = yEndPoint - yStartPoint
  }
  where xStartPoint = w * p
        xEndPoint = w * (1 - p)
        yStartPoint = h * (1 - p)
        yEndPoint = h * p

data ChartPoint = ChartPoint { getX :: Double
                             , getY :: Double
                             }
                             deriving (Eq, Show, Generic, ToJSON)

newtype XValueData = XValueData { xScale :: Double }

getXValueDataAndLabels :: ChartCanvas -> [Day] -> (XValueData, [(Day, Double)])
getXValueDataAndLabels (ChartCanvas w _ xsp xep ysp _ xl _) ds = (
    XValueData scaleValue
  , (\(i, d) -> (d, xsp + i * scaleValue)) <$>
    zip ([0..] :: [Double]) ds
  )
  where
        scaleValue = (xep - xsp) / (fromIntegral $ Prelude.length ds - 1 :: Double)

getXValuePoint :: ChartCanvas -> XValueData -> Double -> Double
getXValuePoint (ChartCanvas _ _ xsp _ _ _ _ _) (XValueData scale) i = xsp + i * scale

data YValueData = YValueData {
    yMin         :: Double
  , yMax         :: Double
  }

getYValueDataAndLabels :: ChartCanvas -> [Double] -> (YValueData, [(Double, Double)])
getYValueDataAndLabels cv@(ChartCanvas w h xsp xep ysp yep xl yl) ds = (yvd, yLabels)
  where
        minV = minimum ds
        maxV = maximum ds
        valueLength = maxV - minV
        interval = max 1 (fromInteger $ floor $ valueLength / 5) * 0.5
        paddingMinValue = max 0 $ (fromIntegral $ floor minV :: Double) - interval * 2
        paddingMaxValue = (fromIntegral $ floor maxV :: Double) + interval * 2
        paddingValueList = enumFromThenTo paddingMinValue (paddingMinValue + interval) paddingMaxValue
        scaleValue = yl / ((fromIntegral $ Prelude.length paddingValueList :: Double) - 1)
        yvd = YValueData paddingMinValue paddingMaxValue
        yLabels = (\(i, v) -> (v, ysp + i * scaleValue)) <$> zip ([0..] :: [Double]) paddingValueList

getYValuePoint :: ChartCanvas -> YValueData ->  Double -> Double
getYValuePoint (ChartCanvas _ _ _ _ ysp _ _ yl) (YValueData min max) v = ysp + ((v - min) / (max - min)) * yl

getValuePoints :: ChartCanvas -> YValueData -> XValueData -> [(Day, Double)] -> [ChartPoint]
getValuePoints cv yv xv dds = uncurry ChartPoint <$> zip dayPoints valuePoints
  where days = fst <$> dds
        values = snd <$> dds
        dayPoints = getXValuePoint cv xv . fst <$> zip [0..] days
        valuePoints = getYValuePoint cv yv <$> values

getValueLine :: [ChartPoint] -> TL.Text
getValueLine = TL.pack . concatMap (\p -> " " <> show (getX p) <> "," <> show (getY p))

data LevelItem = LevelItem {
    levelItemPoint      :: ChartPoint
  , levelItemWidth      :: Double
  , levelItemHeight     :: Double
  , levelItemTitle      :: TL.Text
  , levelItemTitlePoint :: ChartPoint
  , levelItemColor      :: TL.Text
  } deriving (Eq, Show, Generic, ToJSON)

getStandardLevels :: ChartCanvas -> YValueData -> HealthStandard -> [LevelItem]
getStandardLevels cv yv@(YValueData yMin yMax) std = rangeToTemplate <$> levels
  where
        valueRange = yMin +=+ yMax
        stdLevels = getStdLevels std
        stdLevelRanges = getStdRanges std
        overlappedRanges = filter (rangesOverlap valueRange . getLvRange) stdLevels
        getYValuePoint' = getYValuePoint cv yv
        levels = getStdLevels std
        rangeToTemplate :: Level -> LevelItem
        rangeToTemplate lv = case getLvRange lv of
          SpanRange (Bound y _) (Bound y2 _) -> LevelItem {
              levelItemPoint = ChartPoint (canvasXStartPoint cv) upperPointY
            , levelItemWidth = canvasXLength cv
            , levelItemHeight = height
            , levelItemTitle = getLvTitle lv
            , levelItemTitlePoint = ChartPoint (canvasXStartPoint cv + canvasXLength cv / 2) (upperPointY + height / 2)
            , levelItemColor = getLvColor lv
            }
            where upperPointY = getYValuePoint' $ min y2 yMax
                  lowerPoint = getYValuePoint' $ max y yMin
                  height = lowerPoint - upperPointY
          LowerBoundRange (Bound y _) -> LevelItem {
              levelItemPoint = ChartPoint (canvasXStartPoint cv) upperPointY
            , levelItemWidth = canvasXLength cv
            , levelItemHeight = height
            , levelItemTitle = getLvTitle lv
            , levelItemTitlePoint = ChartPoint (canvasXStartPoint cv + canvasXLength cv / 2) (upperPointY + height / 2)
            , levelItemColor = getLvColor lv
            }
            where upperPointY = getYValuePoint' yMax
                  lowerPoint = getYValuePoint' $ max y yMin
                  height = lowerPoint - upperPointY
          UpperBoundRange (Bound y2 _) -> LevelItem {
              levelItemPoint = ChartPoint (canvasXStartPoint cv) upperPointY
            , levelItemWidth = canvasXLength cv
            , levelItemHeight = height
            , levelItemTitle = getLvTitle lv
            , levelItemTitlePoint = ChartPoint (canvasXStartPoint cv + canvasXLength cv / 2) (upperPointY + height / 2)
            , levelItemColor = getLvColor lv
            }
            where upperPointY = getYValuePoint' $ min y2 yMax
                  lowerPoint = getYValuePoint' yMin
                  height = lowerPoint - upperPointY
          _ -> throw err500 { errBody = BLU.fromString "Unknown Health Standard Range." }

data ChartWithValue = ChartWithValue {
    chartVTitle :: TL.Text
  , chartVAxisStart :: Double
  , chartVXAxisEnd :: Double
  , chartVYAxisEnd :: Double
  , chartVXLabels :: [(Day, Double)]
  , chartVYLabels :: [(Double, Double)]
  , chartVValuePoints :: [ChartPoint]
  , chartVValueLine :: TL.Text
  , chartVLevels :: [LevelItem]
  } deriving (Eq, Show, Generic, ToJSON)
