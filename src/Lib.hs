{-# LANGUAGE DeriveGeneric, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Lib
    ( someFunc
    ) where

import Data.Csv

import Data.Fixed
import Data.Time.LocalTime
import Data.Text (Text)
import Data.Time.Format
import Data.Time
import qualified Data.Text as T
import GHC.Generics
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Either
import Data.Maybe
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as BS (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (toStrict, readFile, unpack) 

type Stocks = [Stock]
type Prices = Vector Trade
type Trades = Vector Trade
type FileName = String
type Buy  = Trade
type Sell = Trade
type Buys = Vector Buy
type Sells = Vector Sell

data Stock 
   = Stock 
   { name :: Text
   , prices :: Prices
   } 
   | Profit 
   { name' :: Text
   , profit' :: Vector (Buy, Sell) 
   }
   deriving (Generic)

data Trade
   = Trade 
   { time  :: TimeOfDay
   , price :: Int 
   } 
   | EmptyTrade
   | Trade'
   deriving (Generic)

instance Ord Trade where
    compare Trade{price = p1} Trade{price = p2} = p1 `compare` p2

instance Eq Trade where
    (==) Trade{price = p1} Trade{price = p2} = p1 == p2

instance Show Stock where
    show Stock{..}  =  (T.unpack name) ++ ['\n'] ++ show prices ++ ['\n']
    show Profit{..} =  (T.unpack name') ++ ['\n'] ++ show profit'++ ['\n']

instance Show Trade where
    show Trade{..} = (show time) ++ ": " ++ show price
    -- show Trade{..} = show price
instance FromField TimeOfDay where
    parseField xs = case parseTime' (BS.unpack xs) of
                            Just x  -> pure x
                            Nothing -> mzero
instance FromRecord Trade

parseTime' :: String -> Maybe TimeOfDay
parseTime' str = parseTime defaultTimeLocale "%T" str

dropFromBack :: Int -> String -> String
dropFromBack n xs = take (len' - n) xs
  where len' = length xs

files :: [String]
files = ["EE.csv", "Giffgaff.csv", "O2.csv", "Three.csv", "Vodafone.csv"]
-- files = ["EE.csv"]

someFunc :: IO () 
someFunc = do 
    stocks <- readFiles files
    let stockList = catMaybes $ fmap eatDinner stocks
    -- print stockList
    -- mapM_ (print . show) stockList
    let traders = fmap (\s -> trade1 s 1000) stockList
    -- print traders
    let m = maxTrader traders
    print m
    printTotal m

printTotal :: Trader -> IO ()
printTotal Trader1{..} = print money


getTrade :: FileName -> IO (Maybe (String, Prices))
getTrade xs = do
    file <- LBS.readFile xs
    let csv = decode NoHeader file :: Either String (Vector Trade)
    case csv of 
        Right x -> return $ Just (dropFromBack 4 xs, x)
        Left  e -> return $ Nothing

createStock :: (String, Prices) -> Stock
createStock (s,p) = Stock (T.pack s) p

readFiles :: [FileName] -> IO (Stocks)
readFiles xs = do x <- mapM create' xs
                  return $ catMaybes x
  where
    create' :: FileName -> IO (Maybe Stock)
    create' fn = getTrade fn >>= \price -> do
        case price of 
            Just price' -> return $ Just $ createStock price'
            Nothing     -> return $ Nothing

findOP :: (Trade -> Trade -> Bool) -- (<) or (>)
                    -> Vector Trade     -- Start Vector
                    -> Vector Trade     -- Acc
                    -> Vector Trade     -- Result 
-- ^ First is filled Vector, 2nd Accumulator, 3 Result
findOP op vs acc 
    | V.null vs       = acc              
    | V.length vs < 3 = acc
    | inbetween op    = findOP op (V.drop 2 vs) (V.cons vs2 acc)
    | otherwise       = findOP op (V.drop 1 vs) acc 
  where
    inbetween (<) =  vs1 <=  vs2  && (vs3 < vs2) 
    inbetween (>) = (vs1 > vs2) && (vs3 >=  vs2)
    vs1 = vs V.! 0
    vs2 = vs V.! 1
    vs3 = vs V.! 2

findMinima :: Vector Trade -> Vector Trade -> Vector Trade
findMinima = findOP (>)

findMaxima :: Vector Trade -> Vector Trade -> Vector Trade
findMaxima = findOP (<)

profits' :: Vector Trade -> Vector Trade -> Vector (Buy, Sell)
profits' = V.zip

profits :: Stock -> Stock
profits Stock{..} = Profit { name'   = name 
                           , profit' = profits' buy sell }
  where
    buy  = findMinima prices V.empty
    sell = findMaxima prices V.empty

showBuy :: Stock -> IO ()
showBuy (Profit{..}) = mapM_ (\x -> print x) profit'

data Acc
   = Acc 
   { b :: Buys
   , s :: Sells
   , w :: WannaDo 
   , l :: Trade -- last checked trade
   } deriving Generic

data WannaDo = SELL | BUY deriving (Show, Eq)

createStartAcc :: Trade -> Acc
createStartAcc t = Acc (V.cons t V.empty) V.empty SELL t

eatTillMinimum :: Vector Trade -> Maybe (Trade, Vector Trade)
-- ^ unsafe
eatTillMinimum vs 
    | V.null vs = Nothing
    | vs2 > vs1  = Just (vs1, V.drop 1 vs)
    | otherwise  = eatTillMinimum $ V.drop 1 vs
  where
    vs1 = vs V.! 0
    vs2 = vs V.! 1

eat :: Trade -> Acc -> Acc
eat trade (Acc b s SELL l)  | trade >= l = Acc b s SELL trade
                            | trade <  l = Acc b (V.cons l s) BUY trade
eat trade (Acc b s BUY  l)  | trade >  l = Acc (V.cons l b) s SELL trade
                            | trade <= l = Acc b s BUY trade

eatTrades :: Vector Trade -> Maybe Acc
eatTrades vs = case eatTillMinimum vs of
    Just (t, ts) -> Just $ V.foldl (\acc x -> eat x acc) (createStartAcc t) ts
    Nothing      -> Nothing

eatDinner :: Stock -> Maybe Stock
eatDinner Stock{..} = case eatTrades prices of
    Just Acc{..} -> Just $ Profit { name' = name
                                  , profit' = V.zip (V.reverse b) (V.reverse s)
                          }
    Nothing      -> Nothing

printDinner :: Stock -> IO ()
printDinner Stock{..} = case eatTrades prices of
    Just Acc{..} -> do 
        print $ V.zip (V.reverse b) (V.reverse s)
    Nothing      -> putStrLn "Nothing"

-- lets trade
data Trader 
   = Trader1 
   { stockName :: Text
   , money :: Int
   , tprofit :: Vector (Buy, Sell)
   , tnumber :: Vector Int
   }  
instance Show Trader where
    show Trader1{..} = do 
        let s  = T.unpack stockName
        let num = V.reverse tnumber
        let buys = fmap (\x -> trans x "BUY" s )$ V.zipWith  f  num tprofit
        let sell = fmap (\x -> trans x "SELL" s )$ V.zipWith f' num tprofit
        show $ V.zip buys sell
      where f :: Int -> (Trade, Trade) -> (Int, Trade)
            f  i (t1, _) = (,) i t1 
            f' i (_ ,t2) = (,) i t2
            trans :: (Int, Trade) -> String -> String -> String
            trans (i, Trade{..}) t n = (show time) ++ "," ++ t ++ "," ++ n ++ "," ++ (show i) 

printTrades :: Trader -> IO ()
printTrades Trader1{..} = do
    print stockName
    let s = stockName
    print tnumber
    print tprofit
    let buys = V.zipWith f tprofit tnumber 
    print buys

  where
    f :: (Buy, Sell) -> Int -> (Int, Trade)
    f (b, _) i = (i, b)

instance Ord Trader where
    compare Trader1{money = m1} Trader1{money = m2} = m1 `compare` m2

instance Eq Trader where
    (==) Trader1{money = m1} Trader1{money = m2} = m1 == m2

startTrader1 :: Int -> Stock -> Trader
startTrader1 startMoney Profit{..} = Trader1 name' startMoney profit' V.empty

trade1' :: Trader -> Trader
trade1' Trader1{..} = Trader1 stockName money' tprofit trades'
  where 
    (money', trades') = makeMoney tprofit (money, tnumber)
    makeMoney :: Vector (Buy, Sell) -> (Int, Vector Int) -> (Int, Vector Int)
    makeMoney vs (have, t) | V.null vs = (have,t)
                           | otherwise = makeMoney (V.drop 1 vs) (have', t')
      where
        t' = V.cons x t
        have' = have + (x * vs')
        x     = have `div` (getP b)
        vs'   = (getP s) - (getP b)
        (b,s) = vs V.! 0
        getP :: Trade -> Int 
        getP Trade{..} = price

trade1 :: Stock -> Int -> Trader
trade1 s m = trade1' $ startTrader1 m s

maxTrader :: [Trader] -> Trader
maxTrader = maximum
        

