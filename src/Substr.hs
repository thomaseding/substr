module Substr (main) where

import Data.List
import System.Environment
import System.Exit
import System.IO


data Range w = Range (w Integer) (w Integer)


data Id a = Id a


tryRead :: (Read a) => String -> Maybe a
tryRead str = case reads str of
    [(val, "")] -> Just val
    _ -> Nothing


parse :: String -> Maybe (Maybe Integer)
parse str = case tryRead str of
    Just num -> Just $ Just num
    Nothing -> case str of
        "-" -> Just Nothing
        _ -> Nothing


main :: IO ()
main = do
    args <- getArgs
    range <- case mapM parse args of
        Just [begin] -> return $ Range begin Nothing
        Just [begin, end] -> return $ Range begin end
        _ -> exitFailure
    interact $ substr range


substr :: Range Maybe -> String -> String
substr (Range Nothing Nothing) str = str
substr (Range Nothing end) str = substr (Range (Just 0) end) str
substr (Range begin Nothing) str = substr (Range begin $ Just $ genericLength str) str
substr (Range (Just begin) (Just end)) str = substr' (genericLength str) str range'
    where
        range' = Range (Id begin) (Id end)


substr' :: Integer -> String -> Range Id -> String
substr' len str (Range (Id begin) (Id end))
    | begin < 0 = substr' len str $ Range (Id $ len + begin) (Id end)
    | end < 0 = substr' len str $ Range (Id begin) (Id $ len + end)
    | otherwise = genericTake (end - begin) . genericDrop begin $ str
    






