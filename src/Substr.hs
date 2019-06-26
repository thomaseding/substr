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
  let (elipsis, args') = case args of
        "--elipsis" : rest -> (True, rest)
        _ -> (False, args)
  range <- case mapM parse args' of
    Just [begin] -> return $ Range begin Nothing
    Just [begin, end] -> return $ Range begin end
    _ -> exitFailure
  interact $ runProgram elipsis range


runProgram :: Bool -> Range Maybe -> String -> String
runProgram elipsis mRange s = let
  s' = substr mRange s
  n' = length s'
  n  = length s
  in case elipsis && n' < n of
    False -> s'
    True  -> let
      s'' = take (n' - 3) s'
      in s'' ++ "..."


substr :: Range Maybe -> String -> String
substr mRange str = case mRange of
  Range Nothing Nothing         -> str
  Range Nothing end             -> substr (Range (Just 0) end) str
  Range begin Nothing           -> substr (Range begin $ Just $ genericLength str) str
  Range (Just begin) (Just end) -> substr' (genericLength str) str range'
    where
      range' = Range (Id begin) (Id end)


substr' :: Integer -> String -> Range Id -> String
substr' len str (Range (Id begin) (Id end))
  | begin < 0 = substr' len str $ Range (Id $ len + begin) (Id end)
  | end < 0 = substr' len str $ Range (Id begin) (Id $ len + end)
  | otherwise = genericTake (end - begin) . genericDrop begin $ str


