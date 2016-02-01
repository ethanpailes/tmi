module Main where

import Parse

import Data.Sequence
import System.Environment
import Control.Exception
import Data.Typeable
import qualified Data.List as L

import Test.QuickCheck
import Text.Megaparsec
import Data.Either

import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  case args of
    (tm:inputTape:rest) -> do
      input <- readFile tm
      case runParser parseTuringMachine tm input of
        (Left err) -> print err
        (Right turingMachine) -> 
          if checkInputString turingMachine inputTape
             then runInterpreter
                    (rest /= [] && head rest == "-v")
                    (rest /= [] && head rest == "-t")
                    turingMachine (tapeify inputTape)

          else putStrLn
            ("Parses fine, but there is something wrong with your input string. "
            ++ "Does it match sigma?")
    (tm:_) -> do
      input <- readFile tm
      case runParser parseTuringMachine tm input of
        (Left err) -> print err
        (Right parseRes) -> putStrLn "Parses fine!"
    [] -> putStrLn "Usage: ./tm <tm-file> <input-string (ones and zeros)>"

runInterpreter ::
        Bool -> -- run with trace messages
        Bool -> -- run quickcheck tests
          Tm -> -- the turing machine
  Seq String -> -- the input tape
        IO ()
runInterpreter trace test tm tape =
  if test
     then do
       putStrLn "You've activated my trap card!"
       testTM tm
       -- putStrLn $ if runTM tm tape then "Accept" else "Reject"

  else putStrLn $ if runTM trace tm tape then "Accept" else "Reject"

checkInputString :: Tm -> String -> Bool
checkInputString tm tape = all (`elem` sigma tm) tape

runTM :: Bool -> Tm -> Seq String -> Bool
runTM trace tm tape = runTM' trace (delta tm) (Q (start tm)) 0 tape

runTM' :: Bool -> -- trace
    ((Q, String) -> Either Bool (Q, String, Dir)) ->
    Q -> -- current state
    Int -> -- position
    Seq String -> -- tape
    Bool
runTM' trc tm state n tape =
    case tm (state, sym) of
      (Left answer) -> answer
      (Right (state', sym', L)) ->
        if trc then
            trace (show state ++ ", " ++ show sym ++ " --> " ++ show state' ++
                ", " ++ show sym' ++ ", L")
            runTM' trc tm state' (if n > 0 then n-1 else 0) (update n sym' tape)
        else runTM' trc tm state' (if n > 0 then n-1 else 0) (update n sym' tape)
      (Right (state', sym', R)) ->
        if trc then
            trace (show state ++ ", " ++ show sym ++ " --> " ++ show state' ++
                   ", " ++ show sym' ++ ", R")
          runTM' trc tm state' (n+1) (update n sym' tape)
        else runTM' trc tm state' (n+1) (update n sym' tape)
  where sym = tape `index` n

tapeify :: String -> Seq String
tapeify s = fromList $ map 
              (\c -> [c]) (s ++ "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB")

prop_onesFirst :: Tm -> Int -> Int -> Bool
prop_onesFirst tm m n =
  let (ma, na) = (if m < 0 then -m else m, if n < 0 then -n else n)
      (smaller, larger) = if ma < na then (ma, na) else (na, ma)
      shouldAccept = smaller * 2 <= larger
      zeros = L.take larger (repeat '0')
      ones = L.take smaller (repeat '1')
      inputTape = tapeify (ones ++ zeros)
   in shouldAccept == runTM False tm inputTape

prop_zerosFirst :: Tm ->Int -> Int -> Bool
prop_zerosFirst tm m n =
  let (ma, na) = (if m < 0 then -m else m, if n < 0 then -n else n)
      (smaller, larger) = if ma < na then (ma, na) else (na, ma)
      shouldAccept = smaller * 2 <= larger
      zeros = L.take larger (repeat '0')
      ones = L.take smaller (repeat '1')
      inputTape = tapeify (zeros ++ ones)
   in shouldAccept == runTM False tm inputTape

prop_interleave :: Tm -> Int -> Int -> Bool
prop_interleave tm m n =
  let (ma, na) = (if m < 0 then -m else m, if n < 0 then -n else n)
      (smaller, larger) = if ma < na then (ma, na) else (na, ma)
      shouldAccept = smaller * 2 <= larger
      zeros = L.take larger (repeat '0')
      ones = L.take smaller (repeat '1')
      inputTape = tapeify (interleave zeros ones)
   in shouldAccept == runTM False tm inputTape

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

testTM :: Tm -> IO ()
testTM tm =
     putStrLn "prop_onesFirst"
  >> quickCheck (prop_onesFirst tm)
  >> putStrLn "prop_zerosFirst"
  >> quickCheck (prop_zerosFirst tm)
  >> putStrLn "prop_interleave"
  >> quickCheck (prop_interleave tm)

