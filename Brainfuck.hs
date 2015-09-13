{-
A brainfuck interpreter in Haskell.

An exercise to flex these FP muscles.
-}
module Main where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Data.Char

data Tape a = Tape [a] a [a]
data Program = Program String Position Scope
data Instruction =
  Increment | Decrement |
  MoveForward |MoveBackward |
  JumpAhead | JumpBack |
  Input | Output |
  Invalid
  deriving(Show)
data Direction = Forward | Backward
  
type TuringMachine a = (Program, Tape a)
type Position = Int
type Scope = [Position]

main :: IO ()
main = do
  initialize "helloworld.bf"

initialize :: FilePath -> IO ()
initialize brainfuckProgram = do
  program <- readFile brainfuckProgram
  haltState <- evalStateT step (Program program 0 [], Tape [] 0 (replicate 30000 0))
  return ()
  
step :: StateT (TuringMachine Int) IO (TuringMachine Int)
step = do
  state@(program@(Program text pc scope), (Tape l current r)) <- get
  let instruction = readInstruction program
  case instruction of
   Just instruction' -> do
     case instruction' of
      Output -> liftIO $ putChar (chr current)
      _ -> liftIO $ return ()
     let state'@(program', tape') = interpret instruction' state
     case instruction' of
      Input -> do
        current' <- liftIO $ getChar
        put (program', Tape l (ord current') r)
      _ -> put state'
     step
   Nothing -> return state

readInstruction :: Program -> Maybe Instruction
readInstruction (Program text pc scope)
  | pc == (-1) = Nothing
  | pc < length text = Just $ case character of
                             '>' -> MoveForward
                             '<' -> MoveBackward
                             '+' -> Increment
                             '-' -> Decrement
                             '[' -> JumpAhead
                             ']' -> JumpBack
                             '.' -> Output
                             ',' -> Input
                             _ -> Invalid
  | otherwise = Nothing
  where
    character = text !! pc

nextInstruction :: Program -> Program
nextInstruction (Program text pc scope) = Program text (pc+1) scope

interpret :: Instruction -> TuringMachine Int -> TuringMachine Int
interpret instruction state@(program, tape) = (nextInstruction program', tape')
  where
    (Tape leftSlice element rightSlice) = tape
    (program', tape') = case instruction of
                         MoveForward -> (program, moveForward tape)
                         MoveBackward -> (program, moveBackward tape)
                         Increment -> (program, increment tape)
                         Decrement -> (program, decrement tape)
                         JumpAhead
                           | element == 0 -> (matchBracket program Forward, tape)
                           | otherwise -> (pushScopeLevel program, tape)
                         JumpBack
                           | element > 0 -> (jumpToMatchedBracket program, tape)
                           | otherwise -> (popScopeLevel program, tape)
                         _ -> (program, tape)

moveForward :: Show a => Tape a -> Tape a
moveForward (Tape s current e) = (Tape s' current' e')
  where
    s' = s ++ [current]
    current' = head e
    e' = drop 1 e
  
moveBackward :: Show a => Tape a -> Tape a
moveBackward (Tape s current e) = (Tape s' current' e')
  where
    s' = init s
    current' = last s
    e' = current : e

increment :: Tape Int -> Tape Int
increment (Tape s current e) = Tape s current' e
  where
    current' = current+1

decrement :: Tape Int -> Tape Int
decrement (Tape s current e) = Tape s current' e
  where
    current' = current-1

popScopeLevel :: Program -> Program
popScopeLevel (Program text pc scope) = (Program text pc (drop 1 scope))

pushScopeLevel :: Program -> Program
pushScopeLevel (Program text pc scope) = (Program text pc (pc:scope))

jumpToMatchedBracket :: Program -> Program
jumpToMatchedBracket (Program text pc scope) = Program text pc' scope
  where
    pc' = head scope
    
matchBracket :: Program -> Direction -> Program
matchBracket program@(Program text pc scope) direction
  | pc == (-1) = error "No matching [ bracket."
  | pc < length text = if foundBracket
                       then
                         if openBracket
                         then matchBracket (Program  text (pc+number) (pc:scope)) direction
                         else
                           if correctBracket
                           then (Program text pc (drop 1 scope))
                           else matchBracket (Program text (pc+number) (drop 1 scope)) direction
                       else matchBracket (Program text (pc+number) scope) direction
  | otherwise = error "No matching ] bracket."
  where
    number = case direction of Forward -> 1; Backward -> (-1)
    foundBracket = character == '[' || character == ']'
    openBracket = character == '['
    correctBracket = (length scope) == 1
    character = (text !! pc)
