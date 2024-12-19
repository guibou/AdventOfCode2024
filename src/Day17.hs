module Day17 where

import Utils
import qualified Data.Vector as Vector
import Data.Bits (xor)
import Data.List

data Computer = Computer {
   registerA :: Int,
   registerB :: Int,
   registerC :: Int,
   program :: Vector Int,
   pc :: Int
   }
    deriving (Show)

fileContent = Computer {
   registerA = 30878003,
   registerB = 0,
   registerC = 0,

   program = Vector.fromList [2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0],
   pc = 0
   }

-- * Generics

-- * FIRST problem
run :: Computer -> [Int]
run computer
  | computer.pc + 1 >= length computer.program = []
  | otherwise = do
    let
       instruction = computer.program Vector.! computer.pc
       operand = computer.program Vector.! (computer.pc + 1)

    case instruction of
      0 -> do -- adv
        let numerator = computer.registerA
        let denominator = 2 ^ (combo computer operand)
        run (computer { registerA = numerator `div` denominator, pc = computer.pc + 2 })
      1 -> do -- bxl
        run (computer { registerB = computer.registerB `xor` operand, pc = computer.pc + 2 })
      2 -> do -- bst
        run (computer { registerB = combo computer operand `mod` 8, pc = computer.pc + 2 })
      3 -> do -- jnz
        if computer.registerA == 0
        then run (computer { pc = computer.pc + 2 })
        else run (computer { pc = operand })
      4 -> do -- bxc
        run (computer { registerB = computer.registerB `xor` computer.registerC, pc = computer.pc + 2 })
      5 -> do -- out
        (combo computer operand `mod` 8) : run (computer { pc = computer.pc + 2 })
      6 -> do -- bdv
        let numerator = computer.registerA
        let denominator = 2 ^ (combo computer operand)
        run (computer { registerB = numerator `div` denominator, pc = computer.pc + 2 })
      7 -> do -- cdv
        let numerator = computer.registerA
        let denominator = 2 ^ (combo computer operand)
        run (computer { registerC = numerator `div` denominator, pc = computer.pc + 2 })
      _ -> error $ "Instruction not found:" <> show instruction

day computer = intercalate "," (map show $ run computer)

        
    
combo :: Computer -> Int -> Int
combo computer val = case val of
  0 -> 0
  1 -> 1
  2 -> 2
  3 -> 3
  4 -> computer.registerA
  5 -> computer.registerB
  6 -> computer.registerC
  7 -> error "reserved"
  _ -> error $ "Invalid combo value: " <> show val

-- * SECOND problem
day' = undefined --

bruteForce computer = find (\newA -> run (computer { registerA = newA }) == program) [0..]
  where
    program = Vector.toList computer.program

ex = Computer {
   registerA = 729,
   registerB = 0,
   registerC = 0,
   program = Vector.fromList [0,1,5,4,3,0],
   pc = 0
}

ex' = Computer { 
  registerA = 2024,
  registerB = 0,
  registerC = 0,
  program = Vector.fromList [0,3,5,4,3,0],
  pc = 0
}

-- started at Thu Dec 19 11:40:12 AM +04 2024
