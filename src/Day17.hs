module Day17 where

import Data.Bits (xor)
import Data.List
import Data.Vector qualified as Vector
import Utils
import qualified Data.Set as Set
import qualified Data.SBV as SBV
import qualified Data.Map.Strict as Map
import Data.SBV.Dynamic (CVal(..), CV (cvVal))
import Data.SBV.Internals (SMTModel(..))

data Computer e = Computer
  { registerA :: e,
    registerB :: e,
    registerC :: e,
    program :: Vector Int,
    pc :: Int
  }
  deriving (Show)

fileContent :: OpE e => Computer e
fileContent =
  Computer
    { registerA = litE 30878003,
      registerB = litE 0,
      registerC = litE 0,
      program =
        Vector.fromList
          [ 2,
            4,
            1,
            2,
            7,
            5,
            0,
            3,
            4,
            7,
            1,
            7,
            5,
            5,
            3,
            0
          ],
      pc = 0
    }

-- * Generics

-- * FIRST problem

run :: OpE e => Computer e -> (Computer e, [e])
run computer
  | computer.pc + 1 >= length computer.program = (computer, [])
  | otherwise = do
      let instruction = computer.program Vector.! computer.pc
          operand = computer.program Vector.! (computer.pc + 1)

      case instruction of
        0 -> do
          -- adv
          let numerator = computer.registerA
          let denominator = litE 2 `powInt` (combo computer operand)
          run (computer {registerA = numerator `divE` denominator, pc = computer.pc + 2})
        1 -> do
          -- bxl
          run (computer {registerB = computer.registerB `xorEi` operand, pc = computer.pc + 2})
        2 -> do
          -- bst
          run (computer {registerB = combo computer operand `modE` (litE 8), pc = computer.pc + 2})
        3 -> do
          -- jnz
          if z computer.registerA
            then run (computer {pc = computer.pc + 2})
            else run (computer {pc = operand})
        4 -> do
          -- bxc
          run (computer {registerB = computer.registerB `xorE` computer.registerC, pc = computer.pc + 2})
        5 -> do
          -- out
          ((combo computer operand `modE` (litE 8)) :) <$> run (computer {pc = computer.pc + 2})
        6 -> do
          -- bdv
          let numerator = computer.registerA
          let denominator = litE 2 `powInt` (combo computer operand)
          run (computer {registerB = numerator `divE` denominator, pc = computer.pc + 2})
        7 -> do
          -- cdv
          let numerator = computer.registerA
          let denominator = litE 2 `powInt` (combo computer operand)
          run (computer {registerC = numerator `divE` denominator, pc = computer.pc + 2})
        _ -> error $ "Instruction not found:" <> show instruction

day :: Computer Int -> String
day computer = intercalate "," (map show $ snd $ run computer)

class OpE e where
  xorEi :: e -> Int -> e
  xorE :: e -> e -> e
  powInt :: e -> e -> e
  divE :: e -> e -> e
  modE :: e -> Int -> e
  litE :: Integer -> e
  z :: e -> Bool

instance OpE Int where
  xorE = xor
  xorEi = xor
  powInt = (^)
  divE = div
  modE = mod
  litE = fromIntegral
  z = (==0)

combo :: OpE e => Computer e -> Int -> e
combo computer val = case val of
  0 -> litE 0
  1 -> litE 1
  2 -> litE 2
  3 -> litE 3
  4 -> computer.registerA
  5 -> computer.registerB
  6 -> computer.registerC
  7 -> error "reserved"
  _ -> error $ "Invalid combo value: " <> show val

-- * SECOND problem
day' computer = do
  res <- solver computer
  case res of
    SBV.LexicographicResult (SBV.Satisfiable _ model) -> pure $ Just $ cvIntToInteger $ (cvVal (Map.fromList (modelAssocs model) Map.! "unknown"))
    _ -> pure Nothing

cvIntToInteger (CInteger i) = i

solver computer = SBV.optimize SBV.Lexicographic $ do
  let constraints = run' computer

  unknown <- SBV.sInt64 "unknown"

  SBV.constrain $ unknown SBV..> 0
  mapM_ (SBV.constrain . toConstraint unknown) constraints

  SBV.minimize "unknown" unknown

toConstraint :: _ -> Expr -> SBV.SBool
toConstraint unknown (EqMod a b) = toExpr unknown a `SBV.sMod` (fromIntegral 8) SBV..== (fromIntegral b)
toConstraint unknown (Neq0 a) = toExpr unknown a SBV../= 0
toConstraint unknown (Eq0 a) = toExpr unknown a SBV..== 0

toExpr :: SBV.SInt64 -> Expr -> SBV.SInt64
toExpr unknown Unknown = unknown
toExpr unknown (Div a b) = SBV.sDiv (toExpr unknown a) (toExpr unknown b)
toExpr unknown (Mod a b) = SBV.sMod (toExpr unknown a) (fromIntegral b)
toExpr unknown (Lit i) = fromIntegral i
toExpr unknown (XorEi a b) = toExpr unknown a `xor` (fromIntegral b)
toExpr unknown (XorE a b) = toExpr unknown a `xor` toExpr unknown b
toExpr unknown (Pow (Lit 2) b) = 1 `SBV.sShiftLeft` (toExpr unknown b)
toExpr unknown e = error $ show e

data Expr
  = Unknown
  | Div Expr Expr

  | XorE Expr Expr
  | XorEi Expr Int

  | Pow Expr Expr
  | Mod Expr Int
  | Lit Integer
  | EqMod Expr Int
  | Eq0 Expr
  | Neq0 Expr
  deriving (Show, Ord, Eq)

simplifyStep :: Expr -> Expr
simplifyStep (Pow (Lit a) (Lit b)) = Lit (a ^ b)
simplifyStep (Lit a) = Lit a
simplifyStep (Pow a b) = Pow (simplify a) (simplify b)
simplifyStep Unknown = Unknown
simplifyStep (Div (Div e (Lit x)) (Lit y)) = Div (simplify e) (Lit (x * y))
simplifyStep (Div a b) = Div (simplify a) (simplify b)
simplifyStep (XorE a b) = XorE (simplify a) (simplify b)
simplifyStep (XorEi a b) = XorEi (simplify a) b
simplifyStep (Mod a b) = Mod (simplify a) b
simplifyStep (EqMod a b) = EqMod (simplify a) b
simplifyStep (Eq0 a) = Eq0 (simplify a)
simplifyStep (Neq0 a) = Neq0 (simplify a)

simplify x
  | x == x' = x
  | otherwise = simplify x'
  where x' = simplifyStep x


instance OpE Expr where
  xorE = XorE
  xorEi = XorEi
  powInt = Pow
  litE i = Lit i
  modE = Mod
  divE = Div
  z = error "Not required!"

run' :: Computer Expr -> _
run' computer = Set.map simplify $ Set.fromList $ do
  let program' = Vector.take (length computer.program - 4) computer.program
      [5, comboOut, 3, 0] = drop (length computer.program - 4) $ Vector.toList computer.program
  let computer' =
        computer
          { program = program',
            registerA = Unknown
          }

      go computer [latestValue] = do
        let computer' = fst $ run (computer { pc = 0 })
        let outValue = combo computer' comboOut
        let endTest = computer'.registerA
        [Eq0 endTest, outValue `EqMod` latestValue]
      go computer (value : vs) = do
        let computer' = fst $ run (computer { pc = 0 })
        let outValue = combo computer' comboOut
        let endTest = computer'.registerA
        [Neq0 endTest, outValue `EqMod` value] <> go computer' vs

  go computer' (Vector.toList computer.program)

ex :: OpE e => Computer e
ex =
  Computer
    { registerA = litE 729,
      registerB = litE 0,
      registerC = litE 0,
      program =
        Vector.fromList
          [ 0,
            1,
            5,
            4,
            3,
            0
          ],
      pc = 0
    }

ex' :: (OpE e) => Computer e
ex' =
  Computer
    { registerA = litE 2024,
      registerB = litE 0,
      registerC = litE 0,
      program =
        Vector.fromList
          [ 0,
            3, -- regA = regA `div` (2 ^ 3)
            5,
            4, -- out (regA `mod` 8)
            3,
            0 -- jnz 0 regA
          ],
      pc = 0
    }

-- 35184372088832 is too low
-- started at Thu Dec 19 11:40:12 AM +04 2024
