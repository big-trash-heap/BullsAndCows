

import System.Random (randomIO)
import Control.Monad (when, unless)
import Data.Char (isDigit)
import Data.List (nub, elemIndex, isPrefixOf, delete)



data CodeResult = CodeResult Int Int
data CommandType = Again | Back | Exit deriving (Eq, Show)

type Code = String
type History = [String]
type Step = Int


instance Show CodeResult where
  show (CodeResult x y) = show x ++ " бык; " ++ show y ++ " корова"


codeCheckWin :: CodeResult -> Bool
codeCheckWin (CodeResult x _) = x == 4

validCode :: String -> Code -> Either String CodeResult
validCode s code
  | length s /= 4       = Left "нужно минимум 4 цифры"
  | not $ all isDigit s = Left "используйте цифры от 0 до 9 включительно"
  | length (nub s) /= 4 = Left "цифры в одном числе не должны повторятся"
  | otherwise           = Right $ snd $ foldl foldCode (0, CodeResult 0 0) s
  where
    foldCode (j, CodeResult x y) v = 
      let jj = j + 1 in
      case elemIndex v code of
        Nothing -> (jj, CodeResult x y)
        Just ii ->
          if ii == j
          then (jj, CodeResult (x + 1) y)
          else (jj, CodeResult x (y + 1))

randomRange :: Int -> Float -> Int
randomRange mx r = round (fromIntegral mx * r)

main :: IO ()
main = do
  putStrLn "\nПриветствую в игре \"Быки и коровы\""
  putStrLn "Вы находитесь в главном меню"
  information
  skip >> loopChange
  skip >> putStrLn "  Удачного дня! Рекомендую выпить чаю!" >> skip

loopChange :: IO ()
loopChange =
  do
    command <- input "меню"
    if ":e" `isPrefixOf` command
    then void
    else
      do
        when (":r" `isPrefixOf` command) (skip >> regulation >> skip)
        when (":i" `isPrefixOf` command) (skip >> information >> skip)
        if ":s" `isPrefixOf` command
        then loopGameGenerator >>= \exit -> unless exit loopChange
        else putStrLn "**неизвестная команда" >> skip >> loopChange

loopGameGenerator :: IO Bool
loopGameGenerator =
  do
    code <- generator
    skip >> putStrLn "  У вас есть 8 ходов, игра началась..." >> skip
    reps <- loopGame 8 code []
    if reps == Again
    then loopGameGenerator
    else return $ reps == Exit

loopGame :: Step -> Code -> History -> IO CommandType
loopGame iter code history =
  if iter == 0
  then putStrLn ("  Вы проиграли! Верный код: " ++ code) >> again >> return Again
  else 
    do
      command <- input "игра"
      if ":e" `isPrefixOf` command
      then return Exit
      else
        if ":h" `isPrefixOf` command
        then skip >> putStrLn "  История ввода:" >> mapM_ putStrLn (reverse history) >> skip >> loopGame iter code history
        else
          if ":m" `isPrefixOf` command
          then skip >> putStrLn "  вы вышли в главное меню" >> skip >> return Back
          else 
            case validCode command code of
              Left x -> putStrLn ("**ошибка ввода: " ++ x) >> putStr "**" >> left iter >> skip >> loopGame iter code history
              Right x ->
                if codeCheckWin x
                then skip >> putStrLn "  Вы победили!" >> again >> return Again
                else 
                  let
                    xx = show x
                    iter' = iter - 1
                    name = "  " ++ command ++ " -> " ++ xx
                  in putStrLn ("результат: " ++ xx) >> left iter' >> skip >> loopGame iter' code (name : history)

void :: IO ()
void = return ()

skip :: IO ()
skip = putChar '\n'

left :: Step -> IO ()
left n 
  | n > 0     = putStrLn $ "осталось ходов: " ++ show n
  | otherwise = void

input :: String -> IO String
input line = putStr (line ++ ": ") >> getLine

again :: IO ()
again = putStrLn "  Попробуйте ещё! (новый тайный код получен)"

information :: IO ()
information = do
  putStrLn "  Команды доступные в главном меню:"
  putStrLn "  :i -> получить информацию о всех командах"
  putStrLn "  :r -> чтобы узнать правила"
  putStrLn "  :s -> чтобы начать игру"
  putStrLn "  :e -> чтобы завершить игру"
  putStrLn "  Команды доступные в игре:"
  putStrLn "  :m -> перейти в главное меню"
  putStrLn "  :h -> посмотреть историю ввода кодов"
  putStrLn "  :e -> чтобы завершить игру"

regulation :: IO ()
regulation = do
  putStrLn "  Правила игры:"
  putStrLn "  -при старте игры генерируется тайный код"
  putStrLn "  -код состоит из 4-х цифр от 0 до 9 включительно"
  putStrLn "  -причём каждая цифра встречается лишь один раз"
  putStrLn "  -вам даётся 8 ходов, чтобы отгадать этот код"
  putStrLn "  -для водиммого кода, действуют теже правила,"
  putStrLn "     что и для тайного кода"
  putStrLn "  -после каждой попытки, игра даёт подсказки"
  putStrLn "  -подсказки делятся на два типа:"
  putStrLn "  1.\"Быки\" - цифра в введённом коде существует в"
  putStrLn "    тайном коде и находится на той же позиции"
  putStrLn "    за каждое совпадения вам даётся 1 бык"
  putStrLn "  2.\"Коровы\" - цифра в введённом коде существует в"
  putStrLn "    тайном коде но не находится на той же позиции"
  putStrLn "    за каждое совпадения вам даётся 1 бык"
  putStrLn "\n  Пример работы подсказки"
  putStrLn "  -тайный код    : 1740"
  putStrLn "  -введённый код : 6704"
  putStrLn "  -подсказка     : 1 бык; 2 корова"

generator :: IO Code
generator = gener ([], ['0' .. '9']) >>= gener >>= gener >>= gener >>= \a -> return $ fst a
  where
    gener (bl, xs) =
      do
        point <- randomIO :: IO Float
        let index = randomRange (length xs - 1) point
            value = xs !! index
        return (value : bl, delete value xs)

