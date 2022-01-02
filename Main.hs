import qualified GameCode4 as C
import qualified LoopIO as L
import System.Random (StdGen)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  l <- languageRead
  stdgen <- C.genrStdIO
  ioCurrLang l
  putStr "\n\n"
  languagePrint l langWellcom
  putChar '\n'
  languagePrint l langInMenu
  putChar '\n'
  languagePrintList l langInfo
  (l', _) <- L.loopRun loopMenu () (l, stdgen)
  languagePrint l' langBye
  putChar '\n'



-- loopMenu

loopMenu :: L.Loop () (Language, StdGen)
loopMenu _ d@(l, s) = do
  command <- ioInput l langInputMenu
  
  let may = inputHandle command
  if may == Nothing
  then do
    languagePrint l langNoCommand
    L.retRepeat
  else do
    let (Just c) = may
    case c of
      's' -> newGame d
      'e' -> L.retEnd
      'i' -> languagePrintList l langInfo >> L.retRepeat
      'r' -> languagePrintList l langRegulation >> L.retRepeat
      'l' -> do
        l' <- languageRead
        ioCurrLang l'
        L.retVal () (l', s)
      _   -> do
        languagePrint l langNoCommand
        L.retRepeat




-- loopGame

type Step    = Int
type History = [C.Code]

data GameLoop = GameLoop { code :: C.Code
                         , history :: History
                         , step :: Step 
                         }

newGame :: (Language, StdGen) -> L.IOLoop a (Language, StdGen)
newGame (l, s) = do
  let (code', s') = C.genrCode s
  let d = GameLoop { code = code'
                   , history = []
                   , step = 0
                   }
  putChar '\n'
  languagePrint l langGameBegin
  L.retLoop loopGame d (l, s')

loopGame :: L.Loop GameLoop (Language, StdGen)
loopGame g d@(l, s) = do
  let hist = history g
  let cd = code g

  command <- ioInput l langInputGame
  case C.readCode command of
    
    C.Success c -> 
      if isUnique c hist
      then
        do
          let comp = C.compCode cd c
          if comp == C.Win
          then do
            languagePrint l (langWin c)
            languagePrint l langTryAgain
            newGame d
          else do
            let nstep = step g + 1
            if nstep == 8
            then do
              languagePrint l langEndStep
              languagePrint l $ langGameLoss cd
              languagePrint l langTryAgain
              newGame d
            else do
              languagePrint l $ langLostStep (8 - nstep)
              languagePrint l $ langHint comp
              L.retVal (g { history = c : hist, step = nstep }) d
      else languagePrint l langAgainCode >> L.retRepeat
    
    _ -> do
      let may = inputHandle command
      if may == Nothing
      then languagePrint l langNoCommand >> L.retRepeat
      else do
        putChar '\n'
        let Just c = may
        case c of
          'e' -> L.retEnd
          'm' -> languagePrint l langMenu >> L.retLoop loopMenu () d
          'r' -> languagePrint l (langGameLoss cd) >> newGame d
          'h' -> printHistory l cd hist >> L.retRepeat
          _   -> do
            languagePrint l langNoCommand
            L.retRepeat



-- other
ioInput :: Language -> LangString -> IO String
ioInput l ls =
  let txt = language l ls
  in putChar '\n' >> input txt >>= \t -> putChar '\n' >> return t

ioCurrLang :: Language -> IO ()
ioCurrLang l =
  let txt = language l langCurrentLang
  in putStrLn $ "choice: " ++ txt ++ (" (" ++ show l ++ ")")

find :: Eq a => (a -> Bool) -> [a] -> Bool
find _ []     = False
find f (x:xs) | f x       = True
              | otherwise = find f xs

isUnique :: C.Code -> History -> Bool
isUnique c h = not $ find (== c) h

printHistory :: Language -> C.Code -> History -> IO ()
printHistory l _ [] = languagePrint l langHistoryEmpty
printHistory l c h  = languagePrint l langHistory >> mapM_ mf (hist h 1)
  where
    mf (c', i) = do
      putStr "  "
      putStrLn $ language l langStep ++ show i
      putStr "  "
      putStrLn $ language l langCode ++ C.showCode c'
      putStr "    "
      languagePrint l $ langHint (C.compCode c c')
    hist [] _     = []
    hist (x:xs) i = (x, i) : hist xs (i + 1)




-- language interface (two language ru|en)

data Language         = RU | EN
data LanguageChoice a = LanguageChoice { choiceRU :: a, choiceEN :: a }

instance Show Language where
  show RU = "ru"
  show EN = "en"

type LangString       = LanguageChoice String
type LangStringList   = LanguageChoice [String]

language :: Language -> LanguageChoice a -> a
language RU = choiceRU
language EN = choiceEN

languageRead :: IO Language
languageRead = do
  putStrLn "choice: ru|en<auto> (write \"ru\" or \"en\")"
  l <- input "choice"
  return $ choice l
  where
    choice "ru" = RU
    choice _    = EN

languagePrint :: Language -> LanguageChoice String -> IO ()
languagePrint l = putStrLn . language l

languagePrintList :: Language -> LanguageChoice [String] -> IO ()
languagePrintList l = mapM_ putStrLn . language l



-- input

input :: String -> IO String
input = (>> getLine) . putStr . (++ ": ")

inputHandle :: String -> Maybe Char
inputHandle command =
  case command of
    x:[] -> Just x
    _    -> Nothing




-- Language text

langCode :: LangString
langCode =
  let 
    ru = "Код "
    en = "Code "
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langStep :: LangString
langStep =
  let 
    ru = "Ход "
    en = "Step "
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langHistoryEmpty :: LangString
langHistoryEmpty =
  let 
    ru = "  История кодов пуста"
    en = "  Code history is empty"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langHistory :: LangString
langHistory =
  let 
    ru = "  История:"
    en = "  History:"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langHint :: C.CodeCheck -> LangString
langHint s =
  let 
    (b, c) = C.getResult s
    ru = "Подсказка: " ++ show b ++ " быков; " ++ show c ++ " коров;"
    en = "Hint: " ++ show b ++ " bulls; " ++ show c ++ " cows;"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langLostStep :: Step -> LangString
langLostStep s =
  let 
    ru = "Осталось " ++ show s ++ " ходов"
    en = show s ++ " moves left"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langAgainCode :: LangString
langAgainCode =
  let 
    ru = "Такой код уже был использован!"
    en = "This code has already been used!"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langEndStep :: LangString
langEndStep =
  let 
    ru = "Вы проиграли!"
    en = "You lost!"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langTryAgain :: LangString
langTryAgain =
  let 
    ru = "Попробуйте сыграть ещё!"
    en = "Try again!"
  in LanguageChoice { choiceRU = ru, choiceEN = en }


langWin :: C.Code -> LangString
langWin c =
  let 
    ru = "Вы победили! Ваш секретный код: " ++ C.showCode c
    en = "You won! Your secret code:" ++ C.showCode c
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langMenu :: LangString
langMenu =
  let 
    ru = "Вы перешли в меню"
    en = "You went to the menu"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langGameLoss :: C.Code -> LangString
langGameLoss c =
  let 
    ru = "Не разгаданный код: " ++ C.showCode c
    en = "Unsolved code: " ++ C.showCode c
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langGameBegin :: LangString
langGameBegin =
  let 
    ru = "У вас есть 8 ходов, игра началась..."
    en = "You have 8 moves, the game has begun ..."
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langInputMenu :: LangString
langInputMenu =
  let 
    ru = "меню"
    en = "menu"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langInputGame :: LangString
langInputGame =
  let 
    ru = "игра"
    en = "game"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langNoCommand :: LangString
langNoCommand =
  let 
    ru = "Не существует такой команды"
    en = "There is no such command"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langWellcom :: LangString
langWellcom =
  let 
    ru = "Приветствую в игре \"Быки и коровы\""
    en = "Welcome to the game \"Bulls and Cows\""
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langBye:: LangString
langBye =
  let 
    ru = "Спасибо за игру!"
    en = "Thanks for playing!"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langCurrentLang :: LangString
langCurrentLang =
  let 
    ru = "Выбран русский язык"
    en = "English language selected"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langInMenu :: LangString
langInMenu =
  let 
    ru = "Вы находитесь в меню"
    en = "You are on the menu"
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langInfo :: LangStringList
langInfo =
  let 
    ru = [ "  Команды доступные в меню:"
         , "  i -> получить информацию о всех командах"
         , "  r -> чтобы узнать правила"
         , "  s -> чтобы начать игру"
         , "  e -> чтобы завершить игру"
         , "  l -> сменить язык"
         , "  Команды доступные в игре:"
         , "  m -> перейти в главное меню"
         , "  h -> посмотреть историю ввода кодов"
         , "  e -> чтобы завершить игру"
         , "  r -> начать игру заного"
         ]
    en = [ "  Commands available in the menu:"
         , "  i -> get information about all commands"
         , "  r -> to find out the rules"
         , "  s -> to start the game"
         , "  e -> to end the game"
         , "  l -> change language"
         , "  Commands available in the game:"
         , "  m -> go to main menu"
         , "  h -> view the history of entering codes"
         , "  e -> to end the game"
         , "  r -> start the game again"
         ]
  in LanguageChoice { choiceRU = ru, choiceEN = en }

langRegulation :: LangStringList
langRegulation =
  let 
    ru = [ "  Правила игры:"
         , "  -при старте игры генерируется тайный код"
         , "  -код состоит из 4-х цифр от 0 до 9 включительно"
         , "  -причём каждая цифра встречается лишь один раз"
         , "  -вам даётся 8 ходов, чтобы отгадать этот код"
         , "  -для водиммого кода, действуют теже правила,"
         , "    что и для тайного кода"
         , "  -после каждой попытки, игра даёт подсказки"
         , "  -подсказки делятся на два типа:"
         , "  1.\"Быки\" - цифра в введённом коде существует в"
         , "    тайном коде и находится на той же позиции"
         , "    за каждое совпадения вам даётся 1 бык"
         , "  2.\"Коровы\" - цифра в введённом коде существует в"
         , "    тайном коде но не находится на той же позиции"
         , "    за каждое совпадения вам даётся 1 коворва"
         , "\n  Пример работы подсказки"
         , "  -тайный код    : 1740"
         , "  -введённый код : 6704"
         , "  -подсказка     : 1 бык; 2 корова"
         ]
    en = [ "  Rules of the game:"
         , "  -at the start of the game, a secret code is generated"
         , "  -code consists of 4 digits from 0 to 9 inclusive"
         , "  -with each digit occurs only once"
         , "  -you are given 8 moves to guess this code"
         , "  -for the entered code, the same rules apply,"
         , "    as for the secret code"
         , "  -after each attempt, the game gives hints"
         , "  -Tips are of two types:"
         , "  1. \"Bulls\"- a digit in the entered code exists in"
         , "    secret code and is in the same position"
         , "    for each match you are given 1 bull"
         , "  2. \"Cows\"- a digit in the entered code exists in"
         , "    secret code but not in the same position"
         , "    for each match you are given 1 cow"
         , "\n An example of how the tooltip works"
         , "  -secret code  : 1740"
         , "  -entered code : 6704"
         , "  -hint         : 1 bull; 2 cow"
         ]
  in LanguageChoice { choiceRU = ru, choiceEN = en }

