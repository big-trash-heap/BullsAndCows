module LoopIO
        ( Loop
        , IOLoop
        , loopRun
        , retEnd
        , retBreak
        , retVal
        , retIO
        , retLoop
        , retRepeat
        ) where

data LoopIteration a b = LoopStop
                       | LoopRepeat
                       | LoopBreak b
                       | LoopIteration a b
                       | LoopChangeControl (IO b)

type Loop   a b = (a -> b -> IOLoop a b)
type IOLoop a b = IO (LoopIteration a b)

loopRun :: Loop a b -> a -> b -> IO b
loopRun l a b = do
  r <- l a b
  case r of
    LoopIteration a' b'  -> loopRun l a' b'
    LoopChangeControl io -> io
    LoopBreak b'         -> return b'
    LoopStop             -> return b
    LoopRepeat           -> loopRun l a b


retEnd :: IO (LoopIteration a b)
retEnd = return LoopStop

retBreak :: b -> IO (LoopIteration a b)
retBreak = return . LoopBreak

retVal :: a -> b -> IO (LoopIteration a b)
retVal a b = return $ LoopIteration a b

retIO :: IO b -> IO (LoopIteration a b)
retIO = return . LoopChangeControl

retLoop :: Loop a b -> a -> b -> IO (LoopIteration a' b)
retLoop l a b = retIO $ loopRun l a b

retRepeat :: IO (LoopIteration a b)
retRepeat = return LoopRepeat

