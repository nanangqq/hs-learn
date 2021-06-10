check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x==c then c else y | (x, y) <- zip word display])

turn :: String -> String -> Int -> IO()
turn word display n = 
  do  if n==0
         then putStrLn "You lose" -- 들여쓰기 앞에 if 시작된 것 보다 뒤에 있기만 하면 되는 듯.
       else if word==display
             then putStrLn "You win!"
              else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
--   do putStrLn (display ++ "  " ++ take n (repeat '*'))
  do putStrLn (display ++ "  " ++ replicate n '*')
     putStr "  Enter your guess: "
     q <- getLine
     
     if (length q)==0
     then mkguess word display n
     else do
             let (correct, display') = check word display (q!!0)
             let n' = if correct then n else n-1
             turn word display' n'

starman :: String -> Int -> IO ()
-- starman word n = turn word ['-' | x <- word] n
starman word n = turn word (replicate (length word) '-') n