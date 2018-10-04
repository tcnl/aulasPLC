  leEscreve = do getLine
                 l <- getLine
                 putStrLn (reverse l)
                 putStrLn (reverse l)
                 putStrLn (reverse l)

main = do getLine
   line <- getLine
   if null line
   then (do
          l <- return "String Vazia"
          putStrLn l 
        )
    else (do
        putStrLn $ reverseWords line
        main
        )