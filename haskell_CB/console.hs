import Control.Monad
import System.IO




main::IO()
main = do
    let case1 = do
            line <- getLine
            let a = map (\x -> read x::Integer) (words line)
            print a
    let loop = do
            hSetBuffering stdout NoBuffering
            putStr "Console>"
            command <- getLine
            case command of
                "ceva" -> case1
                "altceva" -> putStrLn "bye"
                "exit" -> return ()
                _ -> putStrLn "Invalid command"
            when (command /= "exit") loop
    loop
