{-# OPTIONS_GHC -fwarn-missing-signatures #-}
import Data.List
import Data.Maybe
import Data.Time
import Control.Monad
import Control.Exception
import Control.Applicative
import System.Process
import System.Directory
import System.Environment
import System.Exit

run :: CreateProcess -> String -> IO ()
run x err = do
    (_, _, _, h) <- createProcess x
    exitCode     <- waitForProcess h
    case exitCode of
        ExitSuccess -> return ()
        _           -> error err

main :: IO ()
main = do
    args   <- getArgs
    date   <- show . utctDay    <$> getCurrentTime
    editor <- fromMaybe "open"  <$> lookupEnv "EDITOR"
    let title     = case args of 
            [t] -> t
            _   -> error "Expected a single parameter for the title of the post."
        file      = date ++ " " ++ title
        html      = file ++ ".html"
        markdown  = file ++ ".markdown"
    exists <- doesFileExist markdown
    unless exists $ writeFile markdown $ unlines
        ['#':title
        ,"##"++date
        ]
    run (proc editor [markdown]) "Failed to run editor" 
    run (proc "pandoc" [markdown,"-o",html])
        "Could not run pandoc. Is pandoc installed, and in the path?" 
    putStrLn $ "HTML generated at '" ++ html ++ "'"

