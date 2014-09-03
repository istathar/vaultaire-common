--
-- Data vault for metrics
--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

--
-- | Common program initalization for Vaultaire binaries
--
module Vaultaire.Program
(
    initializeProgram,
    Verbosity(..)
)
where

import Control.Concurrent.MVar
import Control.Monad
import GHC.Conc
import System.Environment
import System.IO (hFlush, hPutStrLn, stdout)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger
import System.Posix.Signals


--
-- Main program entry point
--

quitHandler :: MVar () -> String -> Handler
quitHandler semaphore message = Catch $ do
    hPutStrLn stdout ("\n" ++message)
    hFlush stdout
    putMVar semaphore ()

useroneHandler :: Handler
useroneHandler = Catch $ do
    hPutStrLn stdout ""
    hFlush stdout
    logger <- getLogger rootLoggerName
    let level   = getLevel logger
        level'  = case level of
                    Just DEBUG  -> INFO
                    Just INFO   -> DEBUG
                    _           -> DEBUG
        logger' = setLevel level' logger
    saveGlobalLogger logger'
    infoM "Main.useroneHandler" ("Change log level to " ++ show level')

data Verbosity = Debug | Normal | Quiet deriving Show

--
-- | Initialize a program. Call this from your 'main' program entry point
-- before doing anything else. Indicate the logging verbosity you want, along
-- with an identification of your program. Returns an MVar which will be set to
-- unit if one of the installed signal handlers catches a signal and requests a
-- shutdown as a result.
--
initializeProgram :: String -> Verbosity -> IO (MVar ())
initializeProgram banner verbosity = do
    -- Indicate startup
    name <- getProgName
    case verbosity of
        Quiet -> return ()
        _     -> putStrLn $ name ++ " (" ++ banner ++ ") starting"

    -- command line +RTS -Nn -RTS value
    when (numCapabilities == 1) (getNumProcessors >>= setNumCapabilities)

    -- Start and configure logger, deleting the default handler in favour of
    -- our own formatter outputting to stdout with timestamps. Run in Zulu time.
    setEnv "TZ" "UTC"

    let level = case verbosity of
                   Debug  -> DEBUG
                   Normal -> INFO
                   Quiet  -> WARNING

    logger  <- getRootLogger
    handler <- streamHandler stdout DEBUG
    let handler' = setFormatter handler (tfLogFormatter "%Y-%m-%dT%H:%M:%SZ" "$time  $msg")
    let logger' = (setHandlers [handler'] . setLevel level) logger
    saveGlobalLogger logger'

    debugM "Program.initialize" "Logging initialized"

    quit <- newEmptyMVar

    _ <- installHandler sigUSR1 (useroneHandler) Nothing
    _ <- installHandler sigINT  (quitHandler quit "Interrupt") Nothing
    _ <- installHandler sigTERM (quitHandler quit "Terminating") Nothing
    _ <- installHandler sigQUIT (quitHandler quit "Quit") Nothing


    debugM "Program.initialize" "Signal handlers installed"

    return quit
