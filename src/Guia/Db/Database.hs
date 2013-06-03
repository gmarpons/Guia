module Guia.Db.Database
    (Connection, connect, disconnect, startServer, stopServer, isServerRunning)
where

import System.IO
    (putStrLn)
import System.Exit
    (ExitCode(ExitSuccess))
import System.Process
    (runCommand, waitForProcess)
import qualified Database.HDBC as HDBC 
    (handleSqlError, proxiedClientName, dbServerVer, disconnect, prepare,
     execute, fetchAllRows, fromSql, toSql, commit, handleSql)
import Database.HDBC.PostgreSQL
    (Connection, connectPostgreSQL)

serverExecutable, serverDataDir, serverLogFile :: String
serverExecutable = "/usr/lib/postgresql/9.1/bin/pg_ctl "
serverDataDir =    "$HOME/Private/guia/data "
serverLogFile =    "$HOME/Private/guia/data/pgserver.log "

startServer :: IO ()
startServer = 
    do
      putStrLn "Trying to start DB server."
      procHandle <- runCommand cmd
      exitCode <- waitForProcess procHandle
      putStrLn (if exitCode == ExitSuccess then "DB server started."
                else "Error on server stratup: " ++ show exitCode)
    where
      cmd :: String
      cmd = serverExecutable ++ "start " ++ "-D " ++ serverDataDir
            ++ "-l " ++ serverLogFile ++ "-o '-k /tmp -p 6543'"

isServerRunning :: IO Bool
isServerRunning =
    do
      putStrLn "Asking DB server status."
      procHandle <- runCommand cmd
      exitCode <- waitForProcess procHandle
      return $ exitCode == ExitSuccess
    where
      cmd :: String
      cmd = serverExecutable ++ "status " ++ "-D " ++ serverDataDir

stopServer :: IO ()
stopServer = 
    do
      putStrLn "Trying to stop DB server."
      procHandle <- runCommand cmd
      exitCode <- waitForProcess procHandle
      putStrLn (if exitCode == ExitSuccess then "DB server stopped."
                else show exitCode)
    where
      cmd :: String
      cmd = serverExecutable ++ "stop " ++ "-D " ++ serverDataDir

connect :: IO (Maybe Connection)
connect =
    HDBC.handleSql (\_ -> return Nothing) $ do
      putStrLn "Trying to connect to the DB."
      conn <- connectPostgreSQL dbConnectionParams
      putStrLn $ "Connected to DB " ++ HDBC.proxiedClientName conn ++ 
                   ", version " ++ HDBC.dbServerVer conn ++ "."
      return $ Just conn
    where
      dbConnectionParams :: String                                     
      dbConnectionParams = "host=/tmp port=6543 dbname=guia user=guia" 

disconnect :: Connection -> IO ()
disconnect conn =
    HDBC.handleSqlError $ do
      HDBC.disconnect conn
      putStrLn "Disconnected from the DB."
