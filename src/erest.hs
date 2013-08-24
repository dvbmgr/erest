-- Copyright (c) 2013, David Baumgartner <ch.davidbaumgartner@gmail.com>
-- 
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the 
-- Software is furnished to do so, subject to the following conditions:
--   - The above copyright notice and this permission notice shall be 
--       included in all copies or substantial portions of the Software.
--   - The Software is provided "as is", without warranty of any kind,
--       express or implied, including but not limited to the warranties
--       of merchantability, fitness for a particular purpose and noninfringement.
--       In no event shall the authors or copyright holders X be liable for any
--       claim, damages or other liability, whether in an action of contract, tort
--       or otherwise, arising from, out of or in connection with the software or
--       the use or other dealings in the Software.
--   - Except as contained in this notice, the name of the David Baumgartner shall
--       not be used in advertising or otherwise to promote the sale, use or other
--       dealings in this Software without prior written authorization from the 
--       David Baumgartner. 

import System.Directory
import System.Process
import System.IO
import System.Exit
import System.Environment 

import qualified Control.Monad as M
import Control.Exception

import Data.Functor
import Data.ConfigFile
import Data.String.Utils
import Data.Either.Utils

import Generator

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
	old <- hGetEcho stdin
	bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getPassword :: IO String
getPassword = do
	putStr "Password: "
	hFlush stdout
	pass <- withEcho False getLine
	putChar '\n'
	return pass

getCPToRead = do
	var <- readfile emptyCP ".erest"
	return $ forceEither var

handleGit (ExitSuccess, _, _) = "Git successfully did want we wanted him to do"
handleGit (ExitFailure a, _, b) = "Git returned an error["++show a++"]: "++b

fInit :: IO ()
fInit = do
	putStrLn "Do you want to use Git ?"
	git <- getLine
	if git `elem` ["y","yes","ok","true","Yes"] then do
		putStrLn "Please enter the URL of the repository"
		repurl <- getLine 
		cloned <- readProcessWithExitCode "git" ["clone",repurl,"www"] []
		putStrLn $ handleGit cloned
		putStrLn "Git’ username:"
		username <- getLine
		putStrLn "Git’ password:"
		password <- getPassword
		putChar '\n'
		writeFile (".erest") ("[git]\n"++
								"user = "++ username ++"\n"++
								"password "++ password ++"\n"++
								"repo "++ repurl ++"\n")
		putStrLn "I saved you datas"
 	else do
		createDirectoryIfMissing False "www"
		writeFile (".erest") "[DEFAULT]\nerest = 0\n"
	createDirectoryIfMissing False "www/tag"
	createDirectoryIfMissing False "www/post"
	createDirectoryIfMissing False "www/static"
	createDirectoryIfMissing False "source"
	createDirectoryIfMissing False "source/posts"
	writeFile "source/about.md" "Hey ! This is a blog which use Erest !"
	copyFile "templates/main.css" "www/static/main.css"
	putStrLn "There we are !"

fNew :: IO ()
fNew = do
	psts <- getDirectoryContents "source/posts"
	let posts = [x | x <- psts, x !! 0 /= '.']
	let fname = if length posts > 0 then show ((read (head $ split "." $ last posts) :: Int)+1) ++ ".md" else "1.md"
	cdir <- getCurrentDirectory
	cp <- getCPToRead
	if "git" `elem` sections cp then do
		putStrLn "I'm updating you Git repository"
		setCurrentDirectory "www"
		pull <- readProcessWithExitCode "git" ["pull"] []
		putStrLn $ handleGit pull
		setCurrentDirectory cdir
	else
		putStrLn "You don't use Git, so we can do other things meanwhile."
	putStrLn "What's the title of you new post ?"
	title <- getLine
	putStrLn "What are the tags ? (separated by comma)"
	tags <- map strip <$> split "," <$> getLine
	putStrLn "Who are you ?"
	author <- getLine
	writeFile ("source/posts/"++fname) ("title = "++title++"\n"++
											"tags = [\""++(join "\", \"" tags)++"\"]\n"++
											"author = "++author++"\n"++
											"-----------------")
	putStrLn $ "Hey ! I'm "++("source/posts/"++fname)

fGenerate :: IO ()
fGenerate = do
	cdir <- getCurrentDirectory
	cp <- getCPToRead
	if "git" `elem` sections cp then do
		generate 
		setCurrentDirectory "www"
		putStrLn "I'm adding your new(s) file(s) to your Git repository"
		adds <- readProcessWithExitCode "git" ["add","."] []
		putStrLn $ handleGit adds
		putStrLn "I'm commiting your new(s) file(s) to your Git repository"
		commit <- readProcessWithExitCode "git" ["commit","-a","-m","New(s) post(s)"] []
		putStrLn $ handleGit commit
		setCurrentDirectory cdir
	else
		generate
	putStrLn "Ok, finished !"

fSend :: IO ()
fSend = do
	cdir <- getCurrentDirectory
	cp <- getCPToRead
	if "git" `elem` sections cp then do
		let username = forceEither $ get cp "git" "user"
		let password = forceEither $ get cp "git" "password"
		setCurrentDirectory "www"
		putStrLn "I'm sending the changes to your Git repository"
		push <- readProcessWithExitCode "git" ["push"] (username++"\n"++password)
		putStrLn $ handleGit push
		setCurrentDirectory cdir
	else
		putStrLn "Oooops ! I think you cannot do this !"

fUpdate :: IO ()
fUpdate = do
	cdir <- getCurrentDirectory
	cp <- getCPToRead
	if "git" `elem` sections cp then do
		setCurrentDirectory "www"
		putStrLn "I'm querying the Git server"
		push <- readProcessWithExitCode "git" ["pull"] []
		putStrLn $ handleGit push
		setCurrentDirectory cdir
	else
		putStrLn "Oooops ! I think you cannot do this !"	

fHelp :: IO ()
fHelp = do
	putStrLn "erest - a static blog generator"
	putStrLn "erest init:\n\tcreate a new blog"
	putStrLn "erest new:\n\tcreate a new post"
	putStrLn "erest generate:\n\tconvert the new post to web"
	putStrLn "erest send:\n\t(git) send changes to git"
	putStrLn "erest update:\n\t(git) sync remote repository with local"

handleArg :: String -> IO ()
handleArg "init"		= fInit
handleArg "new"			= fNew
handleArg "generate"	= fGenerate
handleArg "send"		= fSend
handleArg "update"		= fUpdate
handleArg "help"		= fHelp

main :: IO ()
main = do
	args <- getArgs 
	if length args > 0 then
		handleArg $ head args
	else
		handleArg "help"