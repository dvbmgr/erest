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

module Parse (
		parse,
		Post(..)
	) where

import Data.String.Utils
import Data.Time
import Data.Functor
import Control.Monad
import System.Environment

data Post = Post {
		title :: Maybe String,
		author :: Maybe String,
		datetime :: Maybe UTCTime,
		tags :: Maybe [String], 
		content :: String
	} deriving (Show)

splitContents :: [String] -> ([String], [String])
splitContents a = splitContents' a []

splitContents' :: [String] -> [String] -> ([String], [String])
splitContents' (x:next) passed = 
	if (all (=='-') x) && length x > 3 then
		(passed,next)
	else
		splitContents' next (x:passed)

getField :: [[String]] -> String -> Maybe String
getField [] _		= Nothing
getField (a:as) s	= 
	if (head a == s) then
		Just $ last a
	else
		getField as s

parse :: String -> IO Post
parse fname = readFile fname >>= parse'
	where
		parse' content = do
			let h = map (map strip . split " = ") (fst sted)
			return $ Post { title = (getField h "title"), 
							author = (getField h "author"), 
							datetime = (read <$> ((\a -> a ++ ":00 UTC") <$> (getField h "datetime"))) :: Maybe UTCTime,
							tags = (read <$> getField h "tags") :: Maybe [String],
							content = unlines (snd sted)}
			where
				sted = splitContents $ lines content

main = do
	args <- getArgs
	post <- parse $ args !! 0
	putStrLn $ show post
	