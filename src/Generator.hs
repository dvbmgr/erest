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

module Generator (generate) where

import System.Directory
import System.Locale

import Data.List
import Data.Time
import Data.Maybe
import Data.Char
import Data.Functor
import Data.String.Utils
import qualified Data.Sequence as S

import Text.Markdown
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as T

import Templates
import Parse

renderMarkdown :: String -> String
renderMarkdown = replace "'" "&rsquo;" . replace "..." "&hellip;" . replace "--" "&ndash;" . replace "---" "&mdash;" . T.unpack . renderHtml . markdown def . T.pack

renderDate :: UTCTime -> String
renderDate = formatTime defaultTimeLocale "%m/%d/%Y"

sortPosts a = reverse a 

slugify :: String -> String
slugify a = map toLower $ reverse $ slugify' a []

slugify' :: String -> String -> String
slugify' [] a = a
slugify' (a:as) b = 
	if a `elem` (['a'..'z']++['A'..'Z']++['0'..'9']) then
		slugify' as (a:b)
	else
		slugify' as ('_':b)

mkPostPage :: IO Post -> IO ()
mkPostPage ipost = do
	currentTime <- getCurrentTime
	post <- ipost
	content <- renderFile [("title", fromMaybe "A post" $ title post), 
							("tags", join " " $  map (\a -> "(<a href='/tag/"++slugify a++".html'>"++a++"</a>)") $ fromMaybe [] $ tags post),
							("datetime", renderDate $ fromMaybe currentTime $ datetime post), 
							("author", fromMaybe "An author" $ author post),
							("content", renderMarkdown $ content post)] [] "templates/post.html"
	writeFile ("www/post/"++(slugify $ fromMaybe "apost" $ title post)++".html") content

mkPostsPages :: [IO Post] -> IO ()
mkPostsPages iposts = do
	mapM_ mkPostPage iposts
	currentTime <- getCurrentTime
	posts <- sortPosts <$> sequence iposts
	archive <- renderFile [("title","Posts")] [("posts", [[("title", fromMaybe "A post" $ title x),
												("slug", slugify $ fromMaybe "A post" $ title x)] | x <- posts])] "templates/archive.html"
	writeFile ("www/archives.html") archive
	index <- renderFile [("title","Posts")] [("posts", [[("title", fromMaybe "A post" $ title x),
															("slug", slugify $ fromMaybe "A post" $ title x),
															("tags", join " " $ map (\a -> "(<a href='/tag/"++slugify a++".html'>"++a++"</a>)") $ fromMaybe [] $ tags x),
															("author", fromMaybe "An author" $ author x),
															("datetime", renderDate $ fromMaybe currentTime $ datetime x),
															("content", renderMarkdown $ fst $ splitAt 300 $ content x)] | x <- posts])] "templates/posts.html"
	writeFile ("www/index.html") index

mkTagPage :: (String, [String]) -> IO ()
mkTagPage tag = do
	content <- renderFile [("title", fst tag)] [("posts",(map (\a -> [("title", a), ("slug", slugify a)]) $ snd tag))] "templates/tag.html"
	writeFile ("www/tag/"++(slugify $ fst tag)++".html") content

mkTagsPages :: [IO Post] -> IO ()
mkTagsPages iposts = do
	tagss <- mapM (fmap (\a -> (fromMaybe "A post" $ title a, fromMaybe [] $ tags a))) iposts
	let taglist = nub $ concat [([(y, [fst z | z <- tagss, elem y (snd z)]) | y <- snd x]) | x <- tagss]
	mapM_ mkTagPage taglist
	tgs <- renderFile [("title","Étiquettes")] [("tags", [[("title", fst tag),
																("slug", slugify $ fst tag)] | tag <- taglist])] "templates/tags.html"
	writeFile ("www/tags.html") tgs

mkAbout :: IO ()
mkAbout = do 
	about <- readFile "www/source/about.md"
	content <- renderFile [("title", "À propos"), ("content", renderMarkdown about)] [] "templates/about.html"
	writeFile ("www/about.html") content

generate :: IO ()
generate = do
	dir <- getDirectoryContents "www/source/posts"
	let posts = map parse ["www/source/posts/"++x | x <- dir, x !! 0 /= '.']
	mkTagsPages posts
	mkPostsPages posts
	mkAbout
	