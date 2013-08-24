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

module Templates (	renderString, 
					renderFile, 
					basePage, basePage'
					) where

	import Text.ParserCombinators.Parsec hiding (spaces)
	import Control.Applicative hiding (many, (<|>))
	import System.Environment
	import Control.Monad
	import System.IO.Unsafe (unsafePerformIO)
	import qualified Data.String.Utils as SUtils

	type LocalVars = [(String, String)]

	type DBVars = [(String, [[(String, String)]])]

	data Template = Variable LocalVars DBVars String
				  | HTML LocalVars DBVars String
				  | DBVariable LocalVars DBVars String String String
				  | Include LocalVars DBVars String

	instance Show Template where show = showParsed

	readLocalVars :: LocalVars -> String -> LocalVars
	readLocalVars lvars name = [l|l<-lvars, (fst l)==name]

	readDBVars :: DBVars -> String -> DBVars
	readDBVars dvars name = [l|l<-dvars, (fst l)==name]

	readExpr :: LocalVars -> DBVars -> String -> String
	readExpr lvars dvars input = case parse (parseAll lvars dvars) "" input of
	    Left err -> "[ERROR] " ++ show err
	    Right val -> concatMap showParsed val

	parseAll :: LocalVars -> DBVars -> Parser [Template]
	parseAll lvars dvars = many ((parseVariable lvars dvars) <|> (parseInclude lvars dvars) <|> (parseDBVariable lvars dvars) <|> (parseHTML lvars dvars))

	parseHTML :: LocalVars -> DBVars -> Parser Template
	parseHTML lvars dvars = HTML lvars dvars <$> many1 (noneOf "{[!" <|> try (char '{' <* notFollowedBy (char '{')) <|> try (char '[' <* notFollowedBy (char '[')) <|> try (char '!' <* notFollowedBy (char '(')))

	parseVariable :: LocalVars -> DBVars -> Parser Template
	parseVariable lvars dvars = Variable lvars dvars <$> (string "{{ " *> many (oneOf (['a'..'z']++['A'..'Z'])) <* string " }}")

	parseInclude :: LocalVars -> DBVars -> Parser Template
	parseInclude lvars dvars = do
		_ <- string "!("
		filename <- many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++['.','/','_']))
		_ <- string ")"
		return (Include lvars dvars filename)

	parseDBVariable :: LocalVars -> DBVars -> Parser Template
	parseDBVariable lvars dvars = do
		_ <- string "[[ "
		originalname <- many (oneOf (['a'..'z']++['A'..'Z']))
		_ <- string " as "
		newname <- many (oneOf (['a'..'z']++['A'..'Z']))
		_ <- string " ]](("
		content <- many (noneOf ")" <|> try (char ')' <* notFollowedBy (char ')')))
		_ <- string "))"
		return $ DBVariable lvars dvars originalname newname content

	showParsed :: Template -> String
	showParsed (DBVariable lvars dvars originalname newname content) = 
		SUtils.join "" [parseLocal a|a<-cvar]
		where 
			parseLocal :: [(String, String)] -> String
			parseLocal var = 
				parseLocal' var content
				where
					newstring :: String
					newstring = 
						SUtils.replace originalname newname content
					mkname :: (String, String) -> String	
					mkname rvar =
						"{{ "++(SUtils.join "." [newname, fst rvar])++" }}"
					parseLocal' :: [(String, String)] -> String -> String
					parseLocal' [] str = 
						str
					parseLocal' (cvar:ovar) str = 
						parseLocal' ovar (SUtils.replace (mkname cvar) (snd cvar) str)
			cvar :: [[(String, String)]]
			cvar = 
				if length match > 0 then
					snd (match !! 0)
				else
					error ("No db variable matching ``" ++ originalname ++ "'' is defined")
				where 
					match = readDBVars dvars originalname
	showParsed (HTML lvars dvars x) = x
	showParsed (Variable lvars dvars x) = 
		if length matching > 0 then
			snd $ matching !! 0
		else
			error ("No variable matching ``" ++ x ++ "'' is defined")
		where
			matching = readLocalVars lvars x
	showParsed (Include lvars dvars x) = "\n<!-- include " ++ x ++ " -->\n" ++ (unsafePerformIO $ renderFile lvars dvars x) ++ "\n<!-- /include " ++ x ++ " -->\n"

	-- Read an parse from string
	renderString :: LocalVars -> DBVars -> String -> String
	renderString lvars dvars string = readExpr lvars dvars string

	-- Read an parse from file
	renderFile :: LocalVars -> DBVars -> String -> IO String
	renderFile lvars dvars fname = do
		content <- readFile fname
		return $ renderString lvars dvars content

	basePage :: [String] -> [String] -> String -> String -> String
	basePage csss jss title content = 
		"<!doctype html><html><head><meta charset='utf-8'/><title>"++title ++"</title>"++
			(SUtils.join "" ["<link href='"++x++"' rel='stylesheet' />"|x<-csss])++
			(SUtils.join "" ["<script src='"++x++"' type='text/javascript'></script>"|x<-jss])++
		"</head><body>"++
			content ++
		"</body></html>"

	basePage' :: String -> String -> String
	basePage' title content  = basePage [] [] title content
	