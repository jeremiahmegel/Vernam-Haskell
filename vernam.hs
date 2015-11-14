import Data.Char (ord, chr)
import Data.Bits (xor)
import System.IO (getContents, readFile, IOMode(ReadMode))
import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex, intersect)

vernam :: String -> String -> String
vernam message key = foldl (\acc char -> acc ++ [chr (xor (ord char) (ord (key !! (length acc `mod` length key))))]) [] message

argVal :: String -> [String] -> Maybe String
argVal arg args =
	if isJust indMaybe then
		let ind = fromJust indMaybe in
			if (ind + 2) <= length args && not (head (args !! (ind + 1)) == '-') then
				Just (args !! (ind + 1))
			else
				Nothing
	else
		Nothing
	where
		indMaybe = arg `elemIndex` args

argChoose :: [(String, a)] -> [String] -> Maybe (String, a)
argChoose choices args =	
	if length valid == 1 then
		Just ((head valid), fromJust (lookup (head valid) choices))
	else if length valid == 0 then
		Nothing
	else
		error $ "Argument conflict: " ++ (show valid)
	where valid = intersect (map fst choices) args

argValChoose :: [(String, (String -> a))] -> [String] -> Maybe a
argValChoose choices args =
	if isJust argMaybe then
		let
			arg = fromJust argMaybe
			val = argVal (fst arg) args
		in
			if isJust val then
				Just ((snd arg) (fromJust val))
			else
				error $ "No value provided: " ++ fst arg
	else
		Nothing
	where
		argMaybe = argChoose choices args

main = do
	args <- getArgs
	let
		inputArg = argValChoose [("-i", return), ("-if", (\filename -> readFile filename))] args
		keyArg = argValChoose [("-k", return), ("-kf", (\filename -> readFile filename))] args
	if isJust keyArg then do
		input <- if isJust inputArg then
			fromJust inputArg
		else
			getContents
		key <- fromJust keyArg
		putStr $ vernam input key
	else
		error "Invalid arguments."

