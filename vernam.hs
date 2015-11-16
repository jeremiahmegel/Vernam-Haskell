import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (deleteFirstsBy, elemIndex, intercalate, intersect)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Environment (getArgs)
import System.IO (getContents, readFile)

vernam :: String -> String -> String
vernam = zipWith (\k m -> chr $ (xor `on` ord) k m) . cycle

argList :: [String] -> [(Maybe String, Maybe String)]
argList = foldl argEval []
	where argEval args arg =
		if (head arg) == '-' then
			if isNothing $ lookup (Just arg) args then
				args ++ [(Just arg, Nothing)]
			else
				error $ "Duplicate argument: " ++ arg
		else if null args || isJust (snd $ last args) then
			args ++ [(Nothing, Just arg)]
		else
			init args ++ [(fst $ last args, Just arg)]

argChoose :: [(String, (Maybe String -> a))] -> [(Maybe String, Maybe String)] -> Maybe a
argChoose choices args =
	if length valid == 1 then
		Just $ (fromJust $ lookup (fromJust $ head valid) choices) $ fromJust $ lookup (Just $ fromJust $ head valid) args
	else if length valid == 0 then
		Nothing
	else
		error $ "Argument conflict: " ++ (intercalate ", " $ map fromJust valid)
	where valid = intersect (map (Just . fst) choices) $ map fst args

assertArgVal :: String -> (String -> a) -> Maybe String -> a
assertArgVal arg f s =
	if isJust s then
		f $ fromJust s
	else
		error $ "No value provided: " ++ arg

argVal :: String -> (String -> a) -> (String, (Maybe String) -> a)
argVal arg f = (arg, assertArgVal arg f)

invalidFlags :: [Maybe String] -> [(Maybe String, Maybe String)] -> [(Maybe String, Maybe String)]
invalidFlags valid args = deleteFirstsBy ((==) `on` fst) args $ map (\x -> (x, Nothing)) valid

main = do
	args <- getArgs
	let badArgs = invalidFlags (map Just ["-i", "-if", "-k", "-kf"]) $ argList args
	if not $ null badArgs then
		error $ "Invalid arguments: " ++ (show $ map (\(a, b) -> (fromMaybe "" a, fromMaybe "" b)) $ badArgs)
	else
		let
			inputArg = argChoose [argVal "-i" return, argVal "-if" readFile] $ argList args
			keyArg = argChoose [argVal "-k" return, argVal "-kf" readFile] $ argList args
		in
			if isJust keyArg then do
				input <-
					if isJust inputArg then
						fromJust inputArg
					else
						getContents
				key <- fromJust keyArg
				putStr $ vernam key input
			else
				error "Missing argument(s)"

