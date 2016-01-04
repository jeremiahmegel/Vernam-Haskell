import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (deleteFirstsBy, elemIndex, intercalate, intersect)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Posix.Env.ByteString (getArgs)
import qualified System.Environment as E

import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.String (IsString, fromString)

type Arg = (String, B.ByteString)

vernam :: B.ByteString -> B.ByteString -> B.ByteString
vernam key message = B.pack $ zipWith xor (cycle $ B.unpack key) (B.unpack message)

argList :: [Arg] -> [(Maybe Arg, Maybe Arg)]
argList = foldl argEval []
	where argEval args arg =
		if (head . fst) arg == '-' then
			if isNothing $ lookup (Just arg) args then
				args ++ [(Just arg, Nothing)]
			else
				error "Duplicate argument"
		else if null args || isJust (snd $ last args) then
			args ++ [(Nothing, Just arg)]
		else
			init args ++ [(fst $ last args, Just arg)]

argChoose :: [(String, (Maybe Arg -> a))] -> [(Maybe Arg, Maybe Arg)] -> Maybe a
argChoose choices args =
	if length valid == 1 then
		Just $ (fromJust $ lookup (fromJust $ head valid) choices) $ fromJust $ lookup (Just $ fromJust $ head valid) (map (\(x, y) -> (fmap fst x, y)) args)
	else if length valid == 0 then
		Nothing
	else
		error "Argument conflict"
	where valid = intersect (map (Just . fst) choices) $ map (fmap fst . fst) args

assertArgVal :: String -> (Arg -> a) -> Maybe Arg -> a
assertArgVal arg f s =
	if isJust s then
		f $ fromJust s
	else
		error "No value provided for argument"

argVal :: String -> (Arg -> a) -> (String, (Maybe Arg) -> a)
argVal arg f = (arg, assertArgVal arg f)

invalidFlags :: [Maybe String] -> [(Maybe Arg, Maybe Arg)] -> [(Maybe String, Maybe String)]
invalidFlags valid args = deleteFirstsBy ((==) `on` fst) (map (\(x, y) -> (fmap fst x, fmap fst y)) args) $ map (\x -> (x, Nothing)) valid

main = do
	cleanArgs <- E.getArgs
	bsArgs <- getArgs
	let args = zip cleanArgs bsArgs
	let badArgs = invalidFlags (map Just ["-i", "-if", "-k", "-kf"]) $ argList args
	if not $ null badArgs then
		error "Invalid arguments"
	else
		let
			inputArg = argChoose [argVal "-i" (return . snd), argVal "-if" (B.readFile . fst)] $ argList args
			keyArg = argChoose [argVal "-k" (return . snd), argVal "-kf" (B.readFile . fst)] $ argList args
		in
			if isJust keyArg then do
				input <-
					if isJust inputArg then
						fromJust inputArg
					else
						B.getContents
				key <- fromJust keyArg
				B.putStr $ vernam key input
			else
				error "Missing argument(s)"

