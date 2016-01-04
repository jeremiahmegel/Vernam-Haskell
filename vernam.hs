import Control.Applicative ((<*>))
import Data.Bits (xor)
import qualified Data.ByteString as B
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (deleteFirstsBy, elemIndex, intercalate, intersect)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified System.Environment as E
import System.Posix.Env.ByteString (getArgs)

type Arg = (String, B.ByteString)

vernam :: B.ByteString -> B.ByteString -> B.ByteString
vernam =
	(\ k -> B.pack . (zipWith xor (cycle k))) `on` B.unpack

argList :: [Arg] -> [(Maybe Arg, Maybe Arg)]
argList =
	foldl
		(\ args arg ->
			if (head . fst) arg == '-' then
				if isNothing $ lookup (Just arg) args then
					args ++ [(Just arg, Nothing)]
				else
					error "Duplicate argument"
			else if null args || isJust (snd $ last args) then
				args ++ [(Nothing, Just arg)]
			else
				init args ++ [(fst $ last args, Just arg)]
		)
		[]

argChoose :: [(String, Maybe Arg -> a)] -> [(Maybe Arg, Maybe Arg)] -> Maybe a
argChoose choices args =
	let
		valid =
			intersect
				(map (Just . fst) choices)
				$
				map (fmap fst . fst) args
		in
			if length valid == 1 then
				lookup (fromJust $ head valid) choices
				<*>
				(lookup
					(head valid)
					$
					map (\ (f, v) -> (fmap fst f, v)) args
				)
			else if length valid == 0 then
				Nothing
			else
				error "Argument conflict"

assertArgVal :: String -> (Arg -> a) -> Maybe Arg -> a
assertArgVal arg f s =
	if isJust s then
		f $ fromJust s
	else
		error "No value provided for argument"

argVal :: String -> (Arg -> a) -> (String, Maybe Arg -> a)
argVal arg f =
	(arg, assertArgVal arg f)

invalidFlags :: [Maybe String] -> [(Maybe Arg, Maybe Arg)] -> [(Maybe String, Maybe String)]
invalidFlags valid args =
	deleteFirstsBy
		((==) `on` fst)
		(map (\ (f, v) -> (fmap fst f, fmap fst v)) args)
		$
		map (\ f -> (f, Nothing)) valid

main = do
	args <- fmap zip E.getArgs <*> getArgs
	let
		badArgs =
			invalidFlags [Just "-i", Just "-if", Just "-k", Just "-kf"] $ argList args
		in
		if not $ null badArgs then
			error "Invalid arguments"
		else
			let
				inputArg =
					argChoose
						[
							argVal "-i" (return . snd),
							argVal "-if" (B.readFile . fst)
						]
						$
						argList args
				keyArg =
					argChoose
						[
							argVal "-k" (return . snd),
							argVal "-kf" (B.readFile . fst)
						]
						$
						argList args
			in
				if isJust keyArg then do
					input <- fromMaybe B.getContents inputArg
					key <- fromJust keyArg
					B.putStr $ vernam key input
				else
					error "Missing argument(s)"

