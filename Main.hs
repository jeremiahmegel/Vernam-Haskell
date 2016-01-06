{-
	Copyright Jeremiah Megel 2015-2016

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

import Control.Applicative ((<*>))
import Data.Bits (xor)
import qualified Data.ByteString as B
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (deleteFirstsBy, elemIndex, intercalate, intersect)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Environment (getArgs)
import qualified System.Posix.Env.ByteString as P

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
					usage
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
				usage

assertArgVal :: String -> (Arg -> a) -> Maybe Arg -> a
assertArgVal arg f s =
	if isJust s then
		f $ fromJust s
	else
		usage

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

usage :: a
usage = error $
	"Usage: vernam (-k KEY | -kf KEY_FILE) [-i MESSAGE | -if MESSAGE_FILE]\n"
	++
	"\n"
	++
	"-k  KEY             The key\n"
	++
	"-kf KEY_FILE        The filename of a file containing the key\n"
	++
	"-i  MESSAGE         The data\n"
	++
	"-if MESSAGE_FILE    The filename of a file containing the data\n"

main = do
	args <- fmap zip getArgs <*> P.getArgs
	let
		badArgs =
			invalidFlags
				[
					Just "-i",
					Just "-if",
					Just "-k",
					Just "-kf"
				]
				$
				argList args
		in
		if not $ null badArgs then
			usage
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
					usage

