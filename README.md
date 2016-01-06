# Vernam-Haskell

Computes a [Vernam Cipher](https://en.wikipedia.org/wiki/Gilbert_Vernam#The_Vernam_cipher).

This approach reads the key and plaintext message as bytestreams and hence compares corresponding bytes from each message against each other rather than corresponding characters. This allows the program to process any binary data rather than only text data.

## Usage

Provide a key via one of the following flags:

	-k  KEY             A key
	-kf KEY_FILE        The filename of a file containing a key

Provide a plaintext message either via one of the following flags or stdin:

	-i  MESSAGE         A plaintext message
	-if MESSAGE_FILE    The filename of a file containing a plaintext message

## Output

The program will output the ciphertext to stdout with no trailing newline.

## License

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

