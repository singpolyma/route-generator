module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate)
import Data.Char (isUpper, isSpace)
import Data.Maybe (catMaybes, isJust)
import Yesod.Routes.Dispatch (Piece(Static, Dynamic))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text
import Control.Applicative

data Route = Route {
		method :: Text,
		pieces :: [Piece],
		multi  :: Bool,
		target :: Text
	}
	deriving (Show)

instance Show Piece where
	show Dynamic = "Dynamic"
	show (Static s) = "Static (pack " ++ show (T.unpack s) ++ ")"

emitRoutes :: [Route] -> Int -> IO ()
emitRoutes rs nArgs = do
	-- We want to be polymorphic in the parameter to route, so just let
	-- the inference engine do it all
	-- putStrLn "routes :: [Route a]"
	putStr "routes "
	putStr $ unwords args
	putStrLn " = ["
	putStrLn $ intercalate ",\n" $ map showRoute rs
	putStrLn "\t]"
	where
	args = args' nArgs
	args' 0 = []
	args' n = ("arg" ++ show n) : args' (n-1)
	showRoute r =
		"\t\tRoute {\n" ++
		"\t\t\trhPieces = " ++
		show (Static (method r) : pieces r) ++
		",\n" ++

		"\t\t\trhHasMulti = " ++
		show (multi r) ++
		",\n" ++

		"\t\t\trhDispatch = (\\(" ++
		piecesPattern (pieces r) ++
		") -> (return $ " ++
		T.unpack (target r) ++
		" " ++ unwords args ++
		")" ++
		piecesAp (pieces r) ++
		")\n" ++

		"\t\t}"

	piecesAp pieces = concat $ fst $ foldr (\p (ps,c) -> case p of
			Dynamic -> ((" `ap` (fromPathPiece val" ++ show c ++ ")"):ps, c+1)
			Static _ -> (ps, c)
		) ([],0::Int) pieces

	piecesPattern pieces = intercalate ":" $ ("_":) $ fst $
		foldr (\p (ps,c) -> case p of
			Dynamic -> (("val" ++ show c):ps, c+1)
			Static _ -> ("_":ps, c)
		) (["_"],0::Int) pieces

parser :: Parser [Route]
parser = many1 $ do
	skipSpace
	m <- method
	skipSpace
	p <- pieces
	multi <- fmap isJust $ option Nothing (fmap Just (char '*'))
	skipSpace
	_ <- char '='
	_ <- char '>'
	skipSpace
	t <- target
	skipWhile (\x -> isSpace x && not (isEndOfLine x))
	endOfLine
	return $ Route m p multi t
	where
	target = takeWhile1 (not . isSpace)
	method = takeWhile1 isUpper
	pieces = fmap catMaybes $ many1 $ do
		_ <- char '/'
		option Nothing (fmap Just piece)
	piece = dynamic <|> static
	static = fmap Static (takeWhile1 (\x -> x /= '/' && x /= '*' && not (isSpace x)))
	dynamic = char ':' >> return Dynamic

main :: IO ()
main = do
	args <- getArgs
	main' args
	where
	main' [input, mod, nArgs] = do
		Right routes <- fmap (parseOnly parser) $ T.readFile input

		putStrLn "module Routes where"
		putStrLn ""
		putStrLn $ "import " ++ mod
		putStrLn "import Control.Monad (ap)"
		putStrLn "import Data.Text (pack)"
		putStrLn "import Web.PathPieces (fromPathPiece)"
		putStrLn "import Yesod.Routes.Dispatch (Route(..), Piece(Static, Dynamic))"
		putStrLn ""
		emitRoutes routes (read nArgs)
	main' [input, mod] = main' [input, mod, "0"]
	main' _ =
		hPutStrLn stderr "Usage: ./routeGenerator <input file> <implementation module> [<number of extra args>]"
