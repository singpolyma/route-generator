module Main where

import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
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

data Flag = Help | PathHelpers | NArgs Int | Mod String deriving (Show, Read, Eq)

flags :: [OptDescr Flag]
flags = [
		Option ['m'] ["module"] (ReqArg Mod "MODULE") "Implementation module to import.",
		Option ['n'] ["nArgs"] (ReqArg (NArgs . read) "NARGS") "Number of arguments the `route` function takes.",
		Option ['p'] ["pathHelpers"] (NoArg PathHelpers) "Generate actionPath helper functions.",
		Option ['h'] ["help"] (NoArg Help) "Show this help text."
	]

usage :: [String] -> IO ()
usage errors = do
	mapM_ (hPutStrLn stderr) errors
	name <- getProgName
	hPutStrLn stderr $ usageInfo (name ++ " [-m MODULE] [-n NARGS] [-p] <input-file>") flags

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

isDynamic :: Piece -> Bool
isDynamic Dynamic = True
isDynamic _ = False

argList :: String -> Int -> [String]
argList s n = snd $ until ((<1) . fst)
	(\(n,xs) -> (n-1, (s ++ show n):xs)) (n,[])

emitPathHelpers :: [Route] -> Int -> IO ()
emitPathHelpers rs nArgs = mapM_ emitPathHelper rs
	where
	emitPathHelper r = do
		let args = argList "arg" (length $ filter isDynamic (pieces r))
		T.putStr (target r)
		putStr "Path "
		putStr (unwords args)
		putStr " = URI \"\" Nothing ('/' : intercalate \"/\" ["
		putStr $ intercalate ", " $ snd $ foldr (\p (n,xs) -> case p of
				Dynamic -> (n-1, ("unpack $ toPathPiece arg" ++ show n):xs)
				Static s -> (n, show s : xs)
			) (length args, []) (pieces r)
		putStrLn "]) \"\" \"\""
		-- The where clause forces the typechecker to infer that our arguments
		-- are of the same type as the arguments of the action we map to.
		putStrLn "\twhere"
		putStr "\ttypeRestrict _ "
		putStr (unwords $ argList "undef" nArgs)
		putStr " = "
		T.putStr (target r)
		putStr " "
		putStrLn $ unwords $ argList "undef" nArgs ++ args
		putStrLn ""

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
	args = argList "arg" nArgs
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
		") -> return (" ++
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
	(flags, args, errors) <- fmap (getOpt RequireOrder flags) getArgs

	case (args, errors) of
		_ | Help `elem` flags -> usage errors
		(_, _:_) -> usage errors >> exitFailure
		_ | length args /= 1 -> usage errors >> exitFailure
		_ -> main' (head args) flags
	where
	main' input flags = do
		Right routes <- fmap (parseOnly parser) $ T.readFile input

		putStrLn "module Routes where"
		putStrLn ""
		mapM_ (\flag -> case flag of
				Mod m -> putStrLn $ "import " ++ m
				_ -> return ()
			) flags
		putStrLn "import Data.List (intercalate)"
		putStrLn "import Control.Monad (ap)"
		putStrLn "import Data.Text (pack, unpack)"
		putStrLn "import Network.URI (URI(..))"
		putStrLn "import Web.PathPieces (fromPathPiece, toPathPiece)"
		putStrLn "import Yesod.Routes.Dispatch (Route(..), Piece(Static, Dynamic))"
		putStrLn ""

		let nArgs = getNArgs flags
		when (PathHelpers `elem` flags) (emitPathHelpers routes nArgs)
		emitRoutes routes nArgs

	getNArgs = foldr (\flag n -> case (n,flag) of
			(0, NArgs n) -> n
			_ -> n
		) 0
