import Graph
import Data.Maybe
import Data.Array
import Data.Array.ST as STA
import Data.Array.Unsafe (unsafeFreeze)
import Control.Applicative
import Data.List.Split
import Control.Monad
import Control.Parallel (par, pseq)
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.IntMap as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Control.Arrow
import Control.Monad.ST
import Debug.Trace
import Data.Char

{- This is horrible, sorry :( I need to learn parsec -}
parse :: Text.Text -> Vector.Vector (Vector.Vector (Int,Int))
parse txt = toVector $ runST $ build graphSize edges where
    parseInt t  = let (Right (x,_)) = Text.decimal t in x
    parseEdge   = ((Text.findIndex (== ' ') >>> fromJust >>> (+ 1)) >>= Text.splitAt) >>> (\(a,b)->(parseInt a,parseInt b))
    headIndex   = fromJust $ Text.findIndex (== '\n') txt
    graphSize   = parseInt $ Text.take headIndex txt
    edgesTxt    = Text.drop (headIndex+1) txt
    edges       = map parseEdge $ init $ Text.split (== '\n') edgesTxt
    build graphSize edges = do
        neigs <- newArray (1,graphSize) [] :: (ST s (STArray s Int [(Int,Int)]))
        forM_ edges $ \(a,b) -> do
            aNeigs <- readArray neigs a
            bNeigs <- readArray neigs b
            writeArray neigs a ((b,1):aNeigs)
            writeArray neigs b ((a,1):bNeigs)
        unsafeFreeze neigs
    toVector arr = Vector.map Vector.fromList (Vector.fromList (elems arr))

main = do
    txt <- Text.readFile "./dblp.txt"
    let graph = adjacencyListGraph $ parse txt
    putStrLn $ "BFS: " ++ show (sum $ breadthFirstSearch graph 1)

{-foo graph = sum $ map (\(i,k)->sum k) graph -}
