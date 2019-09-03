import Network.HTTP.Conduit.Downloader
import Data.ByteString as B
import Data.ByteString.UTF8 as BLU 
import qualified Data.ByteString.Char8 as C
import System.IO as S
import GHC.List as L
import Data.Foldable as F
import Data.Text.Encoding as TSE
import Text.HTML.Parser
import Control.Monad

-- file = {Name, text}

tokenize :: Functor f => f B.ByteString -> f [Token]
tokenize text = parseTokens <$>   -- converting from Text to Tokens
    TSE.decodeUtf8 <$>          -- converting from ByteString to Text
    text

--parseLinks :: [Token] -> [B.ByteString]
parseLinks = undefined

removeCommon :: [B.ByteString] -> [B.ByteString] -> [B.ByteString]
removeCommon xs ys = L.filter (\x -> x `F.notElem` xs) ys

crawlLink :: [B.ByteString] -> [B.ByteString] -> [B.ByteString]
crawlLink v [] = v
crawlLink visited unvisited@(url:_) =
    crawlLink (visited ++ [url]) (unvisited ++ newLinks)
        where newLinks = removeCommon visited $ 
                parseLinks $ 
                tokenize <$> 
                urlGetContents $ 
                BLU.toString url

main :: IO()
main = S.putStrLn "none"