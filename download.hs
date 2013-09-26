import Network.HTTP
import Network.URI (parseURI)
import qualified Data.ByteString as B

main = do
    url <- getLine
    binary <- get url
    B.writeFile "temp" binary
  where
    get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u -> u in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
