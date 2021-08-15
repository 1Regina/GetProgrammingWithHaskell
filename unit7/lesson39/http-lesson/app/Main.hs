-- {-# LANGUAGE OverloadedStrings -#} -- Quick check 39.1 Either add in package.yaml or .cabal under extension or add in Main.hs as language pragma

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple --  part of the http-conduit package.
import Network.HTTP.Types.Status -- Q39.2

-- import Lib

-- main :: IO ()
-- main = print "hi"


myToken :: BC.ByteString
myToken = "djvJJFRbJEWhxBsnxzUHnIhIZasQHGLc"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

---------------------------------------------
-- GHCi> :t getResponseStatusCode <$> responsegetResponseStatusCode <$> response:: Control.Monad.IO.Class.MonadIO f => f Int

-- getResponseStatusCode method A
--  GHCi> getResponseStatusCode <$> response

-- getResponseStatusCode method B
-- response <- httpLBS "http://news.ycombinator.com"
-- GHCi> getResponseStatusCode response

-----------------------------------------------
-- Quick check 39.2 There’s also a getResponseHeader function
-- getResponseHeader method A
--  GHCi> getResponseHeader <$> response

-- getResponseHeader method B
-- response <- httpLBS "http://news.ycombinator.com"
-- GHCi> getResponseHeader response


-- Your request to the API requires you to
        --    1. Add your token to the header.
        --    2. Specify the host and path for your request.
        --    3. Make sure you’re using the GET method for your request.
        --    4. Make sure your request works for an SSL connection.
buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestHeader "token" [token] -- REM TO UPDATE TOKEN
                                  $ setRequestPath path
                                  $ setRequestSecure True
                                  $ setRequestPort 443
                                  $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

-- TYPE OF setRequestMethod
-- *Lib Network.HTTP.Simple> :t setRequestMethod
-- setRequestMethod
--   :: Data.ByteString.Internal.ByteString -> Request -> Request

-- TYPE of setRequestHeader
-- *Lib Network.HTTP.Simple> :t setRequestHeader
-- setRequestHeader
--   :: http-types-0.12.3:Network.HTTP.Types.Header.HeaderName
--      -> [Data.ByteString.Internal.ByteString] -> Request -> Request
------------------------------------------------------------------
-- basic application that can fetch data from the REST API and write it to a file
main :: IO ()
main = do
    response <- httpLBS request -- (origina app) but for Q39.1 request = requestNoSSL
    let status = getResponseStatusCode response
    if status == 200
        then do
            print "saving request to file"
            let jsonBody = getResponseBody response
            L.writeFile "data.json" jsonBody
        -- else print "request failed with error" -- original app
        else print $ statusMessage $ getResponseStatus response
-------------------------------------------------------
-- Q39.1    Build a function buildRequestNOSSL that works exactly like buildRequest, only it doesn’t support SSL.
buildRequestNoSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNoSSL token host method path  = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestHeader "token" [token] -- REM TO UPDATE TOKEN
                                    $ setRequestPath path
                                    $ setRequestSecure False
                                    $ setRequestPort 80
                                    -- $ setRequestPath path -- model ans place this here but either works
                                    $ defaultRequest

requestNoSSL :: Request
requestNoSSL = buildRequestNoSSL myToken noaaHost "GET" apiPath

-- final step: change response <- httpLBS request  in main to response <- httpLBS requestNoSSL
-----------------------------------------------------

-- Q39.2    Improve the output of your code when something goes wrong. *getResponseStatus* will give you a data type including both the *statusCode* and the *statusMessage*. Fix main so that if you do get a non-200 *statusCode*, you print out the appropriate error.[Note that you also need to add http-types to your project and import Network.HTTP.Types.Status]
-- change else print "request failed with error" in main to else print $ statusMessage $ getResponseStatus response
