module Vacate.Backend.Routing where

import Vacate.Prelude

import Control.Monad.Error.Class (catchError)
import HTTPure (Method(..))
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

router :: String -> HTTPure.Request -> HTTPure.ResponseM
router dist rqst = 
  catchError (rtr rqst.method rqst.path) errHandler
  where
    rtr Get [] = serveHtml (i dist"/index.html")
    rtr Get ["index.css"] = serveCss (i dist"/index.css")
    rtr Get ["main.js"] = serveJavascript (i dist"/main.js")

    rtr _ _ = HTTPure.notFound

    errHandler err = do
      log (i"-----------\nServer Error: "(show err)"\n-----------")
      HTTPure.internalServerError $ show err

serveHtml :: String -> HTTPure.ResponseM
serveHtml = serveStaticContent "text/html"
serveCss :: String -> HTTPure.ResponseM
serveCss = serveStaticContent "text/css"
serveJavascript :: String -> HTTPure.ResponseM
serveJavascript = serveStaticContent "text/javascript"

serveStaticContent :: String -> String -> HTTPure.ResponseM
serveStaticContent contentType filePath = do
  contents <- readTextFile UTF8 filePath
  HTTPure.ok' (HTTPure.header "Content-Type" (i contentType"; charset=UTF-8")) contents
