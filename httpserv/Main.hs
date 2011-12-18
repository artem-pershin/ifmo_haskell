module HTTPServ
	where

import System.IO
import System.Environment (getArgs)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Directory (doesFileExist)
import Control.Concurrent (forkIO)

bad_req_str = "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\n400: Bad Request"
not_found_str = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n404: Not Found"
ok_resp_str = "HTTP/1.1 200 OK\r\nContent-Length: "

main = withSocketsDo $ do
	port <- fmap (read.head) getArgs
	sock <- listenOn $ PortNumber $ fromIntegral port
	processClients sock

getResponse fn = do
	isAlive <- doesFileExist fn
	if (isAlive)
	then do
		fh <- openFile fn ReadMode
		contents <- hGetContents fh
		seq contents (hClose fh)
		return (ok_resp_str ++ (show (length contents))  ++ "\r\n\r\n" ++ contents)
	else
		return not_found_str
	
processReq clh = do
	req <- hGetLine clh
	case ( head $ words $ req ) of
		"GET" -> do
				getResponse (tail ((words req) !! 1)) >>= (hPutStr clh)
		_     -> do hPutStrLn clh bad_req_str
	
processClients s = do
	(clh, _, _) <- accept s
	hSetBuffering clh NoBuffering
	forkIO $  processReq clh
	processClients s