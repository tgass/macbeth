import FicsConnection2
import FicsMessage
import System.IO

main :: IO ()
main = ficsConnection handler >>= loop


loop :: Handle -> IO ()
loop h = getLine >>= hPutStrLn h >> loop h


handler :: Handle -> FicsMessage -> IO ()
handler h cmd = case cmd of
--      Login       -> hPutStrLn h "guest"
--
--      Password    -> hPutStrLn h ""

      LoggedIn _  -> hPutStrLn h "iset seekinfo 1" >>
                     hPutStrLn h "iset seekremove 1" >>
                     hPutStrLn h "set seek 1" >>
                     hPutStrLn h "set style 12" >>
                     hPutStrLn h "iset nowrap 1" >>
                     hPutStrLn h "iset block 1"

      GuestLogin _ -> hPutStrLn h ""

      _ -> print cmd


