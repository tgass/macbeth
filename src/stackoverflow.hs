{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.Char8


main = (CL.sourceList [BS.pack "foo", BS.pack "bar"]) $$ sink -- endless loop

-- this works:
--main = (CL.sourceList [BS.pack "foobar"]) $$ sink

sink :: Sink BS.ByteString IO ()
sink = awaitForever $ \str -> do
                      liftIO $ putStrLn $ BS.unpack str -- debug
                      case (parse (string "foobar") str) of
                           Fail _ _ _ -> do
                                        liftIO $ putStr $ "f: " ++ BS.unpack str
                                        sink
                           Partial _ -> do
                                        leftover $ BS.tail str
                           Done rest final -> do
                                              liftIO $ putStr $ "d: " ++ show final ++ " // " ++ show rest
                                              sink
