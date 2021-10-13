{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Server (main) where

import           Control.Monad                     (void)
import           Control.Monad.Catch               (SomeException, catch)
import           Data.Char                         (isSpace)
import           Data.Function                     ((&))
import           Network.Socket                    (Socket)
import           Streamly.Data.Fold                (Fold)
import           System.Random                     (randomIO)

import qualified Data.Map.Strict                   as Map
import qualified Streamly.Data.Array.Foreign       as Array
import qualified Streamly.Data.Fold                as Fold
import qualified Streamly.Network.Inet.TCP         as TCP
import qualified Streamly.Network.Socket           as Socket
import qualified Streamly.Prelude                  as Stream
import qualified Streamly.Unicode.Stream           as Unicode

import qualified Streamly.Internal.Data.Fold       as Fold (demuxDefault)
import qualified Streamly.Internal.Data.Time.Clock as Clock (Clock (..),
                                                             getTime)

sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
      Stream.fromList (show x ++ "\n")
    & Unicode.encodeLatin1
    & Stream.fold (Array.writeN 60)
    >>= Socket.writeChunk sk

time :: Socket -> IO ()
time sk = Clock.getTime Clock.Monotonic >>= sendValue sk

random :: Socket -> IO ()
random sk = (randomIO :: IO Int) >>= sendValue sk

def :: (String, Socket) -> IO ()
def (str, sk) = sendValue sk $ "Unknown command: " <> str

commands :: Map.Map String (Fold IO Socket ())
commands = Map.fromList
    [ ("time", Fold.drainBy time)
    , ("random", Fold.drainBy random)
    ]

demux :: Fold IO (String, Socket) ()
demux = snd <$> Fold.demuxDefault commands (Fold.drainBy def)

handler :: Socket -> IO ()
handler sk =
      Stream.unfold Socket.read sk
    & Unicode.decodeLatin1
    & Stream.wordsBy isSpace Fold.toList
    & Stream.map (, sk)
    & Stream.fold demux
    & discard

    where
        discard action = void action `catch` \(_ :: SomeException) -> return ()


server :: IO ()
server =
      Stream.unfold TCP.acceptOnPort 8091
    & Stream.fromSerial
    & Stream.mapM (Socket.forSocketM handler)
    & Stream.fromAsync
    & Stream.drain

main :: IO ()
main = server
