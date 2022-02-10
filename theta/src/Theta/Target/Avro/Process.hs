{-# LANGUAGE FlexibleContexts #-}
-- | Functionality for launching and interacting with external
-- processes: streaming Theta values encoded as Avro over STDIN and
-- reading them back over STDOUT.
module Theta.Target.Avro.Process where

import           Control.Monad.Except              (MonadError)
import           Control.Monad.IO.Class            (MonadIO (liftIO))

import qualified Streamly.External.ByteString.Lazy as SBS
import           Streamly.System.Process           (processChunks)

import           Theta.Error                       (Error)
import           Theta.Target.Haskell.Conversion   (FromTheta, ToTheta,
                                                    decodeAvro, encodeAvro)

-- | Run an external process with a single input of type @a@, reading
-- a single output of type @b@.
--
-- If the process terminates with a non-zero error code, raises a
-- 'ProcessFailure' exception.
run :: (ToTheta a, FromTheta b, MonadIO m, MonadError Error m)
    => FilePath
    -- ^ Path to the executable to run.
    -> [String]
    -- ^ Command-line arguments for the executable.
    -> a
    -- ^ Input value. Written to Avro over STDIN with no Avro
    -- container.
    -> m b
    -- ^ Output value. Read as Avro from STDOUT, not expecting an Avro
    -- container.
run executable args input = do
  let inputStream = SBS.toChunks (encodeAvro input)
  out <- liftIO $ SBS.fromChunksIO (processChunks executable args inputStream)
  decodeAvro out
