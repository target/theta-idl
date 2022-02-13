{-# LANGUAGE FlexibleContexts #-}
-- | Functionality for launching and interacting with external
-- processes: streaming Theta values encoded as Avro over STDIN and
-- reading them back over STDOUT.
module Theta.Target.Avro.Process where

import           Control.Monad.Catch               (MonadThrow (throwM))
import           Control.Monad.Except              (MonadError)
import           Control.Monad.IO.Class            (MonadIO (liftIO))

import qualified Data.ByteString.Lazy              as LBS

import qualified Streamly.Prelude                  as Streamly

import qualified Streamly.External.ByteString.Lazy as SBS
import qualified Streamly.System.Process           as Streamly


import           Control.Monad
import           Control.Monad.Trans               (lift)
import           Theta.Error                       (Error)
import           Theta.Target.Haskell.Conversion   (FromTheta, ToTheta,
                                                    decodeAvro, decodeAvro',
                                                    encodeAvro)

-- | Run an external process with a single input of type @a@, reading
-- a single output of type @b@.
--
-- If the process terminates with a non-zero error code, raises a
-- 'ProcessFailure' exception.
run :: (ToTheta a, FromTheta b, MonadIO m, MonadError Error m)
    => FilePath
    -- ^ Path to the executable to run.
    -> [String]
    -- ^ Arguments for the executable.
    -> a
    -- ^ Input value. Written to Avro over STDIN with no Avro
    -- container.
    -> m b
    -- ^ Output value. Read as Avro from STDOUT, not expecting an Avro
    -- container.
run executable args input = do
  let inputStream = SBS.toChunks (encodeAvro input)
  out <- liftIO $ SBS.fromChunksIO $
    Streamly.processChunks executable args inputStream
  decodeAvro out

-- | Run an external process, streaming in values of type @a@ and
-- streaming out values of type @b@.
--
-- If the process terminates with a non-zero error code, raises a
-- 'ProcessFailure' exception.
--
-- Any decoding errors will result in an exception.
stream :: (ToTheta a, FromTheta b)
       => FilePath
       -- ^ Path to the executable to run.
       -> [String]
       -- ^ Arguments for the executable.
       -> Streamly.SerialT IO a
       -- ^ Stream of inputs that can be encoded to Avro with Theta.
       -> Streamly.SerialT IO b
stream executable args inputs = decodeFromChunks $
  Streamly.processChunks executable args encoded
  where encoded = SBS.toChunks . encodeAvro =<< inputs

        decodeFromChunks chunks = join $ lift $
          Streamly.unfoldrM decodeChunk <$> SBS.fromChunksIO chunks

        decodeChunk bytes
          | LBS.null bytes = pure Nothing
          | otherwise      = case decodeAvro' bytes of
              Left err                  -> throwM err
              Right (result, remainder) -> pure $ Just (result, remainder)
