module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Network.HTTP.Client (ManagerSettings (managerConnCount))
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

newtype Image = Image Text
    deriving (Eq, Show)

newtype CreateContainerOptions = CreateContainerOptions {image :: Image}
    deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int deriving (Eq, Show)

newtype ContainerId = ContainerId Text deriving (Eq, Show)

data Service = Service
    { createContainer :: CreateContainerOptions -> IO ContainerId
    , startContainer :: ContainerId -> IO ()
    }

data ContainerStatus = ContainerRunning | ContainerExited ContainerExitCode | ContainerOther Text deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText (Image image) = image

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

parseResponse ::
    HTTP.Response ByteString ->
    (Aeson.Value -> Aeson.Types.Parser a) ->
    IO a
parseResponse res parser = do
    let result = do
            value <- Aeson.eitherDecodeStrict (HTTP.responseBody res)
            Aeson.Types.parseEither parser value
    case result of
        Left e -> throwString e
        Right status -> pure status

createContainer_ :: CreateContainerOptions -> IO ContainerId
createContainer_ options = do
    manager <- Socket.newManager "/var/run/docker.sock"

    let image = imageToText options.image
    let body =
            Aeson.object
                [ ("Image", Aeson.toJSON image)
                , ("Tty", Aeson.toJSON True)
                , ("Labels", Aeson.object [("quad", "")])
                , ("Cmd", "echo hello")
                , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
                ]

    let req =
            HTTP.defaultRequest
                & HTTP.setRequestManager manager
                & HTTP.setRequestPath "/v1.43/containers/create"
                & HTTP.setRequestMethod "POST"
                & HTTP.setRequestBodyJSON body

    let parser = Aeson.withObject "create-container" $ \o -> do
            cId <- o .: "Id"
            pure $ ContainerId cId

    res <- HTTP.httpBS req
    parseResponse res parser

startContainer_ :: ContainerId -> IO ()
startContainer_ container = do
    manager <- Socket.newManager "/var/run/docker.sock"

    let path = "/v1.43/containers/" <> containerIdToText container <> "/start"

    let req =
            HTTP.defaultRequest
                & HTTP.setRequestManager manager
                & HTTP.setRequestPath (encodeUtf8 path)
                & HTTP.setRequestMethod "POST"

    void $ HTTP.httpBS req

createService :: IO Service
createService = do
    pure
        Service
            { createContainer = createContainer_
            , startContainer = startContainer_
            }