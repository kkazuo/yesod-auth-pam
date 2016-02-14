{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Yesod.Auth.Pam (authPam) where

import           Data.Text          (Text, unpack)
import           System.Posix.PAM   as PAM (authenticate)
import           Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import           Yesod.Core
import           Yesod.Form         (iopt, runInputPost, textField)

pid :: Text
pid = "posixpam"

authPam :: AuthPlugin master
authPam = AuthPlugin pid dispatch login

dispatch :: Text -> [Text] -> AuthHandler master TypedContent
dispatch "POST" [] = do
    input <- lift $ runInputPost $ (,)
                <$> iopt textField "ident"
                <*> iopt textField "password"
    case input of
        (Just ident, Just password) -> do
            auth <- validate ident password
            either (failed . Just) (const (success ident)) auth
        _ -> failed Nothing
dispatch _ _ = notFound

validate :: Text -> Text -> HandlerT Auth (HandlerT master IO) (Either Int ())
validate ident password =
    liftIO $ PAM.authenticate "auth" (unpack ident) (unpack password)

success :: YesodAuth master => Text -> HandlerT Auth (HandlerT master IO) TypedContent
success ident = lift $ setCredsRedirect $ Creds pid ident []

failed :: YesodAuth master => Maybe Int -> HandlerT Auth (HandlerT master IO) TypedContent
failed _ = do
    mr <- lift getMessageRender
    setMessage $ toHtml $ mr Msg.InvalidUsernamePass
    redirect LoginR

url :: Route Auth
url = PluginR pid []

login :: MonadWidget m => (Route Auth -> Route (HandlerSite m)) -> m ()
login toMaster =
        toWidget [hamlet|
$newline never
    <div #pamlogin>
        <form method=post action=@{toMaster url} .form-horizontal>
            <div .control-group>
                <label .control-label>
                    Username
                <div .controls>
                    <input type=text name=ident required>
            <div .control-group>
                <label .control-label>
                    Password
                <div .controls>
                    <input type=password name=password required>
            <div .form-actions>
                <input type=submit .btn .btn-primary value="Login">|]
