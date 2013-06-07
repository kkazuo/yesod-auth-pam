{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Auth.Pam (authPam) where

import Control.Applicative ((<$>), (<*>))
import Data.Text (unpack)
import System.Posix.PAM (authenticate)
import Text.Hamlet (hamlet)
import Yesod.Auth (
    AuthPlugin(..),
    Creds(..),
    Route(..),
    loginErrorMessage,
    setCreds)
import Yesod.Core (
    lift,
    liftIO,
    notFound,
    toWidget)
import Yesod.Form (
    iopt,
    runInputPost,
    textField)

pid = "posixpam"

authPam =
    AuthPlugin pid dispatch login
  where
    dispatch "POST" [] = do
        input <- lift $ runInputPost $ (,)
                 <$> iopt textField "ident"
                 <*> iopt textField "password"
        case input of
            (Just ident, Just password) -> do
                auth <- validate ident password
                either failed (success ident) auth
            _ ->
                failed undefined
    dispatch _ _ = notFound

    validate ident password =
        liftIO $ authenticate "auth" (unpack ident) (unpack password)

    success ident _ =
        lift $ setCreds True $ Creds pid ident []
    failed _ =
        loginErrorMessage LoginR "Login failed."

    url = PluginR pid []
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
