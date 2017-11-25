{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postUsuarioR :: Handler Value
postUsuarioR = do
    usuario <- requireJsonBody :: Handler Usuario
    usuarioid <- runDB $ insert usuario
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey usuarioid)])

getTodosUsuariosR :: Handler Value
getTodosUsuariosR = do
    usuarioList <- runDB $ selectList [] [Asc UsuarioNome]
    sendStatusJSON ok200 (object ["resp" .= toJSON usuarioList])
