{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Share where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql
import qualified Prelude as P

menu :: Widget
menu = [whamlet|
    <nav .navbar .navbar-default .navbar-fixed-top>
      <div .container-fluid>
        <div .navbar-header>
          <button type="button" .navbar-toggle .collapsed data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
            <span .sr-only>Toggle navigation</span>
            <span .icon-bar></span>
            <span .icon-bar></span>
            <span .icon-bar></span>
          <a .navbar-brand href=@{ShareR}>
            <img src=@{StaticR img_sharebooks_png} alt="Sharebooks" width="20%">

        <div .collapse .navbar-collapse id="bs-example-navbar-collapse-1">
            <ul .nav .navbar-nav .navbar-left>
                <li>
                    <a>
                        Perfil
            <ul .nav .navbar-nav .navbar-right>
                <li>
                    <a href=@{CadUserR}>
                        Cadastrar-se
                    <form action=@{LogoutR}} method=post>
                        <input type="submit" value="Logout">
                <li>
                    <a href=@{LoginR}>
                        Login
                
                <li>
                    <a onclick="showlogin()">
                        Entrar
|]

getShareR :: Handler Html
getShareR = do
    sess <- lookupSession "_USR"
    --talvezUsuario <- return $ fmap (P.read . unpack) sess :: Handler (Maybe Usuario)
    defaultLayout $ do
        setTitle . fromString $ "Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"
        toWidget [julius|
          $(document).mouseup(function (e) {
            var div = $(".login");
            if (!div.is(e.target) && div.has(e.target).length === 0) {
                if (div.is(':visible')) {
                    div.toggle("slow");
                }
            }
          });

          function showlogin(){
            $(".login").show("slow");
          }
        |]
        toWidget $ $(luciusFile "templates/share.lucius")
        $(whamletFile "templates/share.hamlet")
        
getSobreR :: Handler Html
getSobreR = do
    defaultLayout $ do
        setTitle . fromString $ "Sobre Nós | Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/sobre.lucius")
        $(whamletFile "templates/sobre.hamlet")
