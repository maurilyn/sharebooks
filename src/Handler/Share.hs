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
import Database.Persist.Postgresql

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
          <ul .nav .navbar-nav .navbar-right>
            <li>
              <a href=@{CadUserR}>
                Cadastrar-se
            <li>
              <a onclick="showlogin()">
                Entrar
|]

footer :: Widget
footer = [whamlet|
    <nav .navbar .navbar-inverse .navbar-static-bottom>
      <div .container-fluid>
        <p .navbar-text .navbar-right>Sharebooks 2017. Todos os direitos reservados.&nbsp;</p>
|]

getShareR :: Handler Html
getShareR = do
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
        
formUser :: Form Usuario
formUser = renderDivs $ Usuario
        <$> areq emailField  "Email: " Nothing
        <*> areq passwordField  "Senha: " Nothing
        <*> areq textField  "Nome: " Nothing
        <*> areq textField  "CPF: " Nothing
        <*> areq textField  "Cidade: " Nothing
        <*> areq textField  "Estado: " Nothing

getCadUserR :: Handler Html
getCadUserR = do 
    (widget,enctype) <- generateFormPost formUser
    defaultLayout $ do 
        setTitle . fromString $ "Cadastre-se | Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/cadUser.lucius")
        $(whamletFile "templates/cadUser.hamlet")

postCadUserR :: Handler Html
postCadUserR = do 
    ((result,_),_) <- runFormPost formUser
    case result of
        FormSuccess usuario -> do 
            usuarioid <- runDB $ insert usuario
            redirect (PerfilUserR usuarioid)
        _ -> redirect ShareR

getPerfilUserR :: UsuarioId -> Handler Html
getPerfilUserR usuarioid = do
    usuario <- runDB $ get404 usuarioid
    defaultLayout $ do
        [whamlet|
            <h1>
                Nome: #{usuarioNome usuario}
            <h2>
                Email: #{usuarioEmail usuario}
            <h2>
                Cidade: #{usuarioCidade usuario}
            <h2>
                Estado: #{usuarioEstado usuario}
        |]