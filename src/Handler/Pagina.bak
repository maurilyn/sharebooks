{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pagina where

import  Import
import  Text.Lucius
import Database.Persist.Postgresql


formSerie :: Form Serie 
formSerie = renderDivs $ Serie 
        <$> areq textField "Nome: " Nothing
        <*> areq textField "Genero: " Nothing
        <*> areq intField  "Temporadas: " Nothing
        <*> areq intField  "Ano: " Nothing

getCadSerieR :: Handler Html
getCadSerieR = do 
    (widget,enctype) <- generateFormPost formSerie
    defaultLayout $ do 
        [whamlet|
            <form action=@{CadSerieR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="OK">
        |]

postCadSerieR :: Handler Html
postCadSerieR = do 
    ((result,_),_) <- runFormPost formSerie
    case result of
        FormSuccess serie -> do 
            serieid <- runDB $ insert serie 
            redirect (PerfilSerieR serieid)
        _ -> redirect HomeR

getPerfilSerieR :: SerieId -> Handler Html
getPerfilSerieR serieid = do
    serie <- runDB $ get404 serieid
    defaultLayout $ do
        [whamlet|
            <h1>
                Nome: #{serieNome serie}
            <h2>
                Gen: #{serieGenero serie}
            <h2>
                Temps: #{serieQtTemp serie}
            <h2>
                Ano: #{serieAno serie}
        |]

wid1 :: Widget
wid1 = [whamlet|
    <h1>
        Content
        <img src=@{StaticR img_download_jpg}>
|]

getPagina1R :: Handler Html
getPagina1R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                Pag1
        |]

getPagina2R :: Handler Html
getPagina2R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                Pag2
        |]

getPagina3R :: Handler Html
getPagina3R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                Pag3
        |]

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")


getColorR :: Text -> Handler Html
getColorR cor = do
    defaultLayout $ do
        toWidgetHead $ [julius|
            function ola(){
                alert("oi");
            }
        |]
        toWidget $ [lucius|
            h1{
                color:#{cor};
            }
                
        |]
        [whamlet|
            <h1>
                Ola Mundo #{cor}
            <button onclick="ola()">
                Click me!
        |]
        
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:[]) = Nothing
safeHead (x:_) = Just x

getListarR :: Handler Html
getListarR = do
    --Handler [String]
    lista <- return ["Winx", "Qdo", "Damo", "Nossas", "Mão"] :: Handler [String]
    defaultLayout $ do
        [whamlet|
            <ul>
                $forall magia <- lista
                    <li>
                        #{magia}
        |]
            

getHeadR :: String -> Handler Html
getHeadR texto = do
    --Handler (Maybe Char)
    maybeletra <- return $ safeHead texto
    defaultLayout $ do
        [whamlet|
            $maybe letra <- maybeletra
                <h1>
                    A letra eh: #{letra}
            $nothing
                <h1>
                    Erro....
            
        |]