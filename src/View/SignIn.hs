{-# LANGUAGE OverloadedStrings    #-}
module View.SignIn (
                  makeSignInPage
                  ) where

import Data.Text (Text)
import Lucid

makeSignInPage :: Text -> Html ()
makeSignInPage errorMessage = do
    html_ $ do
        head_ $ do
            meta_ [ charset_ "UTF-8" ]
            title_ "Log in"
            link_ [ rel_ "stylesheet", href_ "css/style.css" ]
        body_ $ do
            link_ [ href_ "https://fonts.googleapis.com/css?family=Open+Sans:400,300,700", rel_ "stylesheet", type_ "text/css" ]
            div_ [ class_ "container" ] $ div_ [ class_ "frame" ] $ do
                div_ [ class_ "nav" ] $ ul_ [ class_ "links" ] $ do
                    li_ [ class_ "signin-active" ] $ a_ [ class_ "btn", href_ "/signin" ] $ "Sign in"
                    li_ [ class_ "signup-inactive" ] $ a_ [ class_ "btn", href_ "/signup" ] $ "Sign up "
                div_ [ class_ "ng-app ng-init", checked_ ] $ do
                    form_ [ class_ "form-signin", action_ "", method_ "post", name_ "form" ] $ do
                        label_ [ for_ "email" ] $ "Email"
                        input_ [ class_ "form-styling", type_ "text", name_ "email", placeholder_ "" ]
                        label_ [ for_ "password" ] $ "Password"
                        input_ [ class_ "form-styling", type_ "password", name_ "password", placeholder_ "" ]
                        input_ [ type_ "checkbox", id_ "checkbox" ]
                        label_ [ for_ "checkbox" ] $ do
                            span_ [ class_ "ui" ] $ mempty
                            "Generate all statistics"
                        div_ [ class_ "btn-animate", onclick_ "document.forms['form'].submit();"] $ a_ [ class_ "btn-signin" ] $ "Sign in"
                    label_ [ for_ "none", style_ "color: orange; text-align: center;" ] $ toHtml (errorMessage)
                    form_ [ class_ "form-signup", action_ "", method_ "post", name_ "form" ] $ mempty
                    div_ [ class_ "success" ] $ mempty
                div_ [ class_ "forgot" ] $ a_ [ href_ "#" ] $ "Forgot your password? It seems you will have to create a new account :)"
