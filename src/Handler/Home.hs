module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
      addScript $ StaticR js_lib_nerve_js
      addScriptRemote "https://unpkg.com/react@15.3.2/dist/react-with-addons.js"
      addScriptRemote "https://unpkg.com/react-dom@15.3.2/dist/react-dom.js"
      addScript $ StaticR js_chess_controller_js
      addScript $ StaticR js_chess_model_js
      addScript $ StaticR js_chess_react_js
      addStylesheet $ StaticR css_chess_css
      $(widgetFile "homepage")

