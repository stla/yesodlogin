{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
 
import Yesod
import Yesod.Static
import GHC.Generics
import UserFrame
import qualified Data.Text as T

staticFiles "static"
 
data HelloInlineR = HelloInlineR { getStatic :: Static }
 
mkYesod "HelloInlineR" [parseRoutes|
/ HomeR GET
/login LoginR PUT
/static StaticR Static getStatic
|]
 
instance Yesod HelloInlineR
 
data Login = Login {
    _username :: String,
    _password :: String
} deriving (Show,Generic)
 
instance FromJSON Login
 
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Login"
  addScript $ StaticR jquery_jquery_1_10_2_min_js
  addStylesheet $ StaticR css_app_css
  addStylesheet $ StaticR bootstrap_bootstrap_4_0_0_min_css
  addScript $ StaticR bootstrap_bootstrap_4_0_0_min_js
  addScript $ StaticR bootstrap_bootstrap_file_input_js
  addScript $ StaticR js_app_js
  toWidget loginScript
  addStylesheet $ StaticR css_login_css
  toWidget $(whamletFile "index.hamlet") 

loginScript = [julius|
function ajaxlogin(){
    $.ajax({
        contentType: "application/json",
        processData: false,
        url: "@{LoginR}",
        type: "PUT",
        data: JSON.stringify({
                _username: $("#username").val(), 
                _password: $("#password").val()
              }),
        success: function(result) {
            if(result!="success"){
                $("#username").val("").attr("placeholder", "Username");
                $("#password").val("").attr("placeholder", "Password");
                if(result=="unknown"){
					alert("Unknown username");
                }else{
					alert("Wrong password");
                }
            }else{
                $(".login").hide();
                $(".app").show();
            }
        },
        dataType: "text"
    });
}
|]

putLoginR :: Handler String
putLoginR = do
  user <- requireJsonBody :: Handler Login
  let username = _username user
  let password = _password user
  dbPassword <- liftIO $ getUserPassword $ T.pack username
  case dbPassword of
    Nothing -> return "unknown"
    Just pwd -> return $ if pwd==password then "success" else "wrong"

main :: IO ()
main = do 
    static@(Static settings) <- static "static"
    warp 3000 $ HelloInlineR static
