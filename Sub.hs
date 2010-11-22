{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           #-}

module Sub where

import Yesod
import Yesod.Helpers.Static
import Language.Haskell.TH.Syntax

subStatic = $(mkEmbedFiles "static") typeByExt
staticFiles "static"

data Sub = Sub

getStatic _ = subStatic

mkYesodSub "Sub" 
  [ClassP ''Yesod [VarT $ mkName "master"]] 
  [$parseRoutes|
/ RootR GET
/static/*Strings StaticR GET
|]


getRootR :: Yesod y => GHandler Sub y RepHtml
getRootR = do
  rtm <- getRouteToMaster
  defaultLayout [$hamlet| 
  %a!href=@rtm toStaticR hello_txt@ hello.txt
  %br
  %a!href=@rtm toStaticR dir_goodbye_html@ dir/goodbye.html
|]

toStaticR :: StaticRoute -> SubRoute
toStaticR (StaticRoute pieces _) = StaticR pieces

getStaticR = getStaticHandler subStatic toStaticR
