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
/static StaticR Static getStatic
|]


getRootR :: Yesod y => GHandler Sub y RepHtml
getRootR = defaultLayout [$hamlet| 
  @StaticR hello_txt@
|]

