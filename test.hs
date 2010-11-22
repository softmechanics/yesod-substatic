{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           #-}

import Yesod

import Sub (Sub(..))
import qualified Sub
import Yesod.Helpers.Static

data Master = Master

getSub _ = Sub

mkYesod "Master" [$parseRoutes|
/ RootR GET
/sub SubR Sub getSub
|]

getRootR :: GHandler Master Master RepHtml
getRootR = defaultLayout [$hamlet|
  %a!href=@SubR subRootR@ subsite
|]
  where subRootR = Sub.RootR

instance Yesod Master where approot _ = ""

main = basicHandler 3000 Master
