{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           #-}

import Yesod

import Sub
import Yesod.Helpers.Static

data Master = Master

getSub _ = Sub

mkYesod "Master" [$parseRoutes|
/sub SubR Sub getSub
|]

instance Yesod Master where approot _ = ""

main = basicHandler 3000 Master
