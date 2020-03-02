module Import
    ( module Import
    ) where

import           ClassyPrelude        as Import hiding (parseTime)
import           Yesod                as Import hiding (Route (..))
import           Yesod.Auth           as Import
import           Text.Julius          as Import (rawJS)
import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import           Utils                as Import

myFormSettings :: MyForm App AppMessage res
myFormSettings = MyForm
               { mfTitle = MsgUnknown
               , mfEnctype = UrlEncoded
               , mfInfoMsg = (MsgFormOneError, MsgFormNErrors, MsgFormSuccess)
               , mfRoute = HomeR
               , mfFields = [whamlet|Unitialized form! This should not happen.|]
               , mfActions = [whamlet|<input type=submit>|]
               , mfResult = error "Unitialized result for MyForm"
               }
