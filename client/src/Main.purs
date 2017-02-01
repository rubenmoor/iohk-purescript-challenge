module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Either (Either (..))
import Data.Functor ((<#>))
import Data.Time.Duration (Seconds (..))
import Data.Tuple (Tuple (..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

import Network.HTTP.Affjax (AJAX, get)

import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Parser (jsonParser)

import Lib (Tracking (..), Report (..))

type State =
  { loading :: Boolean
  , tracking :: Either String Tracking
  , report :: Either String (Report String Seconds)
  }

initialState :: State
initialState =
  { loading: false
  , tracking: Left "Please make request"
  , report: Left "Please make request"
  }


data Query a
  = RequestTracking a
  | RequestReport a

type AppEffects eff = H.HalogenEffects (ajax :: AJAX | eff)

showTracking :: forall a b. Tracking -> HH.HTML a b
showTracking (Tracking t) =
  HH.table
    [ HP.class_ $ HH.className "table table-bordered" ] $
      t.unTracking <#> \ps ->
        HH.tr_ $ ps <#> \(Tuple name duration) ->
          HH.td_ [ HH.text (name <> ": " <> showSeconds duration) ]

showReport :: forall a b. Report String Seconds -> HH.HTML a b
showReport (Report r) = HH.ul
    [ HP.class_ $ HH.className "list-inline" ] $
      r.unReport <#> \(Tuple name duration) ->
        HH.li_ [ HH.text (name <> ": " <> showSeconds duration)]

showSeconds :: Seconds -> String
showSeconds (Seconds s) = show s <> "s"

ui :: forall eff. H.Component State Query (Aff (AppEffects eff))
ui = H.component { render , eval }
  where

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.p_
          [ HH.text (if st.loading then "Working ..." else " ") ]
      , HH.div_
          [ HH.button
            [ HP.disabled st.loading
            , HE.onClick (HE.input_ RequestTracking)
            ]
            [ HH.text "Get tracking type" ]
          , case st.tracking of
              Left err -> HH.text err
              Right ttype -> showTracking ttype
          ]
      , HH.div_
          [ HH.button
              [ HP.disabled st.loading
              , HE.onClick (HE.input_ RequestReport)
              ]
              [ HH.text "Get report type" ]
          , case st.report of
              Left err -> HH.text err
              Right rtype -> showReport rtype
                -- [ HH.code_ [ showReport $ Map.fromFoldable rtype.unReport ]]
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (AppEffects eff))
  eval = case _ of
    RequestTracking next -> do
      H.modify (_ { loading = true })
      response <- H.fromAff $ get "/trackingType"
      let t = decodeJson =<< jsonParser response.response
      H.modify (_ { loading = false
                  , tracking = t
                  })
      pure next
    RequestReport next -> do
      H.modify (_ { loading = true })
      response <- H.fromAff $ get "/reportType"
      let r = decodeJson =<< jsonParser response.response
      H.modify (_ { loading = false
                  , report = r
                  })
      pure next

main :: Eff (AppEffects ()) Unit
main = runHalogenAff $ H.runUI ui initialState =<< awaitBody
