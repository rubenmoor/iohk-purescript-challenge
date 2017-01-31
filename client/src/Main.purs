module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Either (Either (..))
import Data.Map as Map
import Data.Traversable (for_)

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
  , report :: Either String Report
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

showTracking (Tracking cs) =
  HH.table_ $
    [ for_ cs $ \ps ->
        HH.tr $
          [ for_ ps $ \(Tuple name duration) ->
              HH.text (name <> ": " <> show duration)
          ]
    ]

showReport report = HH.table_ []

ui :: forall eff. H.Component State Query (Aff (AppEffects eff))
ui = H.component { render , eval }
  where

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.p_
          [ HH.text (if st.loading then "Working ..." else "&nbsp;") ]
      , HH.div_
          [ HH.button
            [ HP.disabled st.loading
            , HE.onClick (HE.input_ RequestTracking)
            ]
            [ HH.text "Get tracking type" ]
          , HH.pre_
            case st.tracking of
              Left err -> HH.text err
              Right ttype -> [ HH.code_ [ HH.text $ showTracking ttype ] ]
          ]
      , HH.div_
          [ HH.button
              [ HP.disabled st.loading
              , HE.onClick (HE.input_ RequestReport)
              ]
              [ HH.text "Get report type" ]
          , HH.pre_
            case st.report of
              Left err -> HH.text err
              Right rtype ->
                [ HH.code_
                  [ HH.text $ showReport $ Map.fromFoldable rtype ]
                ]
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (AppEffects eff))
  eval = case _ of
    RequestTracking next -> do
      H.modify (_ { loading = true })
      response <- H.fromAff $ get "/trackingType"
      let msg = case decodeJson =<< jsonParser response.response of
            Left err -> err <> ": " <> response.response
            Right (_ :: Tracking)  -> "Successfully decoded trackingType"
      H.modify (_ { loading = false
                  , result = msg
                  })
      pure next
    RequestReport next -> do
      H.modify (_ { loading = true })
      response <- H.fromAff $ get "/reportType"
      H.modify (_ { loading = false
                  , result = response.response
                  })
      pure next

main :: Eff (AppEffects ()) Unit
main = runHalogenAff $ H.runUI ui initialState =<< awaitBody
