module Main where

import Prelude

-- import Control.Monad.Aff.AVar as Aff
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
--
-- import Data.Argonaut.Core (Json, jsonEmptyArray)
-- import Data.Argonaut.Parser (jsonParser)
-- import Data.Either (Either(..))
-- import Data.Functor.Coproduct (Coproduct)
-- import Data.Json.JTable as J
-- import Data.Maybe (Maybe(..))
--
-- import DOM (DOM)
--
-- import Halogen as H
-- import Halogen.HTML.Events.Indexed as HE
-- import Halogen.HTML.Indexed as HH
-- import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

-- old
import Network.HTTP.Affjax (AJAX, get)
import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Class (liftAff)

import Halogen.Effects (HalogenEffects)
import Control.Monad.Aff (Aff, launchAff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Network.HTTP.Affjax as AX

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, username: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.h1_ [ HH.text "Lookup GitHub user" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter username:" ]
          , HH.input
              [ HP.value st.username
              , HE.onValueInput (HE.input SetUsername)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeRequest)
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
          case st.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    SetUsername username next -> do
      H.modify (_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify (_ { loading = true })
      response <- liftAff $ AX.get ("https://api.github.com/users/" <> username)
      H.modify (_ { loading = false, result = Just response.response })
      pure next

-- getTrackingType :: forall e.
--                    Eff ( err :: EXCEPTION , ajax :: AJAX | e)
--                        ( Canceler ( ajax :: AJAX | e) )
getTrackingType = launchAff $ do
  res <- get "/trackingType"
  liftEff $ log $ "GET /trackingType response: " <> res.response
  pure res

-- getReportType :: forall e.
--                  Eff ( err :: EXCEPTION , ajax :: AJAX | e)
--                      ( Canceler ( ajax :: AJAX | e))
getReportType = launchAff $ do
  res <- get "/reportType"
  liftEff $ log $ "GET /reportType response: " <> res.response
  -- TODO: pure $ Map.fromList res
  pure res

-- type DemoState = String
-- data DemoQuery a = SetJsonText String a
--
-- type DemoSlot = Unit
-- type DemoInstalledState g = H.ParentState DemoState Json DemoQuery J.JTableQuery g DemoSlot
-- type DemoComponent g = H.Component (DemoInstalledState g) (Coproduct DemoQuery (H.ChildF DemoSlot J.JTableQuery)) g
-- type DemoRender g = DemoState -> H.ParentHTML Json DemoQuery J.JTableQuery g DemoSlot
-- type DemoEval g = DemoQuery ~> H.ParentDSL DemoState Json DemoQuery J.JTableQuery g DemoSlot
--
-- ui :: forall g. (Functor g) => J.JTableOpts -> DemoComponent g
-- ui opts = H.parentComponent { render, eval, peek: Nothing }
--   where
--   render :: DemoRender g
--   render jsonString =
--     HH.div
--       [ HP.class_ $ HH.className "container" ]
--       [ HH.h1_ [ HH.text "purescript-jtable demo" ]
--       , HH.p_ [ HH.text "Paste some JSON:" ]
--       , HH.p_
--           [ HH.textarea
--               [ HP.class_ $ HH.className "form-control"
--               , HP.value jsonString
--               , HE.onValueInput $ HE.input SetJsonText
--               ]
--           ]
--       , HH.h2_ [ HH.text "Output" ]
--       , HH.slot unit \_ ->
--           { component : J.jtableComponent opts
--           , initialState : jsonEmptyArray
--           }
--       ]
--
--   eval :: DemoEval g
--   eval (SetJsonText jsonString next) = do
--     H.query unit <<< H.action <<< J.SetJson $
--       case jsonParser jsonString of
--         Left _ -> jsonEmptyArray
--         Right json -> json
--     pure next
--
-- type Effects =
--   ( dom :: DOM
--   , avar :: Aff.AVAR
--   , err :: EXCEPTION
--   )

main :: Eff (HalogenEffects (ajax :: AX.AJAX)) Unit
main = runHalogenAff do
  body <- awaitBody
  pure $ HH.div [] [ HH.h1_ [ HH.text "foooooo"]]
  H.runUI ui
          unit
          body
