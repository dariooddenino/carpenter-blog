module Components.Container where

import Prelude
import Carpenter (spec, Render, Update)
import React (createClass, ReactClass)
import React.DOM (text, button, h1', div', p')
import React.DOM.Props (onClick)
import React.Router.History (historyRouter, link)
import App.Routes (Route(..), Act(..), routes)
import Debug.Trace (trace)

type State =
  { route :: Route }

initState :: State
initState = { route: Home }

data Action = PageView Route

containerComponent :: forall props. ReactClass props
containerComponent = createClass $ spec initState update render

update :: forall props eff. Update State props Action eff
update yield _ action _ _ =
  case action of
    PageView r -> do
      trace "PageView" \_ ->
      yield $ _ { route = r }

render :: forall props. Render State props Action
render dispatch _ state _ =
  div'
    [ historyRouter (dispatch <<< PageView) routes
    , h1' [ link "/" [] [ text "Carpenter Blog" ]]
    , h1' [ link "/posts/1" [] [ text "Post 1" ]]
    , case state.route of
               Home -> p' [ text "Home" ]
               Posts View x -> p' [ text $ "View " <> show x ]
               Posts Edit x -> p' [ text $ "Edit " <> show x ]
    ]
