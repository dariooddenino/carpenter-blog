module Components.Container where

import Prelude
import Carpenter (spec, Render, Update)
import React (createClass, ReactClass)
import React.DOM (text, button, h1', div')
import React.DOM.Props (onClick)
import React.Router.History (historyRouter)
import App.Routes (Route(..), Act(..), routes)

type State =
  { route :: Route }

data Action = PageView Route

containerComponent :: forall props. ReactClass props
containerComponent = createClass $ spec 0 update render

update :: forall props eff. Update State props Action eff
update yield _ action _ _ =
  case action of
    PageView r -> do
      yield $ _ { route = r }

render :: forall props. Render State props Action
render dispatch _ state _ =
  div'
    [ historyRouter (dispatch <<< PageView) routes
    , h1' [ text (case state.route of
                     Home -> "home"
                     Posts View x -> "view" <> show x
                     Posts Edit x -> "edit" <> show x) ]
    ]
