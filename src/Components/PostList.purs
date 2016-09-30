module Components.PostList where

import Prelude
import Carpenter (Render, Update)
import Carpenter.Cedar (cedarSpec', CedarClass)
import Components.Post (Post)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..), either)
import Network.HTTP.Affjax (delete, AJAX, get, post)
import React (createClass)
import React.DOM (text, div', h2', ol')
import Debug.Trace (trace)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)

type Posts = Array Post

data Action
  = RequestPosts
  -- | ReceivePosts (Either String Posts)
  -- | CreatePost
  -- | DeletePost Int

type PostList =
  { posts :: Posts
  , status :: String
  }

initState :: PostList
initState =
  { posts: []
  , status: "..."
  }

-- postListComponent :: componente che chiama requestposts all'inizializzazione
-- postListComponent :: CedarClass PostList Action
-- postListComponent = createClass $ cedarSpec' RequestPosts initState update render


-- update
update :: forall props eff. Update PostList props Action (ajax :: AJAX, console :: CONSOLE | eff)
update yield dispatch action _ _ =
  case action of
    RequestPosts -> do
      res <- attempt $ get "http://localhost:3001/api/posts"
      let decode r = decodeJson r.response :: Either String Posts
      let posts = either (Left <<< show) decode res
      yield $ _ { status = "Fetching posts..." }


-- render
-- render :: forall props. Render PostList props Action
-- render _ _ state _ =
--   div'
--     [ h2' [ text state.status ]
--     , ol' $ map postView state.posts
--     ]

-- postView :: forall props. ReactClass props
-- postView
