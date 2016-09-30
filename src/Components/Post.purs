module Components.Post where

import Prelude
import Data.Argonaut (jsonEmptyObject, class DecodeJson, decodeJson, (.?), class EncodeJson, encodeJson, (:=), (~>))
import Data.Maybe (Maybe(..))

newtype Post = Post
  { id :: Maybe Int
  , title :: String
  , body :: String
  }

type State =
  { post :: Post
  , status :: String
  }

instance decodeJsonPost :: DecodeJson Post where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    body <- obj .? "body"
    pure $ Post { id, title, body }

instance encodeJsonPost :: EncodeJson Post where
  encodeJson (Post post)
    = "id" := post.id
    ~> "title" := post.title
    ~> "body" := post.body
    ~> jsonEmptyObject
