module App.Routes where

import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Prelude (($), (<$>), Unit)
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>), (<*>))
import Routing.Match (Match)
import Routing.Match.Class (lit, num)
import Data.Int (floor)

data Act = View | Edit

data Route
  = Home
  | Posts Act Int

oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

int :: Match Int
int = floor <$> num

routes :: Match Route
routes
    = Home <$ oneSlash
  <|> Posts View <$> (homeSlash *> lit "posts" *> int)
  <|> Posts Edit <$> (homeSlash *> lit "posts" *> int <* lit "edit")
