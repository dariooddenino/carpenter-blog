module App.Routes where

import Data.Functor ((<$))
import Prelude (($), (<$>), Unit, (<>), show)
import Data.Show
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>), (<*>))
import Routing.Match (Match)
import Routing.Match.Class (lit, num)
import Data.Int (floor)

data Act = View | Edit

data Route
  = Home
  | Posts Act Int

instance showRoute :: Show Route where
  show Home = "home"
  show (Posts View x) = "View " <> show x
  show (Posts Edit x) = "Edit " <> show x

oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

int :: Match Int
int = floor <$> num

routes :: Match Route
routes
    = Posts Edit <$> (homeSlash *> lit "posts" *> int <* lit "edit")
  <|> Posts View <$> (homeSlash *> lit "posts" *> int)
  <|> Home <$ homeSlash
