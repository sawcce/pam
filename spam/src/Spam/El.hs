module Spam.El where

data El where
  Tag :: forall x. (IntoEl x) => String -> [(String, String)] -> x -> El
  Templated :: forall x. (Show x) => String -> (x -> El) -> x -> El
  -- Component :: forall x y. (Show x, Show y) => String -> (x -> y) -> (x -> y -> El) -> x -> El
  String :: String -> El
  Empty :: El
  Fragment :: [El] -> El

instance Show El where
  show = render

class IntoEl ty where
  intoEl :: ty -> El

instance IntoEl String where
  intoEl = String

instance (IntoEl x) => IntoEl [x] where
  intoEl l = Fragment $ map intoEl l

instance IntoEl El where
  intoEl x = x

renderArgs :: (Foldable t) => t (String, String) -> [Char]
renderArgs = concatMap f where f (key, value) = " " <> key <> "=" <> "\"" <> value <> "\""

render :: El -> String
render (Tag name params children) = "<" <> name <> renderArgs params <> ">" <> render (intoEl children) <> "</" <> name <> ">"
render (String contents) = contents
render (Templated _ renderer args) = render $ renderer args
render (Fragment children) = concatMap render children
render Empty = ""

cn x = ("class", x)

lang x = ("lang", x)