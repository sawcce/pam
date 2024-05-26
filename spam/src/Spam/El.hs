module Spam.El where

data El where
  Tag :: String -> [(String, String)] -> El -> El
  Templated :: forall x. (Show x) => String -> (x -> El) -> x -> El
  -- Component :: forall x y. (Show x, Show y) => String -> (x -> y) -> (x -> y -> El) -> x -> El
  String :: String -> El
  Empty :: El
  Fragment :: [El] -> El

instance Show El where
  show = render

renderArgs :: (Foldable t) => t (String, String) -> [Char]
renderArgs = concatMap f where f (key, value) = " " <> key <> "=" <> "\"" <> value <> "\""

render :: El -> String
render (Tag name params children) = "<" <> name <> renderArgs params <> ">" <> render children <> "</" <> name <> ">"
render (String contents) = contents
render (Templated _ renderer args) = render $ renderer args
render (Fragment children) = concatMap render children
render Empty = ""

cn x = ("class", x)

lang x = ("lang", x)