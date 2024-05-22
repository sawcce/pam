data El where
  Tag :: String -> [(String, String)] -> El -> El
  Templated :: forall x. (Show x) => String -> (x -> El) -> x -> El
  String :: String -> El
  Empty :: El
  Fragment :: [El] -> El

instance Show El where
  show (Tag name params children) = "Tag(" <> name <> " " <> show params <> ") " <> "{" <> show children <> "}"
  show (Templated name func inv) = name <> " {" <> show inv <> "}"
  show (String str) = show str
  show (Fragment list) = "{ " <> show list <> " }"
  show Empty = "</>"

html = Tag "html"

body = Tag "body"

h1 = Tag "h1"

h2 = Tag "h2"

p = Tag "p"

lang x = ("lang", x)

cn x = ("class", x)

quote :: String -> El
quote = Templated "Quote" (\x -> p [cn "quote"] $ String ("\" " <> x <> " \""))

document :: El -> El
document = Templated "DocumentTemplate" (html [lang "en"] . body [])

page =
  document
    ( Fragment
        [ h1 [] $ String "Welcome to my awesome website",
          h2 [] $ String "A website to do stuff!",
          p [] $ String "Have a great day :)",
          quote "Haskell is great!"
        ]
    )

renderArgs :: (Foldable t) => t (String, String) -> [Char]
renderArgs = concatMap f where f (key, value) = " " <> key <> "=" <> "\"" <> value <> "\""

render :: El -> String
render (Tag name params children) = "<" <> name <> renderArgs params <> ">" <> render children <> "</" <> name <> ">"
render (String contents) = contents
render (Templated _ renderer args) = render $ renderer args
render (Fragment children) = concatMap render children
render Empty = ""

main = do
  print page
  putStrLn $ render page