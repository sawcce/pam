module Main where

import Spam.El

quote :: String -> El
quote = Templated "Quote" (\x -> p [cn "quote"] $ String ("\" " <> x <> " \""))

document :: El -> El
document = Templated "DocumentTemplate" (html [lang "en"] . body [])

filterOdds :: [Int] -> El
filterOdds = Templated "FilterOdds" (Fragment . map g . filter odd)
  where
    g number = p [] (String $ show number)

page :: El
page =
  document
    ( Fragment
        [ h1 [] $ String "Welcome to my awesome website",
          h2 [] $ String "A website to do stuff!",
          p [] $ String "Have a great day :)",
          quote "Haskell is great!",
          filterOdds [1 .. 90]
        ]
    )

main :: IO ()
main = do
  writeFile "page.html" (render page)