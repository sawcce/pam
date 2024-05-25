module Main where

import Spam.El
import Spam.El.Html
import Spam.Hey

docHead :: El
docHead =
  head' [] $
    script' [("src", "https://cdn.tailwindcss.com")] Empty

document :: El -> El
document =
  Templated
    "DocumentTemplate"
    ( \c ->
        html' [lang "en"] $
          Fragment
            [ docHead,
              body' [] c
            ]
    )

headline :: El -> El
headline = h1' [cn "scroll-m-20 text-4xl font-extrabold tracking-tight lg:text-5xl"]

title :: El -> El
title = h2' [cn "scroll-m-20 border-b pb-2 text-3xl font-semibold tracking-tight first:mt-0"]

paragraph :: El -> El
paragraph = p' [cn "leading-7 [&:not(:first-child)]:mt-6"]

filterOdds :: [Int] -> El
filterOdds =
  Templated
    "FilterOdds"
    ( div' [cn "flex flex-row flex-wrap gap-4 w-full my-4"]
        . Fragment
        . map g
        . filter odd
    )
  where
    g number = pre' [cn ""] (String $ show number)

quote :: String -> El
quote = Templated "Quote" (\x -> blockquote' [cn "mt-6 border-l-2 pl-6 italic"] $ String ("\" " <> x <> " \""))

page :: El
page =
  document
    ( main' [cn "p-8"] $
        Fragment
          [ headline $ String "Welcome to my awesome website",
            title $ String "A website to do stuff!",
            paragraph $ String "Have a great day :)",
            quote "Haskell is great!",
            filterOdds [1 .. 1000]
          ]
    )

data Args = Args Bool

data Arg = Arg

instance Evaluate Args Arg Bool where
  evaluate (Args b) _ = b

instance Convert Args Arg where
  convert _ _ = "x"

main :: IO ()
main = do
  let t2 :: Function Args Bool = Function Arg
  print $ evaluate (Args True) t2
  print $ convert (Args True) t2