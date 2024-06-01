module Routes where

import Spam
import Spam.El
import Spam.El.Html

appRoutes :: [Route]
appRoutes = [helloRoute]

helloRoute :: Route
helloRoute =
  Route
    { path = "/",
      contents =
        html' [] $
          Fragment
            [ head' [] $
                Fragment
                  [ link' [("rel", "stylesheet"), ("type", "text/css"), ("href", "/index.css")] ""
                  ],
              body' [cn "p-8"] $
                Fragment
                  [ h2' [cn "text-4xl my-8"] $ String "Demo site for Spam (the web toolkit)!",
                    p' [] $ String "The api is likely to change and I welcome your suggestions..."
                  ]
            ]
    }