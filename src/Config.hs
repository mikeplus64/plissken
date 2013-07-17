{-# LANGUAGE OverloadedStrings #-}
module Config where
import Save
defaultConfig :: Config
defaultConfig = mkConfig $ do
    conf "p.name" (text "MAL")
    conf "scores" $
        list [ pair (text "MAL") (nat 500)
             , pair (text "MAL") (nat 433)
             ]
    conf "options.resx" (nat 512)
    conf "options.rexy" (nat 512)
