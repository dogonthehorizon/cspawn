{-# LANGUAGE
    DeriveGeneric
    , OverloadedStrings
    #-}
module Build where

import Control.Exception
import Data.Aeson
import qualified Data.Text as T
import Git
import Git.CmdLine
import GHC.Generics
import Shelly


-- |
--
-- @
--  {
--      "repo" : "github.com/wayofthepie/test.git",
--      "branch" : "refs/heads/develop"
--  }
-- @
data BuildSpec = BuildSpec
    { repo       :: T.Text
    , branchSpec :: T.Text
    } deriving( Eq, Generic, Show )

instance FromJSON BuildSpec
instance ToJSON BuildSpec


runBuild :: BuildSpec -> IO (Either SomeException T.Text)
runBuild bs = do
    let buildRepo = repo bs
    clone buildRepo "."


-- | Equivalent to git clone "https://xyz.com/test.git" "/opt/repos/test"
clone :: T.Text -> T.Text -> IO (Either SomeException T.Text)
clone repoUrl localDir =
    try . shelly $ do
        cd "/var/tmp"
        git (CliRepo defaultRepositoryOptions)
            ["clone", repoUrl, localDir]


