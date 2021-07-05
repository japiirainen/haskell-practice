{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Data.Int
import           Data.Text        hiding (map)
import           GHC.Generics
import           Hasql.Connection
import           Prelude
import           Rel8

main :: IO ()
main = do
    Right conn <- acquire connectionString

    projects <- select conn getProjects
    authors <- select conn getAuthors

    putStrLn $ show projects
    putStrLn $ show authors

    pure ()
    where
        connectionString = settings "localhost" 5432 "joona" "devi" "devi"


data Author f = Author
    { authorId   :: Column f AuthorId
    , authorName :: Column f Text
    , authorUrl  :: Column f (Maybe Text)
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

newtype AuthorId = AuthorId { toInt64 :: Int64 }
    deriving newtype (DBEq, DBType, Eq, Show)


data Project f = Project
    { projectAuthorId :: Column f AuthorId
    , projectName     :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Author f)
deriving stock instance f ~ Result => Show (Project f)

authorSchema :: TableSchema (Author Name)
authorSchema = TableSchema
    { name = "author"
    , schema = Nothing
    , columns = Author
        { authorId = "author_id"
        , authorName = "name"
        , authorUrl = "url"
        }
    }


projectSchema :: TableSchema (Project Name)
projectSchema = TableSchema
    { name = "project"
    , schema = Nothing
    , columns = Project
        { projectAuthorId = "author_id"
        , projectName = "name"
        }
    }

getProjects :: Query (Project Expr)
getProjects = each projectSchema

getAuthors :: Query (Author Expr)
getAuthors = each authorSchema
