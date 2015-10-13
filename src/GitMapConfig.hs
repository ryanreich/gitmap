{-# LANGUAGE TemplateHaskell #-}

module GitMapConfig where

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Data.Vector ((++))

import Control.Applicative
import Control.Monad

import qualified Data.Yaml.Aeson as Yaml
import Data.Maybe
import Data.List.Split

type GitHashMapConfig =
  GitHashMapConfig
  {
    gmcRepos :: [GitHashMapRepository],
    gmcGlobal :: Yaml.Value
  }

data GitHashMapRepository =
  StackGMR
  {
    stackRepoGitURL :: Text,
    stackRepoExtraGitArgs :: Maybe Args,
    stackRepoExtraDeps :: Maybe [Yaml.Value],
    stackRepoFlags :: Maybe Yaml.Value
  }

data Args =
  Args
  {
    simpleArgs :: Text,
    complexArgs :: [Text]
  }

instance Yaml.FromJSON GitHashMapConfig where
  parseJSON (Yaml.Object hashHashMap) =
    GitHashMapConfig
    {
      gmcRepos = hashHashMap .: "repositories",
      gmcGlobal = hashHashMap .: "global"
    }
  parseJSON _ = empty

instance Yaml.FromJSON GitHashMapRepository where
  parseJSON (Yaml.Object hashHashMap) =
    StackGMR
    {
      stackRepoGitURL = hashHashMap .: "git-url",
      stackRepoExtraGitArgs = hashHashMap .:? "extra-git-args",
      stackRepoExtraDeps = hashHashMap .:? "extra-deps",
      stackRepoFlags = hashHashMap .:? "flags"
    }
  parseJSON _ = empty

instance Yaml.FromJSON Args where
  parseJSON (Yaml.Object hashHashMap) =
    Args
    {
      simpleArgs = hashHashMap .: "simple",
      complexArgs = hashHashMap .: "complex"
    }
  parseJSON _ = empty

data GitHashMapRepoSpec =
  GitHashMapRepoSpec
  {
    gmrsName :: String,
    gmrsGitArgs :: [String]
  }

data GitHashMapConfigData =
  GitHashMapConfigData
  {
    gmcdRepoSpecs :: [GitHashMapRepoSpec],
    gmcdURLs :: [String],
    gmcdStackYaml :: Yaml.Value
  }

extractConfig :: GitHashMapConfig -> GitHashMapConfigData
extractConfig config =
  GitHashMapConfigData
  {
    gmcdRepoSpecs = flip map (gmcRepos config) $ \repo ->
     GitHashMapRepoSpec
     {
       gmrsName = last $ splitOn "/" $ Text.unpack $ stackRepoGitURL repo,
       gmrsGitArgs =
         let as = stackRepoExtraGitArgs repo
         in (words $ Text.unpack $ simpleArgs as) ++
            (map Text.unpack $ complexArgs as)
     },
    gmcdURLs = map (unpack . stackRepoGitURL) (gmcRepos config),
    gmcdStackYaml =
      let repos = gmcRepos config
          urls = map stackGitRepoURL repos
      in combineStackYaml (gmcGlobal config) (
        Yaml.array urls,
        Yaml.array $ catMaybes $ map stackRepoExtraDeps repos,
        HashMap.fromList $ zip urls $ map stackRepoFlags repos
        )
  }

combineStackYaml :: Yaml.Value -> (Yaml.Value, Yaml.Value, HashMap Text Yaml.Value) ->
                    Yaml.Value
combineStackYaml (Yaml.Object global) (packages, extraDeps, flags) =
  Yaml.Object $
  HashMap.insertWith (Vector.++) (Text.pack "packages") packages $
  HashMap.insertWith (Vector.++) (Text.pack "extra-deps") extraDeps $
  HashMap.union flags global
