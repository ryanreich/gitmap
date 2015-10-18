{-# LANGUAGE OverloadedStrings, TupleSections #-}

module GitMapConfig (
  GitMapConfigData (..),
  GitMapRepoSpec (..)
  ) where

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Control.Applicative

import qualified Data.Yaml.Aeson as Yaml
import Data.Char
import Data.List
import Data.List.Split

data GitMapConfigData =
  GitMapConfigData
  {
    gmcdRepoSpecs :: [GitMapRepoSpec],
    gmcdStackYaml :: Yaml.Object
  }

data GitMapRepoSpec =
  GitMapRepoSpec
  {
    gmrsName :: String,
    gmrsURL :: String,
    gmrsGitArgs :: HashMap String [String]
  }
  deriving (Eq)

data GitMapConfig =
  GitMapConfig
  {
    gmcRepos :: [GitMapRepository],
    gmcGlobal :: GitMapConfigGlobal
  }

data GitMapConfigGlobal =
  GitMapConfigGlobal
  {
    gmcgPackages :: [Yaml.Value],
    gmcgExtraDeps :: [Text],
    gmcgFlags :: HashMap Text (HashMap Text Bool),
    gmcgOther :: Yaml.Object
  }

data GitMapRepository =
  StackGMR
  {
    stackRepoGitURL :: String,
    stackRepoExtraGitArgs :: HashMap String Args,
    stackRepoExtraDeps :: [Text],
    stackRepoFlags :: HashMap Text Bool
  }

data Args =
  Args
  {
    simpleArgs :: String,
    complexArgs :: [String]
  }

instance Yaml.FromJSON GitMapConfigData where
  parseJSON yamlValue = do
    configFile <- Yaml.parseJSON yamlValue
    let repos = gmcRepos configFile
        urls = map stackRepoGitURL repos
        names = map (last . splitOn "/" . stackRepoGitURL) repos
    return $ GitMapConfigData {
      gmcdRepoSpecs =
         (\x y z f3 -> zipWith3 f3 x y z) repos urls names $ \repo url name ->
         let asM = stackRepoExtraGitArgs repo
         in GitMapRepoSpec {
           gmrsName = name,
           gmrsURL = url,
           gmrsGitArgs =
             flip HashMap.map asM $
             \as ->
             let sArgs = words $ simpleArgs as
                 cArgs = concatMap ((\(x,y)->[x,y]) . break isSpace) $
                         complexArgs as
             in sArgs ++ cArgs
           },
      gmcdStackYaml =
        let tNames = map Text.pack names
        in combineStackYaml (gmcGlobal configFile) (
          map Yaml.toJSON tNames,
          nub $ concat $ map stackRepoExtraDeps repos,
          HashMap.filter (not . HashMap.null) $ HashMap.fromList $
          zip tNames (map stackRepoFlags repos)
          )
      }

combineStackYaml :: GitMapConfigGlobal ->
                    ([Yaml.Value], [Text], HashMap Text (HashMap Text Bool)) ->
                    Yaml.Object
combineStackYaml GitMapConfigGlobal {
  gmcgPackages = gPackages,
  gmcgExtraDeps = gExtraDeps,
  gmcgFlags = gFlags,
  gmcgOther = gOther
  } (packages, extraDeps, flags) =
  HashMap.insert "packages" (Yaml.toJSON $ packages ++ gPackages)  $
  HashMap.insert "extra-deps" (Yaml.toJSON $ extraDeps ++ gExtraDeps) $
  HashMap.insert "flags" (
    Yaml.toJSON $ HashMap.unionWith (HashMap.unionWith $ curry fst) flags gFlags
    ) gOther
      
instance Yaml.FromJSON GitMapConfig where
  parseJSON (Yaml.Object hashMap) = do
    gmcR <- hashMap Yaml..: "repositories"
    gmcG <- hashMap Yaml..: "global"
    return $ GitMapConfig {gmcRepos = gmcR, gmcGlobal = gmcG}
  parseJSON _ = empty

instance Yaml.FromJSON GitMapConfigGlobal where
  parseJSON (Yaml.Object hashMap) = do
    gmcgP <- hashMap Yaml..:? "packages" Yaml..!= []
    gmcgED <- hashMap Yaml..:? "extra-deps" Yaml..!= []
    gmcgF <- hashMap Yaml..:? "flags" Yaml..!= HashMap.empty 
    let gmcgO =
          HashMap.delete "packages" $
          HashMap.delete "extra-deps" $
          HashMap.delete "flags" hashMap
    return $ GitMapConfigGlobal {
      gmcgPackages = gmcgP,
      gmcgExtraDeps = gmcgED,
      gmcgFlags = gmcgF,
      gmcgOther = gmcgO
      }
  parseJSON _ = empty

instance Yaml.FromJSON GitMapRepository where
  parseJSON (Yaml.Object hashMap) = do
    srGU <- hashMap Yaml..: "git-url"
    srEGA <- hashMap Yaml..:? "extra-git-args" Yaml..!= HashMap.empty
    srED <- hashMap Yaml..:? "extra-deps" Yaml..!= []
    srF <- hashMap Yaml..:? "flags" Yaml..!= HashMap.empty
    return $ StackGMR {
      stackRepoGitURL = srGU,
      stackRepoExtraGitArgs = srEGA,
      stackRepoExtraDeps = srED,
      stackRepoFlags = srF
      }
  parseJSON _ = empty

instance Yaml.FromJSON Args where
  parseJSON (Yaml.Object hashMap) = do
    sA <- hashMap Yaml..:? "simple" Yaml..!= ""
    cA <- hashMap Yaml..:? "complex" Yaml..!= []
    return $ Args {simpleArgs = sA, complexArgs = cA}
  parseJSON _ = empty
