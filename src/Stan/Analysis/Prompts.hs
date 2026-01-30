{-# LANGUAGE TemplateHaskell #-}
module Stan.Analysis.Prompts (getPromptFor) where

import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map

prompt03 :: T.Text
prompt03 = T.decodeUtf8 $(embedFile "prompts/plu-stan-03.md")

-- | Mapping of Rule IDs to their Prompts for LLMs 
prompts :: Map.Map String T.Text
prompts = Map.fromList
     [ ("PLU-STAN-03", prompt03)
     ]

getPromptFor :: String -> Text
getPromptFor ruleId = Map.findWithDefault "Generic Plutus Audit Prompt" ruleId prompts
