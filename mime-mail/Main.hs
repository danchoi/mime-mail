{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Main where
import Network.Mail.Mime
import Data.Aeson
import Data.Text (Text)
import Control.Applicative
import qualified Data.Yaml as Y
import Data.Aeson
import Data.ByteString as B
import Data.Monoid

data MailData = MailData {
    sender :: Address
  , recipients :: [Address]
  , subject :: Text
  , plainBody :: Text
  , htmlHtml :: Text
  , inlineImages :: [(Text, FilePath)]
  , attachments :: [(Text, FilePath)]
  } deriving Show

newtype External = External (Text, FilePath)

instance FromJSON MailData where
  parseJSON (Object v) = MailData 
      <$> v .: "sender"
      <*> v .: "recipients"
      <*> v .: "subject"
      <*> v .: "plain"
      <*> v .: "html"
      <*> ((mapM parseFileRef =<< v .: "images") <|> pure [])
      <*> (v .: "attachments" <|> pure [])
  parseJSON _ = mempty

instance FromJSON Address where
  parseJSON (Object v) = Address 
      <$> (Just <$> v .: "name" <|> pure Nothing)
      <*> v .: "email"
  parseJSON _ = mempty

parseFileRef :: Value -> Y.Parser (Text, FilePath)
parseFileRef (Object v) = do
    f <- v .: "path"
    m <- v .: "mime_type"
    return (m, f)

main = do
  x <- B.getContents 
  let mail :: MailData
      mail = either error id 
                $ Y.decodeEither x
  print mail
