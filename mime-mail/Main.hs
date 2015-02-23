{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Main where
import Network.Mail.Mime
import Data.Aeson
import Data.Text (Text)
import Control.Applicative


data MailData = MailData {
    sender :: Address
  , recipients :: [Address]
  , subject :: Text
  , plainBody :: Text
  , htmlHtml :: Text
  , inlineImages :: [(Text, FilePath)]
  , attachments :: [(Text, FilePath)]
  } 

instance FromJSON MailData where
  parseJSON (Object v) = MailData 
      <$> v .: "sender"
      <*> v .: "recipients"
      <*> v .: "subject"
      <*> v .: "plain"
      <*> v .: "html"
      <*> v .: "images"
      <*> v .: "attachments"

instance FromJSON Address where
  parseJSON (Object v) = Address 
      <$> (Just <$> v .: "name" <|> pure Nothing)
      <*> v .: "email"

main = do
  undefined
