module Domains.Domain where

import Data.Aeson
import GHC.Generics
import Data.Text as T
import Text.Regex.TDFA
import Data.Default
import qualified Data.Char as C

newtype DetectRequest = DetectRequest {
    q :: Text
} deriving (Show, Generic)
instance ToJSON DetectRequest

data SingleDetection = SingleDetection {
    language :: Text,
    isReliable :: Bool,
    confidence :: Float
} deriving (Show, Generic)
instance FromJSON SingleDetection

newtype DetectList = DetectList {
    detections :: [SingleDetection]
} deriving (Show, Generic)
instance FromJSON DetectList where
    parseJSON = withObject "data" $ \o -> do
        inner <- o .: "detections"
        return $ DetectList inner

newtype DetectResponse = DetectResponse DetectList
    deriving (Show, Generic)
instance FromJSON DetectResponse where
    parseJSON = withObject "outer" $ \o -> do
        inner <- o .: "data"
        return $ DetectResponse inner

data TransRequest = TransRequest {
    text :: [Text],
    target_lang :: Text,
    ignore_tags :: [Text],
    formality :: Maybe Text, 
    context :: Maybe Text
} deriving (Show, Generic)
instance ToJSON TransRequest
instance FromJSON TransRequest

instance Default TransRequest where
    def = TransRequest [] "" ["ignore"] (Just "prefer_less") Nothing

data SingleTranslation = SingleTranslation {
    detected_source_language :: Text,
    translated_text :: Text
} deriving (Show, Generic)
instance FromJSON SingleTranslation where
    parseJSON = withObject "singleTranslation" $ \o -> do
        src <- o .: "detected_source_language"
        txt <- o .: "text"
        return $ SingleTranslation src txt

newtype TransResponse = TransResponse {
    translations :: [SingleTranslation]
} deriving (Show, Generic)
instance FromJSON TransResponse

type ApiToken = Text

isTarget :: Text -> DetectResponse -> Bool
isTarget tgt (DetectResponse ls) = Prelude.any (\d -> toLower (language d) == toLower tgt && isReliable d) $ detections ls

toPara :: Text -> TransResponse -> Maybe Text
toPara lang (TransResponse ls) = Prelude.foldr (\a b ->
    if toLower (detected_source_language a) == toLower lang
        then b
        else Just (translated_text a) <> b
        ) Nothing ls


emojiList :: Text
emojiList = T.concat $ pack <$> [['\x1f600'..'\x1F64F'],
                 ['\x1f300'..'\x1f5ff'],
                 ['\x1f680'..'\x1f6ff'],
                 ['\x1f1e0'..'\x1f1ff'],
                 ['\x2600'..'\x26ff'],
                 ['\x2700'..'\x27bf'],
                 ['\x1f900'..'\x1f9ff'],
                 ['\x1fa70'..'\x1faff']
                 ]

mapRegex :: (String -> String) -> String -> String -> String
mapRegex f regex msg
    | msg == "" || Prelude.null msg = ""
    | otherwise =
        let (pre, emote, post) = msg =~ regex
            in
                if emote == "" then
                    pre
                else pre ++ f emote ++ mapRegex f regex post

removeKusa :: Text -> Text
removeKusa = mapCharList (`T.elem` "草") (const "")

removeEmoji :: Text -> Text
removeEmoji = mapCharList (`T.elem` emojiList) (const "")

removeBlank :: Text -> Text
removeBlank = mapCharList (\x -> C.isPunctuation x || C.isSpace x) (const "")

removeEmote :: Text -> Text
removeEmote = mapEmote (const "")

removeUrl :: Text -> Text
removeUrl = mapUrl (const "")

removeW :: Text -> Text
removeW = mapW (const "")

removeAt :: Text -> Text
removeAt = mapAt (const "")

removeNull :: Text -> Text
removeNull = removeBlank . removeW . removeKusa

removeVanity :: Text -> Text
removeVanity = removeAt . removeUrl . removeEmoji . removeEmote

removeIgnore :: Text -> Text
removeIgnore msg = pack $ mapRegex (const "") "(<ignore>|</ignore>)" (unpack msg)

addIgnore :: Text -> Text
addIgnore msg =
    let f s = "<ignore>" ++ s ++ "</ignore>"
        g s = T.concat ["<ignore>", T.cons s "</ignore>"]
        igEmote = mapEmote f msg
        igUrl = mapUrl f igEmote
    in
        mapCharList (`T.elem` emojiList) g igUrl

mapW :: (String -> String) -> Text -> Text
mapW f msg = pack $ mapRegex f "w{2,}" (unpack msg)

mapEmote :: (String -> String) -> Text -> Text
mapEmote f msg = pack $ mapRegex f "<:[^ :\n\t<>]+:[0-9]+>" (unpack msg)

mapUrl :: (String -> String) -> Text -> Text
mapUrl f msg = pack $ mapRegex f "(http(s)?:\\/\\/)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)" (unpack msg)

mapAt :: (String -> String) -> Text -> Text
mapAt f msg = pack $ mapRegex f "@[^ \n\t]+" (unpack msg)

mapCharList :: (Char -> Bool) -> (Char -> Text) -> Text -> Text
mapCharList list f msg
    | T.null msg = ""
    | otherwise =
        case T.break list msg of
            (t, "") -> t
            (pre, post) -> T.concat [pre, f $ T.head post , mapCharList list f $ T.tail post]
