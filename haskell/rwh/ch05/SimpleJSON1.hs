
data JValue = JString String
            | JNumber Double 
            | JBool Bool
            | JNull 
            | JObject [(String, JValue)]
            | JArray  [JValue]
            deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String 
getString (JString s) = Just s
getString _           = Nothing

getBool :: JValue -> Maybe Bool 
getBool (JBool b) = Just b 
getBool _         = Nothing 

getNumber :: JValue -> Maybe Double
getNumber (JNumber n) = Just n
getNumber _           = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject js = case js of
               JObject xs -> Just xs
               _          -> Nothing 


getArray :: JValue -> Maybe [JValue]
getArray js = case js of
              JArray xs -> Just xs
              _         -> Nothing 

isNull :: JValue -> Bool 
isNull JNull = True
isNull _     = False 
