{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Loginuser where

import Import

data AccountFormModel = AccountFormModel 
    {
        account :: Text
        , password :: Text
    }

loginuserForm :: Html -> MForm Handler (FormResult AccountFormModel, Widget)
loginuserForm = renderDivs $ AccountFormModel
         <$> areq textField "ユーザー名" (Just "")
         <*> areq textField "パスワード" (Just "")

getLoginuserR :: Handler Html
getLoginuserR = do
    let
        header = "ログイン" :: Text
    (widget, enctype) <- generateFormPost loginuserForm
    defaultLayout $(widgetFile "loginuser")

postLoginuserR :: Handler Html
postLoginuserR = do
    ((result, widget),enctype) <- runFormPost $ loginuserForm
    let
        header = "ログイン" :: Text
    case result of
        FormSuccess _ -> do
            --loginuser <- runDB $ selectList [AccountAccount ==. "a",AccountPassword ==. "a"] []
            redirect ScheduleListR
        FormFailure _ -> do
            -- 不正な入力値のデータが送信された場合(必須項目が未入力等)
            setMessage "不正なデータが送信されました。"
            defaultLayout $(widgetFile "loginuser")
        FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]
        _ -> Import.undefined

