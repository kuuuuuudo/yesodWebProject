{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Schedule where

import Import

scheduleForm :: Maybe Schedule -> Html -> MForm Handler (FormResult Schedule, Widget)
scheduleForm schedule extra = do 
    (accountidResult, accountidView)  <- mreq intField "" (scheduleAccountid <$> schedule)
    (dayResult, dayView)              <- mreq dayField "日付" (scheduleDay <$> schedule)
    (contentsResult, contentsView)    <- mreq textField "内容" (scheduleContents <$> schedule)
    let
        result = Schedule
            <$> accountidResult
            <*> dayResult
            <*> contentsResult
        widget = $(widgetFile "schedule-editor-form")
    return (result, widget)


getScheduleR :: Handler Html
getScheduleR = do
    let
        header = "予定登録" :: Text
    (widget, enctype) <- generateFormPost $ scheduleForm Nothing
    defaultLayout $(widgetFile "schedule")

postScheduleR :: Handler Html
postScheduleR = do
    ((result,widget),enctype) <- runFormPost $ scheduleForm Nothing
    let
        header = "予定登録" :: Text
    case result of
        FormSuccess schedule -> do
            scheduleId <- runDB $ insert schedule
            redirect ScheduleListR
        FormFailure _ -> do
            setMessage "不正なデータが送信されました。"
            defaultLayout $(widgetFile "schedule")
        FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]

--更新
getScheduleUpdateR :: ScheduleId -> Handler Html
getScheduleUpdateR scheduleId = do
  let
    header = "予定更新" :: Text
  schedule <- runDB $ get404 scheduleId  --更新対象のデータが存在しないと404エラー
  (widget, enctype) <- generateFormPost $ scheduleForm $ Just schedule
  defaultLayout $(widgetFile "scheduleUpdate")

--更新処理のポスト
postScheduleUpdateR :: ScheduleId -> Handler Html
postScheduleUpdateR scheduleId = do
  ((result, widget),enctype) <- runFormPost $ scheduleForm Nothing
  let
    header = "予定更新" :: Text
  case result of
    FormSuccess schedule -> do
      --Postされたデータが正常な場合
      _ <- runDB $ do
        _ <- get404 scheduleId --更新対象のデータが存在しないと404エラー
        replace scheduleId schedule   --更新処理の実行
      redirect ScheduleListR
    FormFailure _ -> do
      --不正な入力値のデータが送信された場合(必須項目が未入力等)
      setMessage "不正なデータが送信されました。"
      defaultLayout $(widgetFile "schedule")
    FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]
    _ -> Import.undefined