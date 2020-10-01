{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.ScheduleList where

import Import

getScheduleListR :: Handler Html
getScheduleListR = do
    schedules <- runDB $ selectList [] [Desc ScheduleDay]
    defaultLayout $(widgetFile "scheduleList")

postScheduleDeleteR :: Handler Html
postScheduleDeleteR = do
    scheduleIdText <- runInputPost $ ireq hiddenField "scheduleId"
    case (fromPathPiece scheduleIdText) of
        Just scheduleId -> do
            _ <- runDB $ delete (scheduleId :: ScheduleId)
            redirect ScheduleListR
        Nothing -> Import.undefined
        _ -> Import.undefined