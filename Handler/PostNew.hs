module Handler.PostNew where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown
import Text.Blaze
import Data.Time (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)

instance ToMarkup UTCTime where
   toHtml = text $ show

data BlogForm = BlogForm {
    title :: Text
    article :: Textarea
  }
  deriving Show

blogPostForm :: AForm Handler BlogPost
blogPostForm = BlogPost 
            <$> areq textField     (bfs ("Title" :: Text)) Nothing
            <*> areq markdownField (bfs ("Article" :: Text)) Nothing

getPostNewR :: Handler Html
getPostNewR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm blogPostForm
    defaultLayout $ do
        $(widgetFile "posts/new")

-- YesodPersist master => YesodPersistBackend master ~ SqlBackend => …

postPostNewR :: Handler RepHtml
postPostNewR = do
    articleForm  <- runFormPost $ renderBootstrap3 BootstrapBasicForm blogPostForm
    postTime     <- liftIO getCurrentTime
    
    case res of
      FormSuccess blogPost -> do
              blogPostId <- runDB $ insert blogPost
              redirect $ PostDetailsR blogPostId
      _ -> defaultLayout $(widgetFile "posts/new")

