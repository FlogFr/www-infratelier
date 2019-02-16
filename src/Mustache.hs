module Mustache where

import Protolude

import qualified Data.HashMap.Strict as HM hiding (map)
import Text.Mustache
import Text.Mustache.Types

import qualified Data.Vector as V
import Data


newtype UserMustache = UserMustache User
instance ToMustache UserMustache where
  toMustache (UserMustache user@User{}) = object
    [ "firstname" ~> (String $ fromMaybe "" $ userFirstName user)
    , "lastname" ~> (String $ fromMaybe "" $ userLastName user)
    , "profilePicture" ~> (String $ fromMaybe "user-default.png" $ userProfilePicture user)
    , "email" ~> (String $ userEmail user)
    , "newsletter" ~> (Bool $ userNewsletter user)
    ]


newtype UrlMustache = UrlMustache Text
instance ToMustache UrlMustache where
  toMustache (UrlMustache url) = object
    [ "url" ~> (String url)
    ]

data MessageMustache = MessageMustache Message
instance ToMustache MessageMustache where
  toMustache (MessageMustache msg') = object
    [ "id" ~> (String . show . messageId $ msg')
    , "createdAt" ~> (String . show . messageCreatedAt $ msg')
    , "message" ~> (String . messageMessage $ msg')
    , "email" ~> (String . messageAuthorEmail $ msg')
    , "ip" ~> (String . messageIP $ msg')
    ]

data NewsletterSubscriberMustache = NewsletterSubscriberMustache NewsletterSubscriber
instance ToMustache NewsletterSubscriberMustache where
  toMustache (NewsletterSubscriberMustache sub') = object
    [ "id" ~> (String . show . newsletterSubscriberId $ sub')
    , "createdAt" ~> (String . show . newsletterSubscriberCreatedAt $ sub')
    , "email" ~> (String . newsletterSubscriberEmail $ sub')
    , "ip" ~> (String . newsletterSubscriberIP $ sub')
    ]

data NotificationMustache = NotificationMustache Text Text
instance ToMustache NotificationMustache where
  toMustache (NotificationMustache messageText messageLevel) = object
    [ "description" ~> (String $ messageText)
    , "level" ~> (String $ messageLevel)
    ]

newtype CategoryBlogMustache = CategoryBlogMustache Text
instance ToMustache CategoryBlogMustache where
  toMustache (CategoryBlogMustache cat) = Object $ HM.fromList [("name", String cat)]

newtype BlogPostTagMustache = BlogPostTagMustache Text
instance ToMustache BlogPostTagMustache where
  toMustache (BlogPostTagMustache tag) = object
    [ "tag" ~> (String tag) ]

data BlogPostEntryMustache = BlogPostEntryMustache Text BlogPostEntry 
instance ToMustache BlogPostEntryMustache where
  toMustache (BlogPostEntryMustache countryCode blogPostEntry) = object
    [ "slug" ~> (String . slug $ blogPostEntry)
    , "title" ~> (String . title $ blogPostEntry)
    , "summary" ~> (String . summary $ blogPostEntry)
    , "authorName" ~> (String . author_name $ blogPostEntry)
    , "authorEmail" ~> (String . author_email $ blogPostEntry)
    , "createdAt" ~> (String . show . created $ blogPostEntry)
    , "updatedAt" ~> (String . show . updated $ blogPostEntry)
    , "tags" ~> (V.fromList (toMustache . BlogPostTagMustache <$> (tags blogPostEntry)))
    , "image" ~> (String . image $ blogPostEntry)
    , "countryCode" ~> (String countryCode)
    , "category" ~> (String . category $ blogPostEntry)
    ]

data BlogPostCategoryEntriesMustache = BlogPostCategoryEntriesMustache Text Text [BlogPostEntry]
instance ToMustache BlogPostCategoryEntriesMustache where
  toMustache (BlogPostCategoryEntriesMustache blogPostEntryCategoryName blogPostEntryCountryCode entries) = object
    [ "name" ~> (String blogPostEntryCategoryName)
    , "entries" ~> (V.fromList (toMustache . (BlogPostEntryMustache blogPostEntryCountryCode) <$> entries))
    ]

data BreadcrumbEntryMustache = BreadcrumbEntryMustache (Text, Text)
instance ToMustache BreadcrumbEntryMustache where
  toMustache (BreadcrumbEntryMustache (entry, href)) = object
    [ "name" ~> (String entry)
    , "href" ~> (String href)
    ]
