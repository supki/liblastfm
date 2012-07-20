{-# LANGUAGE OverloadedStrings #-}
module Network.Lastfm.Error
  ( LastfmError(..)
  , disambiguate
  ) where

import Network.Curl (CurlCode)


data LastfmError
  = DoesntExist
  | InvalidService
  | InvalidMethod
  | AuthenticationFailed
  | InvalidFormat
  | InvalidParameters
  | InvalidResource
  | OperationFailed
  | InvalidSessionKey
  | InvalidAPIKey
  | ServiceOffline
  | SubscribersOnly
  | InvalidMethodSignature
  | TokenHasNotAuthorized
  | NotForStreaming
  | TemporaryUnavailable
  | LoginRequired
  | TrialExpired
  | DoesntExistAgain
  | NotEnoughContent
  | NotEnoughMembers
  | NotEnoughFans
  | NotEnoughNeighbours
  | NoPeakRadio
  | RadioNotFound
  | SuspendedAPIKey
  | Deprecated
  | RateLimitExceeded
  | UnknownError Int
  | CurlError CurlCode


instance Show LastfmError where
  show e = unwords $ case e of
    DoesntExist            → ["DoesntExist:",            "This error does not exist"]
    InvalidService         → ["InvalidService:",         "This service does not exist"]
    InvalidMethod          → ["InvalidMethod:",          "No method with that name in this package"]
    AuthenticationFailed   → ["AuthenticationFailed:",   "You do not have permissions to access the service"]
    InvalidFormat          → ["InvalidFormat:",          "This service doesn't exist in that format"]
    InvalidParameters      → ["InvalidParameters:",      "Your request is missing a required parameter"]
    InvalidResource        → ["InvalidResource:",        "Invalid resource specified"]
    OperationFailed        → ["OperationFailed:",        "Something else went wrong"]
    InvalidSessionKey      → ["InvalidSessionKey:",      "Please re-authenticate"]
    InvalidAPIKey          → ["InvalidAPIKey:",          "You must be granted a valid key by last.fm"]
    ServiceOffline         → ["ServiceOffline:",         "This service is temporarily offline. Try again later."]
    SubscribersOnly        → ["SubscribersOnly:",        "This station is only available to paid last.fm subscribers"]
    InvalidMethodSignature → ["InvalidMethodSignature:", "Invalid method signature supplied"]
    TokenHasNotAuthorized  → ["TokenHasNotAuthorized:",  "This token has not been authorized"]
    NotForStreaming        → ["NotForStreaming:",        "This item is not available for streaming."]
    TemporaryUnavailable   → ["TemporaryUnavailable:",   "The service is temporarily unavailable, please try again."]
    LoginRequired          → ["LoginRequired:",          "Login: User requires to be logged in"]
    TrialExpired           → ["TrialExpired:",           "This user has no free radio plays left. Subscription required."]
    DoesntExistAgain       → ["DoesntExistAgain:",       "This error does not exist"]
    NotEnoughContent       → ["NotEnoughContent:",       "There is not enough content to play this station"]
    NotEnoughMembers       → ["NotEnoughMembers:",       "This group does not have enough members for radio"]
    NotEnoughFans          → ["NotEnoughFans:",          "This artist does not have enough fans for for radio"]
    NotEnoughNeighbours    → ["NotEnoughNeighbours:",    "There are not enough neighbours for radio"]
    NoPeakRadio            → ["NoPeakRadio:",            "This user is not allowed to listen to radio during peak usage"]
    RadioNotFound          → ["RadioNotFound:",          "Radio station not found"]
    SuspendedAPIKey        → ["SuspendedAPIKey:",        "Access for your account has been suspended, please contact Last.fm"]
    Deprecated             → ["Deprecated:",             "This type of request is no longer supported"]
    RateLimitExceeded      → ["RateLimitExceeded:",      "Your IP has made too many requests in a short period"]
    UnknownError n         → ["UnknownError:",           "Lastfm API specs say nothing about this particular error:", show n]
    CurlError s            → ["CurlError:", show s]


disambiguate ∷ Int → LastfmError
disambiguate n = case n of
  1 → DoesntExist
  2 → InvalidService
  3 → InvalidMethod
  4 → AuthenticationFailed
  5 → InvalidFormat
  6 → InvalidParameters
  7 → InvalidResource
  8 → OperationFailed
  9 → InvalidSessionKey
  10 → InvalidAPIKey
  11 → ServiceOffline
  12 → SubscribersOnly
  13 → InvalidMethodSignature
  14 → TokenHasNotAuthorized
  15 → NotForStreaming
  16 → TemporaryUnavailable
  17 → LoginRequired
  18 → TrialExpired
  19 → DoesntExistAgain
  20 → NotEnoughContent
  21 → NotEnoughMembers
  22 → NotEnoughFans
  23 → NotEnoughNeighbours
  24 → NoPeakRadio
  25 → RadioNotFound
  26 → SuspendedAPIKey
  27 → Deprecated
  28 → RateLimitExceeded
  _ → UnknownError n
