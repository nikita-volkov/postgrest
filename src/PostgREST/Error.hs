{-|
Module      : PostgREST.Error
Description : PostgREST error HTTP responses
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Error
  ( errorResponseFor
  , ApiRequestError(..)
  , QPError(..)
  , RangeError(..)
  , SchemaCacheError(..)
  , PgError(..)
  , Error(..)
  , JwtError (..)
  , JwtDecodeError(..)
  , JwtClaimsError(..)
  , errorPayload
  , status
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.CaseInsensitive      as CI
import qualified Data.FuzzySet             as Fuzzy
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Internal         as M
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Hasql.Errors              as SQL
import qualified Hasql.Pool                as SQL
import qualified Hasql.Session             as SQL
import qualified Network.HTTP.Types.Status as HTTP

import Data.Aeson  ((.:), (.:?), (.=))
import Network.Wai (Response, responseLBS)

import Network.HTTP.Types.Header (Header)

import           PostgREST.MediaType (MediaType (..))
import qualified PostgREST.MediaType as MediaType

import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..),
                                           Schema)
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.SchemaCache.Routine      (Routine (..),
                                           RoutineParam (..))
import PostgREST.SchemaCache.Table        (Table (..))
import Protolude


class (ErrorBody a, JSON.ToJSON a) => PgrstError a where
  status   :: a -> HTTP.Status
  headers  :: a -> [Header]

  errorPayload :: a -> LByteString
  errorPayload = JSON.encode

  errorResponseFor :: a -> Response
  errorResponseFor err =
    let
      baseHeader = MediaType.toContentType MTApplicationJSON
      cLHeader body = (,) "Content-Length" (BS.pack $ show $ LBS.length body) :: Header
    in
    responseLBS (status err) (baseHeader : cLHeader (errorPayload err) : headers err) $ errorPayload err

class ErrorBody a where
  code    :: a -> Text
  message :: a -> Text
  details :: a -> Maybe JSON.Value
  hint    :: a -> Maybe JSON.Value

data ApiRequestError
  = AggregatesNotAllowed
  | MediaTypeError [ByteString]
  | InvalidBody ByteString
  | InvalidFilters
  | InvalidPreferences [ByteString]
  | InvalidRange RangeError
  | InvalidRpcMethod ByteString
  | NotEmbedded Text
  | NotImplemented Text
  | PutLimitNotAllowedError
  | QueryParamError QPError
  | RelatedOrderNotToOne Text Text
  | UnacceptableFilter Text
  | UnacceptableSchema Text [Text]
  | UnsupportedMethod ByteString
  | GucHeadersError
  | GucStatusError
  | PutMatchingPkError
  | SingularityError Integer
  | PGRSTParseError RaiseError
  | MaxAffectedViolationError Integer
  | InvalidResourcePath
  | OpenAPIDisabled
  | MaxAffectedRpcViolation
  deriving Show

data QPError = QPError Text Text
  deriving Show

data RaiseError
  = MsgParseError ByteString
  | DetParseError ByteString
  | NoDetail
  deriving Show

data RangeError
  = NegativeLimit
  | LowerGTUpper
  | OutOfBounds Text Text
  deriving Show

instance PgrstError ApiRequestError where
  status AggregatesNotAllowed{}      = HTTP.status400
  status MediaTypeError{}            = HTTP.status406
  status InvalidBody{}               = HTTP.status400
  status InvalidFilters              = HTTP.status405
  status InvalidPreferences{}        = HTTP.status400
  status InvalidRpcMethod{}          = HTTP.status405
  status InvalidRange{}              = HTTP.status416

  status NotEmbedded{}               = HTTP.status400
  status NotImplemented{}            = HTTP.status400
  status PutLimitNotAllowedError     = HTTP.status400
  status QueryParamError{}           = HTTP.status400
  status RelatedOrderNotToOne{}      = HTTP.status400
  status UnacceptableFilter{}        = HTTP.status400
  status UnacceptableSchema{}        = HTTP.status406
  status UnsupportedMethod{}         = HTTP.status405
  status GucHeadersError             = HTTP.status500
  status GucStatusError              = HTTP.status500
  status PutMatchingPkError          = HTTP.status400
  status SingularityError{}          = HTTP.status406
  status PGRSTParseError{}           = HTTP.status500
  status MaxAffectedViolationError{} = HTTP.status400
  status InvalidResourcePath         = HTTP.status404
  status OpenAPIDisabled             = HTTP.status404
  status MaxAffectedRpcViolation     = HTTP.status400

  headers _ = mempty

-- Error codes:
--
-- Error codes are grouped by common modules or characteristics
-- New group of errors will be added at the end of all the groups and will have the next prefix in the sequence
-- Keep the "PGRST" prefix in every code for an easier search/grep
-- They are grouped as following:
--
-- PGRST0xx -> Connection Error
-- PGRST1xx -> ApiRequest Error
-- PGRST2xx -> SchemaCache Error
-- PGRST3xx -> JWT authentication Error
-- PGRSTXxx -> Internal Hasql Error

instance ErrorBody ApiRequestError where
  -- CODE: Text
  code QueryParamError{}           = "PGRST100"
  code InvalidRpcMethod{}          = "PGRST101"
  code InvalidBody{}               = "PGRST102"
  code InvalidRange{}              = "PGRST103"
  -- code ParseRequestError           = "PGRST104" -- no longer used
  code InvalidFilters              = "PGRST105"
  code UnacceptableSchema{}        = "PGRST106"
  code MediaTypeError{}            = "PGRST107"
  code NotEmbedded{}               = "PGRST108"
  -- code LimitNoOrderError           = "PGRST109" -- no longer used
  -- code OffLimitsChangesError       = "PGRST110" -- no longer used
  code GucHeadersError             = "PGRST111"
  code GucStatusError              = "PGRST112"
  -- code BinaryFieldError            = "PGRST113" -- no longer used
  code PutLimitNotAllowedError     = "PGRST114"
  code PutMatchingPkError          = "PGRST115"
  code SingularityError{}          = "PGRST116"
  code UnsupportedMethod{}         = "PGRST117"
  code RelatedOrderNotToOne{}      = "PGRST118"
  -- code SpreadNotToOne              = "PGRST109" -- no longer used
  code UnacceptableFilter{}        = "PGRST120"
  code PGRSTParseError{}           = "PGRST121"
  code InvalidPreferences{}        = "PGRST122"
  code AggregatesNotAllowed        = "PGRST123"
  code MaxAffectedViolationError{} = "PGRST124"
  code InvalidResourcePath         = "PGRST125"
  code OpenAPIDisabled             = "PGRST126"
  code NotImplemented{}            = "PGRST127"
  code MaxAffectedRpcViolation     = "PGRST128"

  -- MESSAGE: Text
  message (QueryParamError (QPError msg _)) = msg
  message (InvalidRpcMethod method)    = "Cannot use the " <> T.decodeUtf8 method <> " method on RPC"
  message (InvalidBody errorMessage)   = T.decodeUtf8 errorMessage
  message (InvalidRange _)             = "Requested range not satisfiable"
  message InvalidFilters               = "Filters must include all and only primary key columns with 'eq' operators"
  message (UnacceptableSchema sch _)   = "Invalid schema: " <> sch
  message (MediaTypeError cts)         = "None of these media types are available: " <> T.intercalate ", " (map T.decodeUtf8 cts)
  message (NotEmbedded resource)       = "'" <> resource <> "' is not an embedded resource in this request"
  message GucHeadersError              = "response.headers guc must be a JSON array composed of objects with a single key and a string value"
  message GucStatusError               = "response.status guc must be a valid status code"
  message PutLimitNotAllowedError      = "limit/offset querystring parameters are not allowed for PUT"
  message PutMatchingPkError           = "Payload values do not match URL in primary key column(s)"
  message (SingularityError _)         = "Cannot coerce the result to a single JSON object"
  message (UnsupportedMethod method)   = "Unsupported HTTP method: " <> T.decodeUtf8 method
  message (RelatedOrderNotToOne _ target) = "A related order on '" <> target <> "' is not possible"
  message (UnacceptableFilter target)    = "Bad operator on the '" <> target <> "' embedded resource"
  message (PGRSTParseError _)            = "Could not parse JSON in the \"RAISE SQLSTATE 'PGRST'\" error"
  message (InvalidPreferences _)         = "Invalid preferences given with handling=strict"
  message AggregatesNotAllowed           = "Use of aggregate functions is not allowed"
  message (MaxAffectedViolationError _)  = "Query result exceeds max-affected preference constraint"
  message InvalidResourcePath            = "Invalid path specified in request URL"
  message OpenAPIDisabled                = "Root endpoint metadata is disabled"
  message (NotImplemented _)             = "Feature not implemented"
  message MaxAffectedRpcViolation        = "Function must return SETOF or TABLE when max-affected preference is used with handling=strict"

  -- DETAILS: Maybe JSON.Value
  details (QueryParamError (QPError _ dets)) = Just $ JSON.String dets
  details (InvalidRange rangeError) = Just $
    case rangeError of
       NegativeLimit           -> "Limit should be greater than or equal to zero."
       LowerGTUpper            -> "The lower boundary must be lower than or equal to the upper boundary in the Range header."
       OutOfBounds lower total -> JSON.String $ "An offset of " <> lower <> " was requested, but there are only " <> total <> " rows."
  details (SingularityError n) = Just $ JSON.String $ T.unwords ["The result contains", T.pack (show n), "rows"]
  details (RelatedOrderNotToOne origin target) = Just $ JSON.String $ "'" <> origin <> "' and '" <> target <> "' do not form a many-to-one or one-to-one relationship"
  details (UnacceptableFilter _)      = Just "Only is null or not is null filters are allowed on embedded resources"
  details (PGRSTParseError raiseErr) = Just $ JSON.String $ pgrstParseErrorDetails raiseErr
  details (InvalidPreferences prefs) = Just $ JSON.String $ T.decodeUtf8 ("Invalid preferences: " <> BS.intercalate ", " prefs)
  details (MaxAffectedViolationError n) = Just $ JSON.String $ T.unwords ["The query affects", T.pack (show n), "rows"]
  details (NotImplemented details') = Just $ JSON.String details'

  details _ = Nothing

  -- HINT: Maybe JSON.Value
  hint (NotEmbedded resource) = Just $ JSON.String $ "Verify that '" <> resource <> "' is included in the 'select' query parameter."
  hint (PGRSTParseError raiseErr) = Just $ JSON.String $ pgrstParseErrorHint raiseErr
  hint (UnacceptableSchema _ schemas) = Just $ JSON.String $ "Only the following schemas are exposed: "  <> T.intercalate ", " schemas

  hint _ = Nothing

instance JSON.ToJSON ApiRequestError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

data SchemaCacheError
  = AmbiguousRelBetween Text Text [Relationship]
  | AmbiguousRpc [Routine]
  | NoRelBetween Text Text (Maybe Text) Text RelationshipsMap
  | NoRpc Text Text [Text] MediaType Bool [QualifiedIdentifier] [Routine]
  | ColumnNotFound Text Text
  | TableNotFound Text Text [Table]
  deriving Show

instance PgrstError SchemaCacheError where
  status AmbiguousRelBetween{} = HTTP.status300
  status AmbiguousRpc{}        = HTTP.status300
  status NoRelBetween{}        = HTTP.status400
  status NoRpc{}               = HTTP.status404
  status ColumnNotFound{}      = HTTP.status400
  status TableNotFound{}       = HTTP.status404

  headers _ = mempty

instance ErrorBody SchemaCacheError where
  code NoRelBetween{}        = "PGRST200"
  code AmbiguousRelBetween{} = "PGRST201"
  code NoRpc{}               = "PGRST202"
  code AmbiguousRpc{}        = "PGRST203"
  code ColumnNotFound{}      = "PGRST204"
  code TableNotFound{}       = "PGRST205"

  message (NoRelBetween parent child _ _ _)  = "Could not find a relationship between '" <> parent <> "' and '" <> child <> "' in the schema cache"
  message (AmbiguousRelBetween parent child _) = "Could not embed because more than one relationship was found for '" <> parent <> "' and '" <> child <> "'"
  message (NoRpc schema procName argumentKeys contentType isInvPost _ _) = "Could not find the function " <> func <> (if onlySingleParams then "" else fmtPrms prmsMsg) <> " in the schema cache"
      where
        onlySingleParams = isInvPost && contentType `elem` [MTTextPlain, MTTextXML, MTOctetStream]
        func = schema <> "." <> procName
        prms = T.intercalate ", " argumentKeys
        prmsMsg = "(" <> prms <> ")"
        fmtPrms p = if null argumentKeys then " without parameters" else p
  message (AmbiguousRpc procs) = "Could not choose the best candidate function between: " <> T.intercalate ", " [pdSchema p <> "." <> pdName p <> "(" <> T.intercalate ", " [ppName a <> " => " <> ppType a | a <- pdParams p] <> ")" | p <- procs]
  message (ColumnNotFound rel col) = "Could not find the '" <> col <> "' column of '" <> rel <> "' in the schema cache"
  message (TableNotFound schemaName relName _) = "Could not find the table '" <> schemaName <> "." <> relName <> "' in the schema cache"

  details (NoRelBetween parent child embedHint schema _) = Just $ JSON.String $ "Searched for a foreign key relationship between '" <> parent <> "' and '" <> child <> maybe mempty ("' using the hint '" <>) embedHint <> "' in the schema '" <> schema <> "', but no matches were found."
  details (AmbiguousRelBetween _ _ rels)       = Just $ JSON.toJSONList (compressedRel <$> rels)
  details (NoRpc schema procName argumentKeys contentType isInvPost _ _) =
      Just $ JSON.String $ "Searched for the function " <> func <>
        (case (isInvPost, contentType) of
           (True, MTTextPlain)       -> " with a single unnamed text parameter"
           (True, MTTextXML)         -> " with a single unnamed xml parameter"
           (True, MTOctetStream)     -> " with a single unnamed bytea parameter"
           (True, MTApplicationJSON) -> fmtPrms prmsDet <> " or with a single unnamed json/jsonb parameter"
           _                         -> fmtPrms prmsDet
        ) <> ", but no matches were found in the schema cache."
      where
        func = schema <> "." <> procName
        prms = T.intercalate ", " argumentKeys
        prmsDet = " with parameter" <> (if length argumentKeys > 1 then "s " else " ") <> prms
        fmtPrms p = if null argumentKeys then " without parameters" else p

  details _ = Nothing

  hint (NoRelBetween parent child _ schema allRels) = JSON.String <$> noRelBetweenHint parent child schema allRels
  hint (AmbiguousRelBetween _ child rels)   = Just $ JSON.String $ "Try changing '" <> child <> "' to one of the following: " <> relHint rels <> ". Find the desired relationship in the 'details' key."
  -- The hint will be null in the case of single unnamed parameter functions
  hint (NoRpc schema procName argumentKeys contentType isInvPost allProcs overloadedProcs) =
    if onlySingleParams
      then Nothing
      else JSON.String <$> noRpcHint schema procName argumentKeys allProcs overloadedProcs
      where
        onlySingleParams = isInvPost && contentType `elem` [MTTextPlain, MTTextXML, MTOctetStream]
  hint (AmbiguousRpc _)      = Just "Try renaming the parameters or the function itself in the database so function overloading can be resolved"
  hint (TableNotFound schemaName relName tbls) = JSON.String <$> tableNotFoundHint schemaName relName tbls

  hint _ = Nothing

instance JSON.ToJSON SchemaCacheError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

toJsonPgrstError :: Text -> Text -> Maybe JSON.Value -> Maybe JSON.Value -> JSON.Value
toJsonPgrstError code' message' details' hint' = JSON.object [
    "code"     .= code'
  , "message"  .= message'
  , "details"  .= details'
  , "hint"     .= hint'
  ]

-- |
-- If no relationship is found then:
--
-- Looks for parent suggestions if parent not found
-- Looks for child suggestions if parent is found but child is not
-- Gives no suggestions if both are found (it means that there is a problem with the embed hint)
--
-- >>> :set -Wno-missing-fields
-- >>> let qi t = QualifiedIdentifier "api" t
-- >>> let rel ft = Relationship{relForeignTable = qi ft}
-- >>> let rels = HM.fromList [((qi "films", "api"), [rel "directors", rel "roles", rel "actors"])]
--
-- >>> noRelBetweenHint "film" "directors" "api" rels
-- Just "Perhaps you meant 'films' instead of 'film'."
--
-- >>> noRelBetweenHint "films" "role" "api" rels
-- Just "Perhaps you meant 'roles' instead of 'role'."
--
-- >>> noRelBetweenHint "films" "role" "api" rels
-- Just "Perhaps you meant 'roles' instead of 'role'."
--
-- >>> noRelBetweenHint "films" "actors" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "noclosealternative" "roles" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "films" "noclosealternative" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "films" "noclosealternative" "noclosealternative" rels
-- Nothing
--
noRelBetweenHint :: Text -> Text -> Schema -> RelationshipsMap -> Maybe Text
noRelBetweenHint parent child schema allRels = ("Perhaps you meant '" <>) <$>
  if isJust findParent
    then (<> "' instead of '" <> child <> "'.") <$> suggestChild
    else (<> "' instead of '" <> parent <> "'.") <$> suggestParent
  where
    findParent = HM.lookup (QualifiedIdentifier schema parent, schema) allRels
    fuzzySetOfParents  = Fuzzy.fromList [qiName (fst p) | p <- HM.keys allRels, snd p == schema]
    fuzzySetOfChildren = Fuzzy.fromList [qiName (relForeignTable c) | c <- fromMaybe [] findParent]
    suggestParent = Fuzzy.getOne fuzzySetOfParents parent
    -- Do not give suggestion if the child is found in the relations (weight = 1.0)
    suggestChild  = headMay [snd k | k <- Fuzzy.get fuzzySetOfChildren child, fst k < 1.0]

-- |
-- If no function is found with the given name, it does a fuzzy search to all the functions
-- in the same schema and shows the best match as hint.
--
-- >>> :set -Wno-missing-fields
-- >>> let procs = [(QualifiedIdentifier "api" "test"), (QualifiedIdentifier "api" "another"), (QualifiedIdentifier "private" "other")]
--
-- >>> noRpcHint "api" "testt" ["val", "param", "name"] procs []
-- Just "Perhaps you meant to call the function api.test"
--
-- >>> noRpcHint "api" "other" [] procs []
-- Just "Perhaps you meant to call the function api.another"
--
-- >>> noRpcHint "api" "noclosealternative" [] procs []
-- Nothing
--
-- If a function is found with the given name, but no params match, then it does a fuzzy search
-- to all the overloaded functions' params using the form "param1, param2, param3, ..."
-- and shows the best match as hint.
--
-- >>> let procsDesc = [Function {pdParams = [RoutineParam {ppName="val"}, RoutineParam {ppName="param"}, RoutineParam {ppName="name"}]}, Function {pdParams = [RoutineParam {ppName="id"}, RoutineParam {ppName="attr"}]}]
--
-- >>> noRpcHint "api" "test" ["vall", "pqaram", "nam"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(name, param, val)"
--
-- >>> noRpcHint "api" "test" ["val", "param"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(name, param, val)"
--
-- >>> noRpcHint "api" "test" ["id", "attrs"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(attr, id)"
--
-- >>> noRpcHint "api" "test" ["id"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(attr, id)"
--
-- >>> noRpcHint "api" "test" ["noclosealternative"] procs procsDesc
-- Nothing
--
noRpcHint :: Text -> Text -> [Text] -> [QualifiedIdentifier] -> [Routine] -> Maybe Text
noRpcHint schema procName params allProcs overloadedProcs =
  fmap (("Perhaps you meant to call the function " <> schema <> ".") <>) possibleProcs
  where
    fuzzySetOfProcs  = Fuzzy.fromList [qiName k | k <- allProcs, qiSchema k == schema]
    fuzzySetOfParams = Fuzzy.fromList $ listToText <$> [[ppName prm | prm <- pdParams ov] | ov <- overloadedProcs]
    -- Cannot do a fuzzy search like: Fuzzy.getOne [[Text]] [Text], where [[Text]] is the list of params for each
    -- overloaded function and [Text] the given params. This converts those lists to text to make fuzzy search possible.
    -- E.g. ["val", "param", "name"] into "(name, param, val)"
    listToText       = ("(" <>) . (<> ")") . T.intercalate ", " . sort
    possibleProcs
      | null overloadedProcs = Fuzzy.getOne fuzzySetOfProcs procName
      | otherwise            = (procName <>) <$> Fuzzy.getOne fuzzySetOfParams (listToText params)

-- |
-- Do a fuzzy search in all tables in the same schema and return closest result
tableNotFoundHint :: Text -> Text -> [Table] -> Maybe Text
tableNotFoundHint schema tblName tblList
  = fmap (\tbl -> "Perhaps you meant the table '" <> schema <> "." <> tbl <> "'") perhapsTable
    where
      perhapsTable = Fuzzy.getOne fuzzyTableSet tblName
      fuzzyTableSet = Fuzzy.fromList [ tableName tbl | tbl <- tblList, tableSchema tbl == schema]


compressedRel :: Relationship -> JSON.Value
-- An ambiguousness error cannot happen for computed relationships TODO refactor so this mempty is not needed
compressedRel ComputedRelationship{} = JSON.object mempty
compressedRel Relationship{..} =
  let
    fmtEls els = "(" <> T.intercalate ", " els <> ")"
  in
  JSON.object $
    ("embedding" .= (qiName relTable <> " with " <> qiName relForeignTable :: Text))
    : case relCardinality of
        M2M Junction{..} -> [
            "cardinality" .= ("many-to-many" :: Text)
          , "relationship" .= (qiName junTable <> " using " <> junConstraint1 <> fmtEls (snd <$> junColsSource) <> " and " <> junConstraint2 <> fmtEls (snd <$> junColsTarget))
          ]
        M2O cons relColumns -> [
            "cardinality" .= ("many-to-one" :: Text)
          , "relationship" .= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]
        O2O cons relColumns _ -> [
            "cardinality" .= ("one-to-one" :: Text)
          , "relationship" .= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]
        O2M cons relColumns -> [
            "cardinality" .= ("one-to-many" :: Text)
          , "relationship" .= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]

relHint :: [Relationship] -> Text
relHint rels = T.intercalate ", " (hintList <$> rels)
  where
    hintList Relationship{..} =
      let buildHint rel = "'" <> qiName relForeignTable <> "!" <> rel <> "'" in
      case relCardinality of
        M2M Junction{..} -> buildHint (qiName junTable)
        M2O cons _       -> buildHint cons
        O2O cons _ _     -> buildHint cons
        O2M cons _       -> buildHint cons
    -- An ambiguousness error cannot happen for computed relationships TODO refactor so this mempty is not needed
    hintList ComputedRelationship{} = mempty

pgrstParseErrorDetails :: RaiseError -> Text
pgrstParseErrorDetails err = case err of
  MsgParseError m -> "Invalid JSON value for MESSAGE: '" <> T.decodeUtf8 m <> "'"
  DetParseError d -> "Invalid JSON value for DETAIL: '" <> T.decodeUtf8 d <> "'"
  NoDetail        -> "DETAIL is missing in the RAISE statement"

pgrstParseErrorHint :: RaiseError -> Text
pgrstParseErrorHint err = case err of
  MsgParseError _ -> "MESSAGE must be a JSON object with obligatory keys: 'code', 'message' and optional keys: 'details', 'hint'."
  _               -> "DETAIL must be a JSON object with obligatory keys: 'status', 'headers' and optional key: 'status_text'."

data PgError = PgError Authenticated SQL.UsageError
  deriving Show

type Authenticated = Bool

instance PgrstError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers (PgError _ (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ (SQL.ServerStatementError (SQL.ServerError "PGRST" m d _ _p))))) =
    case parseRaisePGRST (T.encodeUtf8 m) (fmap T.encodeUtf8 d) of
      Right (_, r) -> map intoHeader (M.toList $ getHeaders r)
      Left e       -> headers e
    where
      intoHeader (k,v) = (CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v)

  headers err =
    if status err == HTTP.status401
       then [("WWW-Authenticate", "Bearer") :: Header]
       else mempty

proxyStatusHeader :: Text -> Header
proxyStatusHeader code' = ("Proxy-Status", "PostgREST; error=" <> T.encodeUtf8 code')

instance JSON.ToJSON PgError where
  toJSON (PgError _ usageError) = toJsonPgrstError
    (code usageError) (message usageError) (details usageError) (hint usageError)

instance ErrorBody PgError where
  code    (PgError _ usageError) = code usageError
  message (PgError _ usageError) = message usageError
  details (PgError _ usageError) = details usageError
  hint    (PgError _ usageError) = hint usageError

instance JSON.ToJSON SQL.UsageError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody SQL.UsageError where
  code    (SQL.ConnectionUsageError _)      = "PGRST000"
  code    (SQL.SessionUsageError e)         = code e
  code    SQL.AcquisitionTimeoutUsageError  = "PGRST003"

  message (SQL.ConnectionUsageError e) = SQL.toErrorMessage e
  message (SQL.SessionUsageError e)    = message e
  message SQL.AcquisitionTimeoutUsageError = "Timed out acquiring connection from connection pool."

  details (SQL.ConnectionUsageError e) = Just $ JSON.String $ SQL.toErrorMessage e
  details (SQL.SessionUsageError e)    = details e
  details SQL.AcquisitionTimeoutUsageError = Nothing

  hint    (SQL.ConnectionUsageError _)     = Nothing
  hint    (SQL.SessionUsageError e)        = hint e
  hint    SQL.AcquisitionTimeoutUsageError = Nothing

instance JSON.ToJSON SQL.SessionError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody SQL.SessionError where
  code (SQL.StatementSessionError _ _ _ _ _ e) = code e
  code (SQL.ScriptSessionError _ e)            = code e
  code (SQL.ConnectionSessionError _)          = "PGRST001"
  code (SQL.DriverSessionError _)              = "PGRSTX00"
  code (SQL.MissingTypesSessionError _)        = "PGRSTX01"

  message (SQL.StatementSessionError _ _ _ _ _ e) = message e
  message (SQL.ScriptSessionError _ e)            = message e
  message (SQL.ConnectionSessionError _)          = "Database client error. Retrying the connection."
  message (SQL.DriverSessionError msg)            = msg
  message (SQL.MissingTypesSessionError types)    = "Missing database types: " <> T.pack (show types)

  details (SQL.StatementSessionError _ _ _ _ _ e) = details e
  details (SQL.ScriptSessionError _ e)            = details e
  details (SQL.ConnectionSessionError msg)        = Just $ JSON.String msg
  details (SQL.DriverSessionError msg)            = Just $ JSON.String msg
  details (SQL.MissingTypesSessionError _)        = Nothing

  hint (SQL.StatementSessionError _ _ _ _ _ e) = hint e
  hint (SQL.ScriptSessionError _ e)            = hint e
  hint (SQL.ConnectionSessionError _)          = Nothing
  hint (SQL.DriverSessionError _)              = Nothing
  hint (SQL.MissingTypesSessionError _)        = Nothing

instance JSON.ToJSON SQL.StatementError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody SQL.StatementError where
  -- Special error raised with code PGRST, to allow full response control
  code (SQL.ServerStatementError (SQL.ServerError "PGRST" m d _ _)) =
    case parseRaisePGRST (T.encodeUtf8 m) (fmap T.encodeUtf8 d) of
      Right (r, _) -> getCode r
      Left e       -> code e
  code (SQL.ServerStatementError (SQL.ServerError c _ _ _ _)) = c

  code (SQL.UnexpectedResultStatementError _) = "PGRSTX00" -- Internal Error
  code (SQL.UnexpectedRowCountStatementError _ _ _) = "PGRSTX00"
  code (SQL.UnexpectedAmountOfColumnsStatementError _ _) = "PGRSTX00"
  code (SQL.UnexpectedColumnTypeStatementError _ _ _) = "PGRSTX00"
  code (SQL.RowStatementError _ _) = "PGRSTX00"

  message (SQL.ServerStatementError (SQL.ServerError "PGRST" m d _ _)) =
    case parseRaisePGRST (T.encodeUtf8 m) (fmap T.encodeUtf8 d) of
      Right (r, _) -> getMessage r
      Left e       -> message e
  message (SQL.ServerStatementError (SQL.ServerError _ m _ _ _)) = m
  message (SQL.UnexpectedResultStatementError msg) = msg
  message (SQL.UnexpectedRowCountStatementError _ _ _) = "Unexpected amount of rows"
  message (SQL.UnexpectedAmountOfColumnsStatementError _ _) = "Unexpected amount of columns"
  message (SQL.UnexpectedColumnTypeStatementError _ _ _) = "Unexpected column type"
  message (SQL.RowStatementError _ rowErr) = SQL.toErrorMessage rowErr

  details (SQL.ServerStatementError (SQL.ServerError "PGRST" m d _ _)) =
    case parseRaisePGRST (T.encodeUtf8 m) (fmap T.encodeUtf8 d) of
      Right (r, _) -> JSON.String <$> getDetails r
      Left e       -> details e
  details (SQL.ServerStatementError (SQL.ServerError _ _ d _ _)) = JSON.String <$> d
  details (SQL.UnexpectedResultStatementError msg) = Just $ JSON.String msg
  details (SQL.UnexpectedRowCountStatementError minRows maxRows actual) = 
    Just $ JSON.String $ "Expected " <> T.pack (show minRows) <> " to " <> T.pack (show maxRows) <> " rows, got " <> T.pack (show actual)
  details (SQL.UnexpectedAmountOfColumnsStatementError expected actual) = 
    Just $ JSON.String $ "Expected " <> T.pack (show expected) <> " columns, got " <> T.pack (show actual)
  details (SQL.UnexpectedColumnTypeStatementError colIdx expectedOid actualOid) = 
    Just $ JSON.String $ "Column " <> T.pack (show colIdx) <> ": expected OID " <> T.pack (show expectedOid) <> ", got " <> T.pack (show actualOid)
  details (SQL.RowStatementError rowIdx _) = Just $ JSON.String $ "Error in row " <> T.pack (show rowIdx)

  hint (SQL.ServerStatementError (SQL.ServerError "PGRST" m d _ _p)) =
    case parseRaisePGRST (T.encodeUtf8 m) (fmap T.encodeUtf8 d) of
      Right (r, _) -> JSON.String <$> getHint r
      Left e       -> hint e
  hint (SQL.ServerStatementError (SQL.ServerError _ _ _ h _)) = JSON.String <$> h
  hint _ = Nothing


pgErrorStatus :: Bool -> SQL.UsageError -> HTTP.Status
pgErrorStatus _      (SQL.ConnectionUsageError _) = HTTP.status503
pgErrorStatus _      SQL.AcquisitionTimeoutUsageError = HTTP.status504
pgErrorStatus _      (SQL.SessionUsageError (SQL.ConnectionSessionError _)) = HTTP.status503
pgErrorStatus authed (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ statementError)) =
  case statementError of
    SQL.ServerStatementError serverError ->
      serverErrorStatus authed serverError
    SQL.UnexpectedResultStatementError{} -> HTTP.status500
    SQL.UnexpectedRowCountStatementError{} -> HTTP.status500
    SQL.UnexpectedAmountOfColumnsStatementError{} -> HTTP.status500
    SQL.UnexpectedColumnTypeStatementError{} -> HTTP.status500
    SQL.RowStatementError{} -> HTTP.status500
pgErrorStatus authed (SQL.SessionUsageError (SQL.ScriptSessionError _ serverError)) =
  serverErrorStatus authed serverError
pgErrorStatus _ (SQL.SessionUsageError (SQL.DriverSessionError _)) = HTTP.status500
pgErrorStatus _ (SQL.SessionUsageError (SQL.MissingTypesSessionError _)) = HTTP.status500

serverErrorStatus :: Bool -> SQL.ServerError -> HTTP.Status
serverErrorStatus authed (SQL.ServerError c m d _ _) =
  case T.unpack c of
    '0':'8':_ -> HTTP.status503 -- pg connection err
    '0':'9':_ -> HTTP.status500 -- triggered action exception
    '0':'L':_ -> HTTP.status403 -- invalid grantor
    '0':'P':_ -> HTTP.status403 -- invalid role specification
    "23503"   -> HTTP.status409 -- foreign_key_violation
    "23505"   -> HTTP.status409 -- unique_violation
    "25006"   -> HTTP.status405 -- read_only_sql_transaction
    "21000"   -> -- cardinality_violation
      if T.isSuffixOf "requires a WHERE clause" m
        then HTTP.status400 -- special case for pg-safeupdate, which we consider as client error
        else HTTP.status500 -- generic function or view server error, e.g. "more than one row returned by a subquery used as an expression"
    "22023"   -> -- invalid_parameter_value. Catch nonexistent role error, see https://github.com/PostgREST/postgrest/issues/3601
      if T.isPrefixOf "role" m && T.isSuffixOf "does not exist" m
        then HTTP.status401 -- role in jwt does not exist
        else HTTP.status400
    '2':'5':_ -> HTTP.status500 -- invalid tx state
    '2':'8':_ -> HTTP.status403 -- invalid auth specification
    '2':'D':_ -> HTTP.status500 -- invalid tx termination
    '3':'8':_ -> HTTP.status500 -- external routine exception
    '3':'9':_ -> HTTP.status500 -- external routine invocation
    '3':'B':_ -> HTTP.status500 -- savepoint exception
    '4':'0':_ -> HTTP.status500 -- tx rollback
    "53400"   -> HTTP.status500 -- config limit exceeded
    '5':'3':_ -> HTTP.status503 -- insufficient resources
    '5':'4':_ -> HTTP.status500 -- too complex
    '5':'5':_ -> HTTP.status500 -- obj not on prereq state
    "57P01"   -> HTTP.status503 -- terminating connection due to administrator command
    '5':'7':_ -> HTTP.status500 -- operator intervention
    '5':'8':_ -> HTTP.status500 -- system error
    'F':'0':_ -> HTTP.status500 -- conf file error
    'H':'V':_ -> HTTP.status500 -- foreign data wrapper error
    "P0001"   -> HTTP.status400 -- default code for "raise"
    'P':'0':_ -> HTTP.status500 -- PL/pgSQL Error
    'X':'X':_ -> HTTP.status500 -- internal Error
    "42883"-> if T.isPrefixOf "function xmlagg(" m
      then HTTP.status406
      else HTTP.status404 -- undefined function
    "42P01"   -> HTTP.status404 -- undefined table
    "42P17"   -> HTTP.status500 -- infinite recursion
    "42501"   -> if authed then HTTP.status403 else HTTP.status401 -- insufficient privilege
    'P':'T':n -> fromMaybe HTTP.status500 (HTTP.mkStatus <$> readMaybe n <*> pure (T.encodeUtf8 m))
    "PGRST"   ->
      case parseRaisePGRST (T.encodeUtf8 m) (fmap T.encodeUtf8 d) of
        Right (_, r) -> maybe (toEnum $ getStatus r) (HTTP.mkStatus (getStatus r) . T.encodeUtf8) (getStatusText r)
        Left e       -> status e
    _         -> HTTP.status400


data Error
  = ApiRequestError ApiRequestError
  | SchemaCacheErr SchemaCacheError
  | JwtErr JwtError
  | NoSchemaCacheError
  | PgErr PgError
  deriving Show

data JwtError
  = JwtDecodeErr JwtDecodeError
  | JwtSecretMissing
  | JwtTokenRequired
  | JwtClaimsErr JwtClaimsError
  deriving Show

data JwtDecodeError
  = EmptyAuthHeader
  | UnexpectedParts Int
  | KeyError Text
  | BadAlgorithm Text
  | BadCrypto
  | UnsupportedTokenType
  | UnreachableDecodeError
  deriving Show

data JwtClaimsError
  = JWTExpired
  | JWTNotYetValid
  | JWTIssuedAtFuture
  | JWTNotInAudience
  | ParsingClaimsFailed
  | ExpClaimNotNumber
  | NbfClaimNotNumber
  | IatClaimNotNumber
  | AudClaimNotStringOrArray
  deriving Show

instance PgrstError Error where
  status (ApiRequestError err) = status err
  status (SchemaCacheErr err)  = status err
  status (JwtErr err)          = status err
  status NoSchemaCacheError    = HTTP.status503
  status (PgErr err)           = status err

  headers (ApiRequestError err)  = proxyStatusHeader (code err) : headers err
  headers (SchemaCacheErr err)   = proxyStatusHeader (code err) : headers err
  headers (JwtErr err)           = proxyStatusHeader (code err) : headers err
  headers (PgErr err)            = proxyStatusHeader (code err) : headers err
  headers err@NoSchemaCacheError = proxyStatusHeader (code err) : mempty

instance JSON.ToJSON Error where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody Error where
  code (ApiRequestError err) = code err
  code (SchemaCacheErr err)  = code err
  code (JwtErr err)          = code err
  code NoSchemaCacheError    = "PGRST002"
  code (PgErr err)           = code err

  message (ApiRequestError err) = message err
  message (SchemaCacheErr err)  = message err
  message (JwtErr err)          = message err
  message NoSchemaCacheError    = "Could not query the database for the schema cache. Retrying."
  message (PgErr err)           = message err

  details (ApiRequestError err) = details err
  details (SchemaCacheErr err)  = details err
  details (JwtErr err)          = details err
  details NoSchemaCacheError    = Nothing
  details (PgErr err)           = details err

  hint (ApiRequestError err) = hint err
  hint (SchemaCacheErr err)  = hint err
  hint (JwtErr err)          = hint err
  hint NoSchemaCacheError    = Nothing
  hint (PgErr err)           = hint err

instance PgrstError JwtError where
  status JwtDecodeErr{}   = HTTP.unauthorized401
  status JwtSecretMissing = HTTP.status500
  status JwtTokenRequired = HTTP.unauthorized401
  status JwtClaimsErr{}   = HTTP.unauthorized401

  headers e@(JwtDecodeErr _) = [invalidTokenHeader $ message e]
  headers JwtTokenRequired   = [requiredTokenHeader]
  headers e@(JwtClaimsErr _) = [invalidTokenHeader $ message e]
  headers _                  = mempty

instance JSON.ToJSON JwtError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody JwtError where
  code JwtSecretMissing = "PGRST300"
  code (JwtDecodeErr _) = "PGRST301"
  code JwtTokenRequired = "PGRST302"
  code (JwtClaimsErr _) = "PGRST303"

  message JwtSecretMissing = "Server lacks JWT secret"
  message (JwtDecodeErr e) = case e of
    EmptyAuthHeader        -> "Empty JWT is sent in Authorization header"
    UnexpectedParts n      -> "Expected 3 parts in JWT; got " <> T.pack (show n)
    KeyError _             -> "No suitable key or wrong key type"
    BadAlgorithm _         -> "Wrong or unsupported encoding algorithm"
    BadCrypto              -> "JWT cryptographic operation failed"
    UnsupportedTokenType   -> "Unsupported token type"
    UnreachableDecodeError -> "JWT couldn't be decoded"
  message JwtTokenRequired = "Anonymous access is disabled"
  message (JwtClaimsErr e) = case e of
    JWTExpired               -> "JWT expired"
    JWTNotYetValid           -> "JWT not yet valid"
    JWTIssuedAtFuture        -> "JWT issued at future"
    JWTNotInAudience         -> "JWT not in audience"
    ParsingClaimsFailed      -> "Parsing claims failed"
    ExpClaimNotNumber        -> "The JWT 'exp' claim must be a number"
    NbfClaimNotNumber        -> "The JWT 'nbf' claim must be a number"
    IatClaimNotNumber        -> "The JWT 'iat' claim must be a number"
    AudClaimNotStringOrArray -> "The JWT 'aud' claim must be a string or an array of strings"

  details (JwtDecodeErr jde) = case jde of
    KeyError dets     -> Just $ JSON.String dets
    BadAlgorithm dets -> Just $ JSON.String dets
    _                 -> Nothing
  details _ = Nothing

  hint _    = Nothing

invalidTokenHeader :: Text -> Header
invalidTokenHeader m =
  ("WWW-Authenticate", "Bearer error=\"invalid_token\", " <> "error_description=" <> encodeUtf8 m)

requiredTokenHeader :: Header
requiredTokenHeader = ("WWW-Authenticate", "Bearer")

-- For parsing byteString to JSON Object, used for allowing full response control
data PgRaiseErrMessage = PgRaiseErrMessage {
  getCode    :: Text,
  getMessage :: Text,
  getDetails :: Maybe Text,
  getHint    :: Maybe Text
}

data PgRaiseErrDetails = PgRaiseErrDetails {
  getStatus     :: Int,
  getStatusText :: Maybe Text,
  getHeaders    :: Map Text Text
}

instance JSON.FromJSON PgRaiseErrMessage where
  parseJSON (JSON.Object m) =
    PgRaiseErrMessage
      <$> m .: "code"
      <*> m .: "message"
      <*> m .:? "details"
      <*> m .:? "hint"

  parseJSON _ = mzero

instance JSON.FromJSON PgRaiseErrDetails where
  parseJSON (JSON.Object d) =
    PgRaiseErrDetails
      <$> d .: "status"
      <*> d .:? "status_text"
      <*> d .: "headers"

  parseJSON _ = mzero

parseRaisePGRST :: ByteString -> Maybe ByteString -> Either ApiRequestError (PgRaiseErrMessage, PgRaiseErrDetails)
parseRaisePGRST m d = do
  msgJson <- maybeToRight (PGRSTParseError $ MsgParseError m) (JSON.decodeStrict m)
  det <- maybeToRight (PGRSTParseError NoDetail) d
  detJson <- maybeToRight (PGRSTParseError $ DetParseError det) (JSON.decodeStrict det)
  return (msgJson, detJson)
