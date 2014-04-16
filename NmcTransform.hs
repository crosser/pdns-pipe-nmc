module NmcTransform ( seedNmcDom
                    , descendNmcDom
                    ) where

import Prelude hiding (lookup)
import Data.ByteString.Lazy (ByteString)
import Data.Map (lookup, delete, size)
import Control.Monad (foldM)
import Data.Aeson (decode)

import NmcDom

-- | Perform query and return error string or parsed domain object
queryNmcDom ::
  (String -> IO (Either String ByteString)) -- ^ query operation action
  -> String                                 -- ^ key
  -> IO (Either String NmcDom)              -- ^ error string or domain
queryNmcDom queryOp key = do
  l <- queryOp key
  case l of
    Left estr -> return $ Left estr
    Right str -> case decode str :: Maybe NmcDom of
      Nothing  -> return $ Left $ "Unparseable value: " ++ (show str)
      Just dom -> return $ Right dom

-- | Try to fetch "import" object and merge it into the base domain
--   Original "import" element is removed, but new imports from the
--   imported objects are processed recursively until there are none.
mergeImport ::
  (String -> IO (Either String ByteString)) -- ^ query operation action
  -> Int                                    -- ^ recursion counter
  -> NmcDom                                 -- ^ base domain
  -> IO (Either String NmcDom)              -- ^ result with merged import
mergeImport queryOp depth base = do
  let
    mbase = mergeSelf base
    base' = mbase {domImport = Nothing}
  -- print base
  if depth <= 0 then return $ Left "Nesting of imports is too deep"
  else case domImport mbase of
    Nothing  -> return $ Right base'
    Just keys -> foldM mergeImport1 (Right base') keys
      where
        mergeImport1 (Left  err) _   = return $ Left err
        mergeImport1 (Right acc) key = do
          sub <- queryNmcDom queryOp key
          case sub of
            Left  err  -> return $ Left err
            Right sub' -> mergeImport queryOp (depth - 1) $
                                sub' `mergeNmcDom` acc

-- | If there is an element in the map with key "", merge the contents
--   and remove this element. Do this recursively.
mergeSelf :: NmcDom -> NmcDom
mergeSelf base =
  let
    map   = domMap base
    base' = base {domMap = removeSelf map}
    removeSelf Nothing    = Nothing
    removeSelf (Just map) = if size map' == 0 then Nothing else Just map'
      where map' = delete "" map
  in
    case map of
      Nothing   -> base'
      Just map' ->
        case lookup "" map' of
          Nothing  -> base'
          Just sub -> (mergeSelf sub) `mergeNmcDom` base'
        -- recursion depth limited by the size of the record

-- | SRV case - remove everyting and filter SRV records
normalizeSrv :: String -> String -> NmcDom -> NmcDom
normalizeSrv serv proto dom =
  emptyNmcDom {domService = fmap (filter needed) (domService dom)}
    where
      needed r = srvName r == serv && srvProto r == proto

-- | Presence of some elements require removal of some others
normalizeDom :: NmcDom -> NmcDom
normalizeDom dom = foldr id dom [ srvNormalizer
                                , translateNormalizer
                                , nsNormalizer
                                ]
  where
    nsNormalizer dom = case domNs dom of
      Nothing  -> dom
      Just ns  -> emptyNmcDom { domNs = domNs dom, domEmail = domEmail dom }
    translateNormalizer dom = case domTranslate dom of
      Nothing  -> dom
      Just tr  -> dom { domMap = Nothing }
    srvNormalizer dom = dom { domService = Nothing, domMx = makemx }
      where
        makemx = case domService dom of
          Nothing  -> Nothing
          Just svl -> Just $ map makerec (filter needed svl)
            where
              needed sr = srvName sr == "smtp"
                        && srvProto sr == "tcp"
                        && srvPort sr == 25
              makerec sr = (show (srvPrio sr)) ++ " " ++ (srvHost sr)

-- | Merge imports and Selfs and follow the maps tree to get dom
descendNmcDom ::
  (String -> IO (Either String ByteString)) -- ^ query operation action
  -> [String]                               -- ^ subdomain chain
  -> NmcDom                                 -- ^ base domain
  -> IO (Either String NmcDom)              -- ^ fully processed result
descendNmcDom queryOp subdom base = do
  base' <- mergeImport queryOp 10 base
  case subdom of
    []   -> return $ fmap normalizeDom base'
    -- A hack to handle SRV records: don't descend if ["_prot","_serv"]
    [('_':p),('_':s)] -> return $ fmap (normalizeSrv s p) base'
    d:ds ->
      case base' of
        Left err     -> return base'
        Right base'' ->
          case domMap base'' of
            Nothing  -> return $ Right emptyNmcDom
            Just map ->
              case lookup d map of
                Nothing  -> return $ Right emptyNmcDom
                Just sub -> descendNmcDom queryOp ds sub

-- | Initial NmcDom populated with "import" only, suitable for "descend"
seedNmcDom ::
  String        -- ^ domain key (without namespace prefix)
  -> NmcDom     -- ^ resulting seed domain
seedNmcDom dn = emptyNmcDom { domImport = Just (["d/" ++ dn])}
