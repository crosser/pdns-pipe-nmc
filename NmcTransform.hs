module NmcTransform ( seedNmcDom
                    , descendNmcDom
                    ) where

import Prelude hiding (lookup)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (splitOn, pack, unpack)
import Data.Map.Lazy (empty, lookup, delete, size, singleton
                     , foldrWithKey, insert, insertWith)
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
    mbase = (expandSrv . splitSubdoms . mergeSelf) base
    base' = mbase {domImport = Nothing}
  -- print base
  if depth <= 0 then return $ Left "Nesting of imports is too deep"
  else case ((domDelegate mbase), (domImport mbase)) of
    (Nothing,  Nothing  ) -> return $ Right base'
    (Nothing,  Just keys) -> foldM mergeImport1 (Right base') keys
    (Just key, _        ) -> mergeImport1 (Right emptyNmcDom) key
  where
    mergeImport1 (Left  err) _   = return $ Left err -- can never happen
    mergeImport1 (Right acc) key = do
      sub <- queryNmcDom queryOp key
      case sub of
        Left  err  -> return $ Left err
        Right sub' -> mergeImport queryOp (depth - 1) $ sub' `mergeNmcDom` acc

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

-- | replace Service with Srv down in the Map
expandSrv :: NmcDom -> NmcDom
expandSrv base =
  let
    base' = base { domService = Nothing }
  in
    case domService base of
      Nothing -> base'
      Just sl -> foldr addSrvMx base' sl
        where
          addSrvMx sr acc = sub1 `mergeNmcDom` acc
            where
              sub1 = emptyNmcDom { domMap = Just (singleton proto sub2)
                                 , domMx = maybemx}
              sub2 = emptyNmcDom { domMap = Just (singleton srvid sub3) }
              sub3 = emptyNmcDom { domSrv = Just [srvStr] }
              proto = "_" ++ (srvProto sr)
              srvid = "_" ++ (srvName sr)
              srvStr =  (show (srvPrio sr)) ++ " "
                     ++ (show (srvWeight sr)) ++ " "
                     ++ (show (srvPort sr)) ++ " "
                     ++ (srvHost sr)
              maybemx =
                if srvName sr == "smtp"
                   && srvProto sr == "tcp"
                   && srvPort sr == 25
                then Just [(show (srvPrio sr)) ++ " " ++ (srvHost sr)]
                else Nothing

-- | Convert map elements of the form "subN...sub2.sub1.dom.bit"
--   into nested map and merge it
splitSubdoms :: NmcDom -> NmcDom
splitSubdoms base =
  let
    base' = base { domMap = Nothing }
  in
    case domMap base of
      Nothing -> base'
      Just sdmap -> (emptyNmcDom { domMap = Just sdmap' }) `mergeNmcDom` base'
        where
          sdmap' = foldrWithKey stow empty sdmap
          stow fqdn sdom acc = insertWith mergeNmcDom fqdn' sdom' acc
            where
              (fqdn', sdom') =
                nest (map unpack (splitOn (pack ".") (pack fqdn)), sdom)
              nest ([], v)   = (fqdn, v) -- can split result be empty?
              nest ([k], v)  = (k, v)
              nest (k:ks, v) =
                nest (ks, emptyNmcDom { domMap = Just (singleton k v) })
 
-- | Presence of some elements require removal of some others
normalizeDom :: NmcDom -> NmcDom
normalizeDom dom = foldr id dom [ translateNormalizer
                                , nsNormalizer
                                ]
  where
    nsNormalizer dom = case domNs dom of
      Nothing  -> dom
      Just ns  -> emptyNmcDom { domNs = domNs dom, domEmail = domEmail dom }
    translateNormalizer dom = case domTranslate dom of
      Nothing  -> dom
      Just tr  -> dom { domMap = Nothing }

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
