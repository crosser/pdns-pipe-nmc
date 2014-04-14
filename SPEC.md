% JSON Domain Format as Implemented by Pdns-Pipe-Nmc

## Data Format

`DomObj` is a JSON object, specifically a `Map` (not an `Array`), or a
`String` containing a dotted quad (see Note below).

### `DomObj` Attributes

| Key         | Type                 | Comment                         |
|-------------|----------------------|---------------------------------|
| service     | Array(SrvObj)        | Located two levels above pos.   |
| ip          | Array(String)        | Dotted quad "1.2.3.4"           |
| ip6         | Array(String)        | Semicolon format "DEAD::BEEF"   |
| tor         | String               | Onion name                      |
| i2p         | I2pObj               |                                 |
| freenet     | String               |                                 |
| alias       | String               | Nullifies other attributes      |
| translate   | String               | Nullifies other attributes      |
| email       | String               | Used in `SOA`                   |
| loc         | String               | Format suitable for `LOC`       |
| info        | JsonObj              | Currently unspecified           |
| ns          | Array(String)        | Domain names as in `NS`         |
| delegate    | String               | Nullifies other attributes      |
| import      | String               | "Deep" merges into current obj. |
| map         | Map(String:DomObj)   | Tree of subdomain objects       |
| fingerprint | Array(String)        |                                 |
| tls         | *TlsMap*             | See note below                  |
| ds          | Array(Array(String)) |                                 |

### Notes:

* Any attribute specified as `Array(String)` may be present in the
  JSON document as `String`, which is interpreted the same way as
  an `Array` containing a single `String` element.
* In any place where `DomObj` is expected there may be a `String`, which
  is interpreted as an IPv4 address. In other words, a string `"1.2.3.4"`
  is interpreted the same way as the object `"{\"ip\":\"1.2.3.4\"}"`
  Such "shorthand" DomObj can be present at the top level or as a value
  in the `"map"` attribute.
* *TlsMap* is `Map(String:Map(String:Array(TlsObj)))`, where `TlsObj` is
  a heterogenous Array of 3 elements: `[Int, String, Int]`. It is not
  used by the DNS bridge.

### SrvObj format

`SrvObj` is a heterogenous Array of fixed size containing 6 elements:

| N | Type   | Meaning  |
|---|--------|----------|
| 0 | String | Service  |
| 1 | String | Protocol |
| 2 | Int    | Priority |
| 3 | Int    | Weight   |
| 4 | Int    | Port     |
| 5 | String | Hostname |

### Notes

* `Service` and `Protocol` are two elements of the domain name, without
  the undescore '_'.
* `SrvObj` with Service `"smtp"`, Protocol `"tcp"` and Port `25` is also
  interpteted as an `MX` DNS respource.
* When lookup is performed for `SRV` records at fqdn
  `"_serv._proto.sub.dom.bit"`, domain object for `"sub.dom.bit"` must be
  fetched, and in this object, `SrvObj`s for the Service `"serv"` and
  Protocol `"proto"` selected from it.

## Data Interpretation

Assuming a query for a subdomain of a basedomain in the `.bit` TLD
(subdomain possibly being empty), lookup starts by fetching the
"base" object for basedomain. The domain object is then transformed
by the following sequece applied recursively:

1. Value of the element of the `"map"` attribute with the key `""`
   (empty string) is recursively merged into the base domain. The
   `""` element of the `"map"` is removed from the result.
2. If attribute `"import"` does not exist in the resulting object,
   recursion stops, and step 3 is performed on the result
   If attribute `"import"` exists in the resulting object, lookup is
   is performed for the value of this attribute, and fetched object
   is recursively merged into the base domain. The `"import"` attribute
   is removed from the result. Then the result is passed as base
   domain to step 1.
3. If subdomain chain is empty, recursion stops, and step 4 is
   performed on the result. If subdomain chain is not empty, next
   element is taken out of the chain, and the `"map"` is looked
   up for the element with the name matching the subdomain element.
   The value of this element of the `"map"` is passed as base domain
   to step 1. If matching element does not exist, lookup is considered
   failed.
4. Domain object with all `""` map elements and all `"import"` data
   merged is "normalized" by removal of attributes that are nullified
   by the presence of other attributes.

Note that the process involves recursion nested to three levels.

## Merging

When a domain object `sub` needs merging into a domain object `base`,
the following rules are applied:

* Of `String` and other "scalar" attributes, one is chosen, the value
  from the `base` taking precedence.
* On `Array` attribtes, `union` operation is performed. (Of equal
  elements, only one copy is left.)
* On `Map` attributes, recursive merge is performed. On the top level,
  elemens with keys that are only present in either `base` or `sub`
  object are all put into result. The values of the elements that are
  present in both `base` and `sub` are merged according to the rules
  applicable to their type.
