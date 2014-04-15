% JSON Domain Format as Implemented by Pdns-Pipe-Nmc

## Data Format

### `DomObj` Object

`DomObj` either a `String` containing a dotted quad (see Note below),
or a JSON `Map`, with the following attributes, all optional:

| Key         | Type                                  | Comment                                    |
|-------------|---------------------------------------|--------------------------------------------|
| service     | Array(SrvObj)                         | Located two levels above pos.              |
| ip          | Array(String)                         | Dotted quad format "1.2.3.4"               |
| ip6         | Array(String)                         | Semicolon format "DEAD::BEEF"              |
| tor         | String                                | Onion name                                 |
| i2p         | I2pObj                                |                                            |
| freenet     | String                                |                                            |
| alias       | String                                | Nullifies other attributes                 |
| translate   | String                                | Nullifies other attributes                 |
| email       | String                                | Used in `SOA`                              |
| loc         | String                                | Format suitable for `LOC`                  |
| info        | JsonObj                               | Currently unspecified                      |
| ns          | Array(String)                         | Domain names as in `NS`                    |
| delegate    | String                                | Replaces current object                    |
| import      | String                                | "Deep" merges into current obj.            |
| map         | Map(String:DomObj)                    | Tree of subdomain objects                  |
| fingerprint | Array(String)                         |                                            |
| tls         | Map(String:Map(String:Array(TlsObj))) | Outer `Map` by `Protocol`, inner by `Port` |
| ds          | Array(DsObj)                          |                                            |

#### Notes:

* Any attribute specified as `Array(String)` may be present in the
  JSON document as a plain `String`, which is interpreted the same way
  as an `Array` containing a single `String` element.
* If `DomObj` is a `String`, it is interpreted as an IPv4 address.
  In other words, string `"1.2.3.4"` is the same as the Map
  `"{\"ip\":\"1.2.3.4\"}"`. Such "shorthand" DomObj can be present at
  the top level or as a value in the `"map"` attribute.

### `SrvObj` Object

`SrvObj` is a heterogenous Array of fixed size containing 6 elements:

| N | Type   | Meaning  |
|---|--------|----------|
| 0 | String | Service  |
| 1 | String | Protocol |
| 2 | Int    | Priority |
| 3 | Int    | Weight   |
| 4 | Int    | Port     |
| 5 | String | Hostname |

#### Notes

* `Service` and `Protocol` are two elements of the domain name, without
  the undescore '_'.
* `SrvObj` with Service `"smtp"`, Protocol `"tcp"` and Port `25` is also
  interpteted as an `MX` DNS resource.
* When lookup is performed for `SRV` records at fqdn
  `"_serv._proto.sub.dom.bit"`, domain object for `"sub.dom.bit"` must be
  fetched, and in this object, `SrvObj`s for the Service `"serv"` and
  Protocol `"proto"` selected from its `"service"` attribute.

### `TlsObj` Object

`TlsObj` is a heterogenous Array of fixed size containing 3 elements:

| N | Type   | Meaning                                               |
|---|--------|-------------------------------------------------------|
| 0 | Int    | Match type - 0:Exact, 1:SHA-256, 2:SHA-512            |
| 1 | String | Match value - certificate or hash of it as hex string |
| 2 | Int    | Include subdomains - 0:No, 1:Yes                      |

### `DsObj` Object

`DsObj` is a heterogenous Array of fixed size containing 4 elements:

| N | Type   | Meaning                  |
|---|--------|--------------------------|
| 0 | Int    | Key Tag                  |
| 1 | Int    | Key Algorithm            |
| 2 | Int    | Hash Type                |
| 3 | String | Hash Value as hex string |

## Data Interpretation

Assuming a query is performed for
`sdN`++"."++{...}++"."++`sd2`++"."++`sd1`++"."++`dom`++".bit"
(`sdX` list possibly being empty), the lookup process starts by
populating a "seed" DomObj with a single attribute `"import"`
the value of which corresponds to the `dom` name in the
Namecoin namespace, currently `"d/" ++ dom`.
This domain object is then transformed by the following
recursive sequece:

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

