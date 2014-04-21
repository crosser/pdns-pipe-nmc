% JSON Domain Format as Implemented by Pdns-Pipe-Nmc

This document is based on
[Namecoin Domain Name Specification](https://wiki.namecoin.info/index.php?title=Domain_Name_Specification).
It tries to follow it closely, and clarify parts that are not
specified, or specified ambiguously, in the original document.

One notable deviation is the specification of `"delegate"` and `"import"`
attributes: domain objects to which their value point are
replacing/merging to the domain object in which they are defined.
This seems to be in line with at least one existing "real world"
implementation.

This specification is implemented by the `pdns-pipe-nmc` program.

## Data Format

### `DomObj` Object

`DomObj` either a `String` containing a dotted quad (see Note below),
or a JSON `Map`, with the following attributes, all optional:

| Key         | Type                                  | Comment                                    |
|-------------|---------------------------------------|--------------------------------------------|
| service     | Array(SrvObj)                         | Located two levels above position          |
| ip          | Array(String)                         | Dotted quad format "1.2.3.4"               |
| ip6         | Array(String)                         | Semicolon format "DEAD::BEEF"              |
| tor         | String                                | Onion name                                 |
| i2p         | I2pObj                                |                                            |
| freenet     | String                                |                                            |
| alias       | String                                | Nullifies other attributes                 |
| translate   | String                                | Nullifies the "map" attribute              |
| email       | String                                | Used in `SOA`                              |
| loc         | String                                | Format suitable for `LOC`                  |
| info        | JsonObj                               | Currently unspecified                      |
| ns          | Array(String)                         | Domain names as in `NS`                    |
| delegate    | String                                | Replaces current object                    |
| import      | Array(String)                         | "Deep" merges into current object          |
| map         | Map(String:DomObj)                    | Tree of subdomain objects                  |
| fingerprint | Array(String)                         |                                            |
| tls         | Map(String:Map(String:Array(TlsObj))) | Outer `Map` by `Protocol`, inner by `Port` |
| ds          | Array(DsObj)                          |                                            |

#### Notes:

* Any attribute specified as `Array(String)` may be present in the
  JSON document as a plain `String`, which is interpreted the same way
  as an `Array` containing a single `String` element. In other words,
  `"ip":"1.2.3.4"` is equivalent to `"ip":["1.2.3.4"]`. (This does not
  apply to non-string-array attributes, like "service" or "ds".)
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
| 5 | String | FQDN     |

#### Notes

* `Service` and `Protocol` are two elements of the domain name, without
  the undescore '_'.
* `SrvObj` with Service `"smtp"`, Protocol `"tcp"` and Port `25` is also
  interpteted as an `MX` DNS resource at the domain level containing the
  `"service"` object.
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

#### Notes

* The fields of the object correspond to the attributes defined by
  [RFC-6698](http://tools.ietf.org/html/rfc6698) ("DANE").

### `DsObj` Object

`DsObj` is a heterogenous Array of fixed size containing 4 elements:

| N | Type   | Meaning                  |
|---|--------|--------------------------|
| 0 | Int    | Key Tag                  |
| 1 | Int    | Key Algorithm            |
| 2 | Int    | Hash Type                |
| 3 | String | Hash Value as hex string |

#### Notes

* The fields of the object correspond to the attributes defined by
  [RFC-3658](http://tools.ietf.org/html/rfc3658).

## Data Interpretation

### Semantics of the Attributes

#### service attribute

Translates to DNS `SRV` RR, only it is located in the subdomain tree
two levels higher than the `SRV` record would. For example, a
`"service"` attribute in the `"map"` hieararchy at the point
corresponding to the FQDN "sub.dom.bit" with the value

```
"service": [ ["imap", "tcp", 0, 0, 143, "mail.host.com" ],
             ["smtp", "tcp", 0, 0,  25, "relay.host.com"] ]
```

corresponds to two `SRV` RRs at two different points in the
subdomain tree:

```
_imap._tcp.sub.dom.bit. IN SRV 0 0 143 mail.host.com.
_smtp._tcp.sub.dom.bit. IN SRV 0 0  25 relay.host.com.
```

In addition to these, an `MX` RR is syntesized at the "sub.dom.bit"
level:

```
sub.dom.bit. IN MX 0 relay.host.com.
```

Note: Hostname element **must** be specified as fully qualified domain
name of the host, and **must not** terminate with a dot. 
This requirement seems to be in line with many existing definitions in
the blockchain; however it deviates from the BIND zone file format, in
which names that have not terminating dot are automatically expanded
by attaching the current origin zone to the end of the name.

#### ip attribute

Contains a list of strings representing IPv4 addresses in dotted
quad notation. For example,

```
"ip": ["1.2.3.4", "5.6.7.8"]
```

translates into a series of `A` RRs:

```
        IN A 1.2.3.4
        IN A 5.6.7.8
```

#### ip6 attribute

Contains a list of strings representing IPv6 addresses in semicolon
quads notation. For example,

```
"ip6": ["2001:4860:0:1001::68"]
```

translates into one AAAA RR:

```
        IN AAAA 2001:4860:0:1001::68
```

#### tor attribute

Does not translate into any DNS RR. Contains Tor hidden service address.

#### i2p attribute

Does not translate into any DNS RR. Contains an object with three
optional String attributes: `"destination"`, `"name"` and `"b32"`.

#### freenet attribute

Does not translate into any DNS RR. Contains Freesite key.

#### alias attribute

Translates into `CNAME` RR. Invalidates all other attributes except
the element of the `"map"` with empty key. Such element is analysed
and its contents merged into the base domain before the check.

Note: Hostname element **must** be specified as fully qualified domain
name of the host, and **must not** terminate with a dot. 

#### translate attribute

Translates into `DNAME` RR. Invalidates the contents of the `"map"`
attribute, except the element of the `"map"` with empty key. Such
element is analysed and its contents merged into the base domain
before the check.

#### email attribute

Translates into the `email` element of the SOA and RP RRs. The
value `"email":"user@domain.tld"` becomes `user.domain.tld.`
in the DNS record.

#### loc attribute

Translates into `LOC` RR. Value must conform to the format defined
by [RFC-1876](http://tools.ietf.org/html/rfc1876).

#### info attribute

Does not translate into any DNS RR. Contains a JSON object with
format unspecified at the time of this writing.

#### ns attribute

Translates into `NS` RR. Invalidates all other attributes, except
the element of the `"map"` with empty key. Such element is analysed
and its contents merged into the base domain before the check.

Note: the value of the attribute  **must** be specified as fully
qualified domain name of the host, and **must not** terminate
with a dot.

#### delegate attribute

Does not translate into any DNS RR. Instead, the value is used as
a key for namecoin lookup (i.e. the value must be specified with
the namespace prefix), and the result of the lookup replaces all
other attributes, except the element of the `"map"` with empty key.
Such element is analysed and its contents merged into the base
domain before the check.

#### import attribute

Does not translate into any DNS RR. Instead, the value is used as
a key for namecoin lookup (i.e. the value must be specified with
the namespace prefix), and the result of the lookup is merged with
the current domain object.

#### map attribute

JSON Map object containing subdomain names as its keys and domain
objects as values. Element of the map with empty key "" has special
meaning: the value of this map element is merged into the current
domain object. This operaton happens first when a new domain object
is analyzed, and is performed recursively. In the result of the
merge, the `"map"` does not contain the element with empty key.
Further operatons that can potentially modify the contents of the
current domain object (`import` and `delegate` lookups) start when
the empty element of the `"map"` has been recursively merged into
the current object.

#### fingerprint attribute

Does not translate into any DNS RR. Contains a list of TLS
certificate fingerprints. Deprecated.

#### tls attribute

Intended to carry attributes as per
[RFC-6698](http://tools.ietf.org/html/rfc6698) ("DANE").
As of this writing, the specification is under discussion.

#### ds attribute

Translates into `DS` RR. Carries attributes defined by
[RFC-4034](http://tools.ietf.org/html/rfc4034).

### Lookup Sequence

Assuming a query is performed for
`sdN`++"."++{...}++"."++`sd2`++"."++`sd1`++"."++`dom`++".bit"
(`sdX` list possibly being empty), the lookup process starts by
querying the database for the object corresponding to `dom`.
Technically, it is easiest to populate a "seed" DomObj with a
single attribute `"import"` the value of which corresponds to the
`dom` name in the Namecoin namespace, which is `"d/" ++ dom`.
This domain object is then transformed by the following
recursive sequece:

1. Value of the element of the `"map"` attribute with the key `""`
   (empty string) is recursively merged into the base domain. The
   `""` element of the `"map"` is removed from the result.
2. If attribute `"delegate"` does not exist in the resulting object,
   step 3 is is performed. If attribute `"delegate"` exists, in
   the resulting object, lookup is performed for the values of this
   attribute, and fetched object replaces the base domain completely.
   The result is passed as base domain to step 1.
3. If attribute `"import"` does not exist in the resulting object,
   recursion stops, and step 4 is performed on the result
   If attribute `"import"` exists in the resulting object, lookup is
   performed for the values of this attribute, and fetched objects
   are recursively merged into the base domain. The `"import"`
   attribute is removed from the result. Then the result is passed
   as base domain to step 1.
4. If subdomain chain is empty, recursion stops, and step 5 is
   performed on the result. If subdomain chain is not empty, next
   element is taken out of the chain, and the `"map"` is looked
   up for the element with the name matching the subdomain element.
   The value of this element of the `"map"` is passed as base domain
   to step 1. If matching element does not exist, lookup is considered
   failed.
5. Domain object in which all `""` map elements and all `"delegate"`
   and `"import"` elements are acted upon and removed, is then
   "normalized" by removal of attributes that are nullified by the
   presence of other attributes.

Note that the process involves recursion nested to three levels.

### Merging Procedure

When a domain object `extra` needs merging into a domain object `base`,
the following rules are applied:

* Of `String` and other "scalar" attributes, one is chosen, the value
  from the `base` taking precedence.
* On `Array` attribtes, `union` operation is performed. (Of equal
  elements, only one copy is left.)
* On `Map` attributes, recursive merge is performed. On the top level,
  elemens with keys that are only present in either `base` or `extra`
  object are all put into result. The values of the elements that are
  present in both `base` and `extra` objects are merged according to
  the rules applicable to their type.

