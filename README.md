% Namecoin pipe backend for PowerDNS

There exists a project named
[nmcontrol](https://github.com/namecoin/nmcontrol) to create an
all-in-one tool that can, among other things, act as a DNS server
for the `.bit` zone. There is also a tool,
[NamecoinToBind](https://github.com/namecoin/NamecoinToBind),
for offline conversion of namecoin data into BIND zone file.

Unlike those, this project is a single-purpose tool acting as a (real
time) bridge between [Namecoin](http://namecoin.info/) and DNS.
It is implemented as a `pipe backend` to
[PowerDNS](https://www.powerdns.com/), which provides stable DNS
frontend, and has simple backend interface.

## Building

The program is built as a single executable to be run by PowerDns's
pipe backend. It is written in [Haskell](http://www.haskell.org/).
There is no `cabal` configuration at the moment, so to build it,
simply run

```
ghc --make pdns-pipe-nmc
```

and install any missing packages it complains about.

## Installing

In the powerdns configuration, you want to specify `master=yes`.
Enable `pipe` backend by setting `launch=pipe`.
Wherever your pdns package keeps the backend configurations, set
this for the pipe backend:

```
pipe-command=/path/to/pdns-pipe-nmc
pipe-timeout=10000
pipe-regex=.bit;.*$
pipebackend-abi-version=1 ## all versions supported, but extra data ignored
```

Copy `pdns-pipe-nmc` to the place that you've set up as `pipe-command`.
Copy your namecoin cofig file to `/etc/namecoin.conf` and make sure it
is readable by the userid specified in the powerdns config. Entries
recognized in the `/etc/namecoin.conf` file (with default values) are:

```
rpcuser=
rpcpassword=
rpchost=localhost
rpcport=8336
```

They are the parameters needed to contact the `namecoind` server over
its JsonRPC interface. With default installation on `localhost`, you
will only need to specify `rpcpassword`.

Configure your resolvers to use the PowerDns instance for queries in
the `.bit` zone. This is left as an exercise to the reader.

## Security Considerations

Namecoin per se has excellent non-repudiation characteristics. But
once you've converted the data into (non-DNSSEC-protected) DNS
format, all bets are off. If you intend to query your powerdns
instance over public Internet, remember that nothing prevents evil
hackers or ruthless governments from tampering with your queries
and powerdns responses. There are two possible approaches to
mitigation of this problem:

* Run namecoind and powerdns as close to the consumer as
possible: on the same host, or at least on the same network, and
keep it guarded.
* I did not try it, but it should be possible to use PowerDNS
[Front-signing](http://doc.powerdns.com/html/dnssec-modes.html#dnssec-frontserver),
so the communication will happen over DNSSEC protocol without the
need to keep the signatures in the zone data itself. You probably
would need to create signing key for the PowerDNS instance, and add
the corresponding public key as "trusted" into the configuration of
your resolvers.

## Status

Beta. It is mostly feature-complete, but insufficiently tested.
It implements the
[data format specification](http://www.average.org/pdns-pipe-nmc/spec.html)
(SPEC.md in the source distribution) that slightly deviates from the
[official specification](https://wiki.namecoin.info/index.php?title=Domain_Name_Specification).
I am using it to access some of the `.bit` websites and it works
for me.

Try at your risk.

## Unsolved problems

The biggest problem by far is generating meaningful `SOA` records.

### SOA Version a.k.a. Generation Count

DNS infrastructure (including PowerDNS implementation) relies on the
"generation" field of the `SOA` RR when it makes decision to invalidate
the cache. So, if there is zone data in the DNS cache, and a DNS server
needs to respond to a request about an object from that zone, it first
checks if the TTL has expired. If it has not, the server takes the data
from the cache. If it has expired, the server asks the "authoritative
source" (which is in our case the dnamecoin daemon) for the SOA record
and compares the generation count in the received response with the
number kept in the cache. If the "authoritative" SOA does not have a
greater generation count than the cached SOA, DNS server **does not**
refresh its cache, presuming that the data there is still valid.

So, it is important that the generation count in the SOA record is
incremented every time when the domain object, or any of the object that
it "include"-s or to which it "delegate"-s is changed.

At present, there is no machanism for that. In most cases, simply
summing the number of entries in `name_history`-s of all domain object
involved in resolution would work, but this approach would produce
wrong result when an "import" entry is removed from a domain, because
in such case the sum would decrease. It would also not notice the
changes in an object "include"-ed in a subdomain, unless complete
recursive resolution of the subdomain tree is enforced for when
SOA record is requested. That would invalidate the reason to have
caching in the first place.

One possible workaround, currently implemented in `pdns-pipe-nmc`, is to
use a derivative of absolute time, in our case the number of 10-munute
intervals elapsed since Namecoin was concieved, as the SOA generation
count.

### Nameserver field

There is no "reasonable" value that could be placed there. Except
possibly the name of the host on which the PoweDNS instance is running,
in the `.bit` zone. Currently, `pdns-pipe-nmc` just puts a dot "."
there, and no problems where noticed so far.

## Getting the Software

Check the [project homepage](http://www.average.org/pdns-pipe-nmc/).

### Source

Git [clone](git://git.average.org/git/pdns-pipe-nmc.git) or
[browse](http://www.average.org/gitweb/?p=pdns-pipe-nmc.git;a=summary),
or use [github mirror](https://github.com/crosser/pdns-pipe-nmc).

### Binary Executable

There is a binary built for x86_64 Linux with glibc6:

| Executable file                                                                                                      | PGP                                                                 |
|----------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|
| [**pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-22.git-108b6c2**](http://www.average.org/pdns-pipe-nmc/pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-22.git-108b6c2) | [sig](http://www.average.org/pdns-pipe-nmc/pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-22.git-108b6c2.sig) |
| [pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-20.git-e9bd43f](http://www.average.org/pdns-pipe-nmc/pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-20.git-e9bd43f) | [sig](http://www.average.org/pdns-pipe-nmc/pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-20.git-e9bd43f.sig) |

## Author

Eugene Crosser \<crosser at average dot org\>    
<http://www.average.org/~crosser/>
