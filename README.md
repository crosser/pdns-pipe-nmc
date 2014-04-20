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
the corresponding public key as "trused" into the configuration of
your resolvers.

## Status

Alpha. It is insufficiently tested, and there are loose ends in the
functionality. For example, version in the `SOA` record is bogus.
Some of the the problems are due to incomplete and/or imprecise
[definition of the domain data format](https://wiki.namecoin.info/index.php?title=Domain_Name_Specification)
on the wiki. That said, I am using it to access some of the `.bit` websites
and did not notice anomalies so far.

Try at your risk.

## Getting the Software

### Source

Check the [project homepage](http://www.average.org/pdns-pipe-nmc/).

Git [clone](git://git.average.org/git/pdns-pipe-nmc.git) or
[browse](http://www.average.org/gitweb/?p=pdns-pipe-nmc.git;a=summary),
or use [github mirror](https://github.com/crosser/pdns-pipe-nmc).

### Binary Executable

There is a binary built for x86_64 Linux with glibc6:

|----------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|
| Executable file                                                                                                      | PGP                                                                 |
| [pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-20.git-e9bd43f](pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-20.git-e9bd43f) | [sig](pdns-pipe-nmc.linux-glibc6.x86_64.2014-04-20.git-e9bd43f.sig) |
|----------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|

## Author

Eugene Crosser \<crosser at average dot org\>    
<http://www.average.org/~crosser/>
