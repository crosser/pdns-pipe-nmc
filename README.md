% Namecoin pipe backend for PowerDNS
# Namecoin pipe backend for PowerDNS

There exists a [project](https://github.com/namecoin/nmcontrol) to
create an all-in-one tool that can, among other things, act as a DNS
server for the `.bit` zone. There is also a
[tool](https://github.com/namecoin/NamecoinToBind) for offline
conversion of namecoin data into BIND zone file.

Unlike that, this project is a single-purpose tool acting as a (real
time) bridge between [Namecoin](https://namecoin.org/) and DNS.
[PowerDNS](https://www.powerdns.com/) provides a stable DNS frontend,
with an easy to implement backend interface, which is used in this
project.

## Status

Alpha. It does not handle `SRV` records at all, does not support
`delegate` (not to mention `import`), provides bogus version in the
`SOA` record, and is largely untested. Try at your risk.
