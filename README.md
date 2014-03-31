% Namecoin pipe backend for PowerDNS

```
Copyright (c) 2014 Eugene Crosser

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must
    not claim that you wrote the original software. If you use this
    software in a product, an acknowledgment in the product documentation
    would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must
    not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
```

------------------------------------------------------------------------

## Namecoin pipe backend for PowerDNS

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

# Status

Alpha. It does not handle `SRV` records at all, does not support
`delegate` (not to mention `import`), provides bogus version in the
`SOA` record, and is largely untested. Try at your risk.
