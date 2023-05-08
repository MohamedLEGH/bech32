bech32
===========

A package to encode and decode bech32 encoding, according to the bitcoin documentation https://en.bitcoin.it/wiki/Bech32.

# Installation
```bash
raco pkg install bech32
```

For more info see: https://pkgs.racket-lang.org/package/bech32

# Usage

```racket
> (require bech32)
> (bech32-encode "0049c3307695e88874509b77ff859ab10064d1cb704733eea7")
"17j29zgqUxMDTAgbBaZwEMFubHjnCrDpXp"
> (bech32-decode "17j29zgqUxMDTAgbBaZwEMFubHjnCrDpXp")
"0049c3307695e88874509b77ff859ab10064d1cb704733eea7"
```

# Documentation

https://docs.racket-lang.org/bech32/index.html