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
> (bech32-encode "751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6")
"bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y"
> (bech32-decode "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y")
"751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6"
> (bech32-verify "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y")
#t
```

# Documentation

https://docs.racket-lang.org/bech32/index.html