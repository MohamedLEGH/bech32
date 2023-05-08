#lang scribble/manual
@require[@for-label[bech32
                    racket/base]]

@title{bech32}
@author[(author+email "Mohamed Amine LEGHERABA" "mohamed.amine.legheraba@gmail.com")]

@defmodule[bech32]

Encoding and decoding functions for the @hyperlink["https://en.bitcoin.it/wiki/Bech32"]{bech32} encoding, as defined by the @hyperlink["https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki"]{BIP-173} specification.
Also working with the bech23m encoding, according to the specification @hyperlink["https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki"]{BIP-350}.

@defproc[(bech32-encode [value string?]) string?]{
  Encodes @racketfont{value} to a bech32 string.
  @racketfont{value} should be a hexadecimal string.
}

@defproc[(bech32-decode [value string?]) string?]{
  Decodes bech32 string @racketfont{value} to a hexadecimal string.
}

@defproc[(bech32-verify [value string?]) string?]{
  Verify the checksum of the bech32 string @racketfont{value}.
}