let masonDev = https://raw.githubusercontent.com/ursi/purescript-package-set/7c46d72464a09b9aabfc2fe48a3699bd30db768b/packages.dhall sha256:828e05fe310b76397ce641d7ad8ffd6cacb4672f9f365432f4d0e6ab464ed041
let upstream = https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115
in (masonDev // upstream)
  // { elmish      = masonDev.elmish      // { version = "b1768b5d359ed3a8d6d1771c937ee0b7ddbd0da6" } }
  // { ffi-options = masonDev.ffi-options // { version = "2cada376e6facb9338a1da5452e927b1aa51fe87" } }
  // { whatwg-html = masonDev.whatwg-html // { version = "35757998138b7070c75880c72ca5322fb41cec1e" } }
