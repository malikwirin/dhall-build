-- maybe use example with not fully evaluated env args but this currently points to a bug
derivation
  { name = "hello-script"
  , system = < builtin | x86_64-linux >.x86_64-linux
  , builder = (< Builtin : < `Fetch-Url` : {} > | Exe : Text >).Exe "/bin/sh"
  , args =
      [ "-c"
      , "mkdir -p $out/bin && printf '#!/bin/sh\\necho Hello World\\n' > $out/bin/hello && chmod +x $out/bin/hello"
      ]
  , environment =
      [ { name = "coreutils"
        , value = (< Bool : Bool | Text : Text >).Text
            "/nix/store/wkkwxc04gdw6b263l1h29pjarjnjdyb6-coreutils-9.8"
        }
      ]
  , outputs = [ "out" ]
  , `output-hash` =
      None { algorithm : < SHA256 >, hash : Text, mode : < Flat | Recursive > }
  }

