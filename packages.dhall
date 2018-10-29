{-
 Welcome to Spacchetti local packages!
You can edit this file as you like.

e.g.

```
in  let overrides =
        { halogen =
        upstream.halogen ⫽ { version = "master" }
        , halogen-vdom =
        upstream.halogen-vdom ⫽ { version = "v4.0.0" }
        }
```
-}

    let mkPackage =
          https://raw.githubusercontent.com/justinwoo/spacchetti/241018/src/mkPackage.dhall

in  let upstream =
          https://raw.githubusercontent.com/justinwoo/spacchetti/241018/src/packages.dhall

in  let overrides = {
	heterogeneous = upstream.heterogeneous ⫽ { version = "v0.3.0" }
}

in  let additions = {
	halogen-formless =
          mkPackage
          ["halogen","halogen-renderless","variant","heterogeneous"]
          "https://github.com/thomashoneyman/purescript-halogen-formless.git"
          "v0.4.0"	
      , halogen-renderless =
          mkPackage
          ["halogen","prelude","control"]
          "https://github.com/thomashoneyman/purescript-halogen-renderless.git"
          "v0.0.3"	
      , remote-data =
          mkPackage
	      ["bifunctors", "either", "profunctor-lenses", "generics-rep"]
          "https://github.com/krisajenkins/purescript-remotedata.git"
          "v4.0.0"
}

in  upstream ⫽ overrides ⫽ additions
