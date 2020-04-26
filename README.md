# glue

Simple low-overhead openGL wrapper/front-end

Unfinished, super experimental, unstable

## Todo
  - [ ] everything
  - [ ] texture handling
  - [ ] Think out monad transformer stack structure
      - [ ] prevent running without GL context
      - [ ] Prevent nested `ProgramT`s
  - [ ] Hide `ProgramT` constructor
  - [ ] Uniform arrays

## Potential names
  - glue
    - not available on hackage
  - ezgl/eagle
  - luster/shine/gleam/glint
      - references to gloss
      - fuck shine is taken
      - also gleam but who cares
  - gsdl
    - get shit done library
  - glass
    - transparent
    - looks like gloss
    - has ass in it

## Haskell graphics

### Libraries

#### Super high level

  - Gloss
  - https://github.com/schell/gelatin
    - https://github.com/schell/gristle
    - https://github.com/schell/ixshader
  - https://gitlab.com/sheaf/fir
  - GPipe

#### Super low level

  - gl
  - OpenGl
  - OpenGlRaw
  - https://hackage.haskell.org/package/vulkan-api
  - https://hackage.haskell.org/package/vulkan

### Other projects

  - https://github.com/ocharles/zero-to-quake-3
    - uses Vulkan

