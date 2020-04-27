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
  - [ ] Resource cleanup
      - [ ] Manual? Brackets? ResourceT?
  - [ ] GLStorable
    - [ ] Rename to vertex?
    - [ ] Change to just associate with the GL type enum?
      - [ ] Pull size from `Storable`?
      - [ ] Is always normalized anyway?
  - [ ] generate GLSL template from/for `Program`
  - [ ] Meshes
    - [ ] Lines / points
    - [ ] Strips / fans
  - [ ] App
    - [ ] client side MVP?
    - [ ] flush
      - [ ] also `finish` for benchmarks

## Design Goals
  - [ ] Make common things fast
  - [ ] Make idiomatic easy
  - [ ] Be transparent and flexible
    - [ ] Imperative, impure
    - [ ] Users write shaders(!)
      - [ ] Seriously GLSL is fine
      - [ ] When is writing shaders ever the hard part
    - [ ] Easy to reason through the gl calls
    - [ ] Don't get in the way of users who know better
    - [ ] Don't preclude any hand-optimization
    - [ ] Be porcelain but don't hide the plumbing
    - [ ] Ideally don't force an OpenGL version

### Maybe in the future who knows
  - [ ] Abstract out the underlying implementation
    - [ ] And then maybe use Vulkan/Metal
      - [ ] How different is Vulkan?
    - [ ] or mock it in some other monad

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

TODO: Write comparisons

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

