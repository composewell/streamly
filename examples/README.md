# Running The Examples

## Running directly using stack
You can run these examples using `stack` like this:

```
$ stack build
$ stack AcidRain.hs
```

Note: This method may not work for `CirclingSquare.hs` SDL animation example.

## Build and run

Build the library with the `examples` flag on e.g.

```
stack build --flag streamly:examples
cabal new-build --flags examples
```

Then run the executables, for example:

```
stack exec AcidRain
```

The executable name are the same as the filenames.

## Running the SDL animation example

To include the SDL examples as well build with `examples-sdl` flag:

```
stack build --flag streamly:examples-sdl
cabal new-build --flags examples-sdl
```

Make sure that you have the SDL OS package installed on your system and the
headers are visible to Haskell build tool.

```
stack exec CirclingSquare
```
