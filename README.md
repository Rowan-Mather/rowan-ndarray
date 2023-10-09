# rowan-numskull

## Using Numskull

This is a Summer Internship project from 2023. Numskull is a NumPy-like library for Haskell, featuring NdArrays which can be created and manipulated to store many different types (of the DType class). 

Numskull was designed for purposes of integration into an [Onnx](https://onnx.ai/) backend, but it can be used anywhere you need to operate on arrays of unspecified type and shape.

For more information, have a look at my talk: [slides](demo/presentation-slides.pdf).

To run the demo you need 
1) jupyter
2) iHaskell (https://github.com/IHaskell/IHaskell) to put Numskull 
code into a jupyter notebook. 
3) nix-shell
4) cd demo/notebook/
5) ./start.sh

Note that the work in main is Numskull 1.0. 
Numskull 2.0 can be found in the so-called branch! The second version is less well tested and complete, but should be more efficient since it makes use of strides. I didn't have time to integrate that into the Onnx backend, but it shouldn't be at all difficult to do so. There is an open pull request so it's easy to find.

## Development

### Using Cabal

This builds like any Cabal project with `cabal build`, `cabal repl`, etc.

#### Benchmark

Run the benchmark with

```sh
$ cabal run bench
```

### Using Nix (and Cabal)

There is a `default.nix` so the project can be built with `nix-build`, and a
`shell.nix` for a development shell with `nix-shell`.

#### Niv

Dependencies are maintained using `niv` in `nix/sources.json`.
If any source repositories are specified in `cabal.project`, they should be
kept up to date with the sources specified in `nix/sources.json`.

#### cabal2nix

`numskull.nix` should be updated with

```sh
$ cabal2nix . > numskull.nix
```

whenever the Cabal file is updated with e.g. new dependencies.

#### Nix shell

Within a Nix shell, ideally you build the project with
`cabal build --project-file=cabal-nix.project` to avoid fetching and building
dependencies specifid in `cabal.project` which are only intended for the
non-Nix build.
