# rowan-numskull

## Development

### Using Cabal

This builds like any Cabal project with `cabal build`, `cabal repl`, etc.

### Using Nix (and Cabal)

There is a `default.nix` so the project can be built with `nix-build`, and a
`shell.nix` for a development shell with `nix-shell`.

#### Niv

Dependencies are maintained using `niv` in `nix/sources.json`.
Source repositories specified in `cabal.project` should be kept up to date with
`nix/sources.json`.

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
