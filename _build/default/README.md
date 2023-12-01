Copy `dune-project` and adjust names as desired

`dune build` at top level (generates `{name}.opam`)

`opam install . --deps-only` at top level (installs `core` dependency)

For each day:
- create folder `dayX`
- copy `dune` file with entrypoint
- make `{entrypoint}.ml` in `dayX` folder
- `dune build` in `dayX` folder
- start developing in `{entrypoint}.ml` (hints will work in VSCode if you've installed the OCaml extension)