Copy `dune-project` and adjust names as desired

`dune build` at top level (generates `{name}.opam`)

`opam install . --deps-only` at top level (installs `core` dependency)

Subsequently, on terminal start run `eval $(opam env)` to bring installs into scope

For each day:
- create folder `dayX`
- copy `dune` file with entrypoint
- make `{entrypoint}.ml` in `dayX` folder
- `dune build` in `dayX` folder
- start developing in `{entrypoint}.ml` (hints will work in VSCode if you've installed the OCaml extension)
- run program with `ocaml {entrypoint}.ml`
  - NOTE: If using `str` library, need to explicitly link it by using `ocaml str.cma {entrypoint}.ml`