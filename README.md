# ghc_hyperopt

Haskell programs have a variety of parameters that can be tuned to improve performance, from options provided by GHC to knobs exposed by the runtime system.

GHC HyperOpt is a tool for automatically tuning these parameters.

## TODO

- [ ] Builds should take place out-of-tree to enable parallel builds and avoid clobbering existing builds
- [ ] Benchmark binaries produced should be cached for re-use
  - Take the perspective of a set of fixed GHC options as an epoch, and a set of runtime system options as steps
    within that epoch
  - While binaries cannot be re-used across epochs, they can (and should!) be re-used across steps within an epoch
