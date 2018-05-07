scripts
===

Racket code:

- `count-chaperones.rkt` : collects low-level performance details
- `make-configurations.rkt` : creates the `2**N` configurations for a benchmark
- `modulegraph.rkt` : builds module-dependence graphs
- `type-info.rkt` : collects type annotations
- `copy-configuration.rkt` : creates one configuration (not user-friendly)

Other:

- `count-chaperones.patch` patch that adds C-level variables to count chaperones.
  This patch probably will not apply automatically. Good luck.
  Originally from <https://www.cs.umd.edu/~sstrickl/chaperones/index.html>
