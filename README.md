# lyric

WIP - An abstract machine for the Verse language

The paper presents a rewrite semantics for the language, which "lays out choices in space," but this semantics goes back to laying them out in time. In particular, the machine uses a two-layer stack where the inner layer is a stack of reduction continuations (e.g. save an evaluated application head while you evaluate the argument), and the outer layer is a stack of control continuations (e.g. barriers for one/all). The machine maintains two pointers, one to the top inner frame for reduction, and one to the top outer frame to cons alternatives for resumption (depth-first search). The machine makes small steps by looking at the current focus operation (reduce, return, control, halt) then performing atomic work updating the state and focus.

Currently this implements enough to do some function application and backtracking. The basic structure _should_ be right, but there are some pieces missing as I'm assembling it. In particular, I need to test the unification part - for now I just copied things I ususally use and stubbed it out. Also there's some work to be done maintaining the environment correctly with returned closures.

## References

* [paper](https://simon.peytonjones.org/assets/pdfs/verse-conf.pdf)
* [presentation](https://simon.peytonjones.org/assets/pdfs/haskell-exchange-22.pdf)
