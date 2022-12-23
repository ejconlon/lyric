# lyric

WIP - An abstract machine semantics for the Verse language

The paper presents a rewrite semantics for the language, but I find that a bit unsatisfying. It seems silly to flatten all alternatives rather than just picking them in time.

Currently this implements enough to do some function application and backtracking. The basic structure _should_ be right, but I haven't tested the unification part (just followed what I usually use).

## References

* [paper](https://simon.peytonjones.org/assets/pdfs/verse-conf.pdf)
* [presentation](https://simon.peytonjones.org/assets/pdfs/haskell-exchange-22.pdf)
