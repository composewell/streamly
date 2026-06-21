# Nesting Monad in Fold

The stream's nesting monad does not become a monad in folds. On the fold it
splits into (a) the contravariant Divisible/Decidable fan-out on the input
(tee/demux/partition, with Fold.many as the operational nesting), and (b) the
comonad / scan (Moore, Scanl) on the output.

# Scanl Comonad

The structural dual of stream concatMap (nest production) is the fold's
scan (nest consumption-with-history). A streamly Scanl is a Moore
machine, and the pure Moore machine is a literal Comonad. The pure Moore
machine / Scanl Identity is a real Comonad; the effectful Fold m a b has
extract :: s -> m b, so it's a comonad only relative to m (coKleisli),
not a literal Comonad instance -- and the Done/early-stop state breaks
the classical non-terminating Moore picture, which is why scans (Scanl)
and folds carry the truncated version of comonad.

Categorically this is the free-monoid-monad <=> cofree-comonoid-comonad
duality: `[a]` (the stream functor) is the free monoid and carries the
list/nesting monad; the Moore/scan object is the cofree comonadic dual
and carries extract/extend.

# No Contravariant Monad

Applicative has contravariant counterpart but Monad does
not. Applicative and Monad live in two different monoidal structures,
and the arrow-reversal that builds the contravariant hierarchy preserves
one and destroys the other.  Monad's dual is the Comonad -- but you
reach it by a different/lateral arrow-reversal than the one that turns
Functor into Contravariant.

# Why no contravariant Monad?

What is the basic intuition behind not having a contravariant monad?

## Reflection

A *single* function's two ends mirror perfectly -- that's exactly
what reversing the arrow does, and it's why the producer/consumer
flip works so cleanly: a producer of `a` and a consumer of `a` are
honest mirror images (`Functor` <=> `Contravariant`, `Applicative`
<=> `Divisible`). So argument and result each *do* have a fine mirror
concept on their own. The thing with no mirror isn't either end of
a function -- it's the act of **connecting one function's result to
another function's argument**.

We should not conflate reversing the arrows with function invertibility.
Invertibiltiy is a far stronger notion than duality. Ordinary duality
deliberately asks for less: not "can I undo this arrow," only "what's
the structure when arrows are typed the other way," which any category
answers for free.

## Pairing and Plugging

There are two ways to combine computations, and only one of them survives
reflection.

**Pairing** (Applicative): set two computations side by side and bundle them --
`(,) <$> x <*> y`. Nothing passes between them; `x` and `y` are independent,
and you only combine their results at the very end. Merging two things into a
pair and splitting a pair back apart are mirror operations, so this whole mode
reflects cleanly: reversing variance just turns "merge two results" into "split
one argument into two," which is `divide`. Symmetric.

**Plugging** (Monad): take the *result* of one computation and feed it into the
*next* -- `x >>= \a -> k a`. The defining move is that `a`, the value `x`
produces, *determines what the next computation even is*. That's a directed
junction -- output → next input — and it's directional in the strong, causal
sense: you have to run `x` first, because `k` doesn't exist until you're
holding `a`.

## Reflecting Causality

Now ask what the mirror of *that* would be. Reflecting "this result selects the
next computation" gives "this argument selects the *previous* computation" -- a
value flowing backward into a step that already ran. That isn't a
different-but-valid structure; it's causality in reverse, and no ordinary type
does it. *That's* the missing dual. Not result lacking a mirror, not argument
lacking a mirror -- the **wire from result to next-argument** points one way
through time, and its reflection would point the wrong way. (Laziness lets you
*fake* backward flow — reverse-state, the `Tardis` monad — but those are still
clever *covariant* monads exploiting a fixpoint, not a contravariant `join`;
the genuine reflection still isn't a thing.)

This is just the intuitive face of the "two layers" from
before. Applicative lives entirely in side-by-side pairing of
values (`×`), which is reflection-symmetric. Monad lives in
feeding-output-into-input -- composing `F` with itself -- **which is the
one operation where the arrow's direction is significant**.

## Comonad: Dual with forward causality

And it explains why the dual that *does* exist isn't a backward-monad but a
**comonad** (the scan/fold from earlier). Rather than reverse time -- "result
picks the next step" => "argument picks the previous step," impossible -- the
comonad keeps time pointing forward and instead flips *which side the
dependency sits on*: each value depends on the **context accumulated before
it**. A monad threads "this output becomes the next input"; a scan threads "the
whole history so far determines this reading." Same direction of time,
dependency moved from focus->future to past->focus. That's the legitimate mirror
-- and exactly why the stream's nesting monad reappears on the fold as a scan
rather than as some impossible reverse-running monad.

## Causality cannot be reversed

In summary, combining in parallel is symmetric; chaining in sequence is
not -- because chaining means "the result of now decides what happens
next," and the mirror of that is "decides what happened before," which
time won't allow.
