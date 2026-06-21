cons and snoc are in fact both folding operations. One is a natural
right fold constructions step (for building streams) and the other is a
natural left fold construction step.

cons        ?

If we can represent a Fold as a chain of singleton consumers which keep
passing the accumulator to the next, then we build a Fold as a chain of
continuations just like we build a stream as a linked list of elements
using cons. In that case adding one more continuation would represent
the dual of stream's "cons".

Is this similar to the snocList? so the operation should be snoc? Just
like StreamK provides a "cons" operation but Stream does not provide an
efficient cons; similarly, FoldK will provide this operation but Fold
does not provide an efficient, though similar to Stream we can always
append a Step to the Fold but it will not fuse. Note FoldK already exists as
ParserK.

So the full picture lines up as: cons-list ⟷ snoc-list, head-cheap
⟷ tail-cheap, right-fold/Stream ⟷ left-fold/Fold, StreamK.cons ⟷
FoldK.snoc. (If streamly's Fold were a right fold instead, the cheap end
would flip and it'd genuinely be cons — the snoc falls directly out of
its being a left fold.)

# Builder

FoldK is the incrementally constructible left fold.

FoldK is like the DList and the natural operation for this is "snoc". It is
essentially a left associative stream.

It is a builder. Can we build the Poke monad using this?

See also Streamly.Internal.Data.Builder.hs
