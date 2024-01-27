# Correspondence of Operations

When determining the duality think about what the stream produces and what the
fold consumes. The out of the stream corresponds to the input of the fold.

We may not have exact duals but duals by nature or characteristics.

| Stream     | Fold          |
|------------|------------   |
| Stream m a | Fold m a b    |
| Zip*       | Tee           |

Connecting fold and stream:

| fold       | drive         |
| foldBreak  | addStream     |
|            | duplicate     |

Stream generation, fold accumulation:

| nil        | fromPure      | produce nil, consume nil
| nilM       | fromEffect    | produce effect, consume effect?
| cons       | snoc          |
|            | addOne        |
| consM      | snocM         |
| unfoldr    | foldl'        |
| unfoldrM   | foldlM'       |
| fromPure   | one           | produce one, consume one
| iterate    | foldl1'       | no termination, no initiation
| iterateM   | foldlM1'      |
| -          | foldr'        |
| repeat     | head (latest) |
| replicate  | the           |
| enumerate  | sum           |

Scanners are something that can be used on folds as well as streams to
transform the output or input stream. So it probably makes sense to have a
neutral type for this.

| findIndices  |
| elemIndices  |
| deleteBy     |
| uniqBy       |
| nub          |

Combinators:

| -            | rMapM          |
| fmap         | lmap           |
