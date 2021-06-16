# Contributors' Guide

## Bug Reports

Please feel free to [open an
issue](https://github.com/composewell/streamly/issues) for any questions,
suggestions, issues or bugs in the library. In case you are reporting a bug
we will appreciate if you provide as much detail as possible to reproduce or
understand the problem, but nevertheless you are encouraged to open an issue
for any problem that you may encounter.

## Picking Issues to Work on

Beginners are encouraged to pick up issues that are marked `help wanted`. It is
a good idea to update the issue expressing your intent so that others do not
duplicate the effort and people with a background on the issue can help.

## Contributing A Change

If the feature makes significant changes to design, we encourage you to open an
issue as early as possible so that you do not have to redo much work because of
changes in design decisions. However, if you are confident, you can still go
ahead and take that risk as the maintainers are supposed to be reasonable
people.

### Pull Requests (PR)

Please feel free to [send a pull
request (PR)](https://github.com/composewell/streamly/pulls) whether it is a
single letter typo fix or a complex change.  We will accept any PR that makes a
net positive change to the package. We encourage you to provide a complete,
consistent change with test, documentation and benchmarks. You can contact the
[maintainers](https://gitter.im/composewell/streamly) for any help or
collaboration needed in that regard. However, if due to lack of time you are
not able to complete the PR, you are still welcome to submit it, maintainers
will try their best to actively contribute and pick up your change as long as
the change is approved.

### Pull Request (PR) Checklist

Here is a quick checklist for a PR, for details please see the next section:

* PR contains one logical changeset
* Each commit in the PR consists of a logical change
* Commits are rebased/squashed/fixup/reordered as needed
* Stylistic changes to irrelevant parts of the code are separated in
  independent commits.
* Code is formatted as per the style of the file or that of other files
* Compiler warnings are fixed
* Reasonable hlint suggestions are accepted
* Tests are added to cover the changed parts
* All [tests](test) pass
* [Performance benchmarks](benchmark) are added, where applicable
* No significant regressions are reported by [performance benchmarks](benchmark/README.md)
* Haddock documentation is added to user visible APIs and data types
* Tutorial module, [README](README.md), and [guides](docs) are updated if
  necessary.
* [Changelog](Changelog.md) is updated if needed
* The code conforms to the license, it is not stolen, credit is given,
  copyright notices are retained, original license is included if needed.

### Structuring Pull Requests

__Lean PR:__ Please keep the reviewer in mind when sending pull
requests. Use one PR for each logical changeset. A lean and thin
PR with one independent logical change is more likely to be reviewed and
merged quickly than a big monolithic PR with several unrelated changes.

__PR Dependencies:__ If your change depends on an earlier PR you can
create a branch from the old PR's branch, and raise a new PR targeting
to merge into the old PR branch. Do not push the commits to the same PR
just because the commit depends on that PR.

__Commits:__ You are encouraged to group a logically related set of
changes into a single commit.  When the overall changeset is largish you
can divide it into multiple smaller commits, with each commit having
a logically grouped changeset and the title summarizing the logical
grouping.  Always keep in mind a logical division helps reviewers
understand and review your code quickly, easier history tracking and
when required clean reversal changes.

__Functional Changes:__ Keep the reviewer in mind when making
changes.  Please resist the temptation to make style related changes to
surrounding code unless you are changing that code anyway . Whenever
possible, try to separate unrelated refactoring changes which do not
affect functionality in separate commits so that it is easier for the
reviewer to verify functional changes.

__Style Changes:__ Make sure that your IDE/Editor is not automatically
making sweeping style changes to all the files you are editing. That
makes separating the signal from the noise very difficult and makes
everything harder. If you would like to make style related changes
then please send a separate PR with just those changes and no other
functionality changes.

__Rebasing:__ If your commits reflect how you fixed intermediate
problems during testing or made random changes at different times you
may have to squash your changes (`git rebase -i`) into a single commit
or logically related set of commits.

### Resolving Conflicts

If during the course of development or before you send the PR you find that
your changes are conflicting with the master branch then use `git rebase
master` to rebase your changes on top of master. DO NOT MERGE MASTER INTO YOUR
BRANCH.

### Testing

It is a good idea to include tests for the changes where applicable. See the
existing tests [here](test).

### Documentation

For user visible APIs, it is a good idea to provide haddock documentation that
is easily understood by the end programmer and does not sound highfalutin,
and preferably with examples. If your change affects the tutorial or needs to
be mentioned in the tutorial then please update the tutorial. Check if the
additional [guides](docs) are affected or need to updated.

### Performance Benchmarks

It is a good idea to run performance benchmarks to see if your change affects
any of the existing performance benchmarks. If you introduced something new
then you may want to add benchmarks to check if it performs as well as expected
by the programmers to deem it usable.

See the [README](benchmark/README) file in the `benchmark` directory for more
details on how to run the benchmarks.

The first level of check is the regression in "allocations", this is
the most stable measure of regression. "bytesCopied" is another stable
measure, it gives an idea of the amount of long lived data being copied
across generations of GC. 

The next measure to look at is "cpuTime" this may have some variance
from run to run because of factors like cpu frequency scaling, load on
the system or the number of context switches etc. However, the variance
would usually be within 5-10%, anything more than that is likely to be a
red flag.

Quite often an increase in "cpuTime" would correspond to an increase
in "allocations". Increase in "allocations" indicates an inefficiency
in computing due to too much GC activity e.g. due to lack of
fusion. However, it is also possible for the cpu time to go up without
the allocations going up, this indicates an inefficiency in processing
in general or more CPU being used for the same task. It could be because
of inefficient code generation, branch mis-predictions or lack of cache
locality etc.

For concurrent benchmarks we can compare "cpuTime" and "time" to check
the degree of concurrency and total efficiency.

### Changelog

Any new changes that affect the user of the library in some way must be
documented under `Unreleased` section at the top of the `Changelog`.  The
changes in the changelog must be organized in the following categories, in that
order:

* Breaking Changes
* Enhancements
* Bug Fixes
* Deprecations

If there are very few changes then you can just prefix a bullet with these
annotations. If there are many changes make sections to group them. A section
can be absent if there is nothing to add in it.

If you make changes that are incompatible with the released versions
of the library please indicate that in the `Changelog` as `Breaking Changes`
and also write short notes regarding what the programmers need to do to adapt
their existing code to the new change.

### Licensing

If you have copied code from elsewhere you need to conform to the
licensing terms of the original code. Usually you would need to add a
copyright notice in the source header, and include the license of the
original code as per the terms of the license. If the code you are
copying does not have an associated license please do not use it.

## Developer documentation

To build haddock documentation:

```
$ cabal haddock
```

Open the link printed at the end of the output of this command, in
your browser.

For general library developer documentation see the `dev` directory.

## Coding Guidelines

### Coding Style

Please see [the Haskell coding style guide](https://github.com/composewell/haskell-dev/blob/master/coding-style.rst).

### StreamD coding style

Some conventions that we follow in the StreamD code are illustrated by the
following example:

```
mapM f (Stream step1 state1) = Stream step state
    where

    step gst st = do
        r <- step1 (adaptState gst) st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Skip s    -> return $ Skip s
            Stop      -> return Stop
```

* For the input streams use numbering for `step` and `state` e.g.
  step1/state1. For the output stream use `step` and `state`.
* For state argument of `step`, use `st`.
* For result of executing a `step` use `r` or `res`
* For the yielded element use `x`
* For the yielded state use `s`

In general, the rule is - the shorter the scope of a variable the shorter its
name can be. For example, `s` has the shortest scope in the above code, `st`
has a bigger scope and `state` has the biggest scope.

## Design guides

See [the design directory](design) for design guidelines and documents.

### Tricky Parts

The state-passing through each API is currently fragile. Every time we run a
stream we need to be careful about the state we are passing to it. In case of
folds where there is no incoming state, we start with the initial state
`defState`. When we have an incoming state passed to us there are two cases:

1. When we are building a concurrent stream that needs to share the same `SVar`
   we pass the incoming state as is.
2. In all other cases we must not share the SVar and every time we pass on the
   state to run a stream we must use `adaptState` to reset the `SVar` in the
   state.

When in doubt just use `adaptState` on the state before passing it on, we will at
most lose concurrency but the behavior will be correct.

There is no type level enforcement about this as of now, and therefore we need
to be careful when coding. There are specific tests to detect and report any
problems due to this, all transform operations must be added to those tests.
