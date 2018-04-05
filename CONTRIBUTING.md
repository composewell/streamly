# Contributors' Guide

## Bug Reports

Please feel free to [open an
issue](https://github.com/composewell/streamly/issues) for any questions,
suggestions, issues or bugs in the library. In case you are reporting a bug
we will appreciate if you provide as much detail as possible to reproduce or
understand the problem, but nevertheless you are encouraged to open an issue
for any problem that you may encounter.

## Pull Requests (PR)

Please feel free to [send a pull
request (PR)](https://github.com/composewell/streamly/pulls) whether it is a single
letter typo fix in documentation or a more complex change. If the feature makes
significant changes to design, we encourage you to open an issue as early as
possible so that you do not have to redo much work because of changes in design
decisions. However, if you are confident, you can still go ahead and take that
risk.

### Picking Issues to Work on

Beginners are encouraged to pick up issues that are marked `help wanted`.

### Changeset in a PR

* Please make sure that a single PR contains a single logical changeset. That
  helps the reviewers in quickly understanding and reviewing the change.
* You are encouraged to group a logically related set of changes into a single
  commit.  When the overall changeset is largish you can divide it into
  multiple smaller commits, with each commit having a logically grouped
  changeset and the title summarizing the logical grouping.  Always keep in
  mind a logical division helps reviewers understand and review your code
  quickly, easier history tracking and when required clean reversal changes.
* If your commits reflect how you fixed intermediate problems during testing
  or made random changes at different times you may have to squash your changes
  (`git rebase -i`) into a single commit or logically related set of commits.
* Please resist the temptation to make style related changes to surrounding
  code unless you are changing that code anyway . Make sure that your
  IDE/Editor is not automatically making sweeping style changes to all the
  files you are editing. That makes separating the signal from the noise
  very difficult and makes everything harder. If you would like to make style
  related changes then please send a separate PR with just those changes and no
  other functionality changes.

### Resolving Conflicts

If during the course of development or before you send the PR you find that
your changes are conflicting with the master branch then use `git rebase
master` to rebase your changes on top of master. DO NOT MERGE MASTER INTO YOUR
BRANCH.

### Testing

It is a good idea to include tests for the changes where applicable.

### Documentation

For user visible APIs, it is a good idea to provide haddock documentation that
is easily understood by the end programmer and does not sound highfalutin,
and preferably with examples.

### Performance Benchmarks

It is a good idea to run performance benchmarks to see if your change affects
any of the existing performance benchmarks. If you introduced something new
then you may want to add benchmarks to check if it performs as well as expected
by the programmers to deem it usable.

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

## Coding

### Style

As long as possible please try to match the style of the file or the
surrounding code. For haskell coding style guidelines, please [see this style
guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).
Specifically,

* Please use 4 spaces for indentation.
* Do not let the code go beyond 80 columns

### Organization

Use this simple rule to organize functions in a file: `Define before first
use`. In other words, use the `bottom up style` to organize functions.

One big benefit of this rule is easier type error debugging. Using a single
block comment, at any point in the file up to the end of the file, you can cut
the tail and still cleanly compile the remaining head portion of the file.

It is very helpful when we make significant changes to the file. We can start
compiling a minimal head portion of the file and keep expanding the head by
just moving the start of commented block further down. This keeps the scope of
type inference minimal and type errors can be discovered incrementally by
increasing the scope a little bit at a time.  If you use type signatures on all
top level declarations then you do not even need to comment out the code.  This
organization results in a better order of type errors which makes it easier to
fix them and when necessary use elimination method by commenting out some code.

Similarly when you put modules in the cabal file in dependency order you can
do the same there, just comment out a tail of the module list and your library
will still compile.

Note that this rule does not apply in choosing `let` vs `where` clauses. Use of
`let`s  leads to bottom up and use of `where`s leads to top down style. But we
do not encourage `let` over `where`, you can freely use `where` clauses.

Also note that this is not possible in mutually recursive code. The mutually
recursive code has to be considered a single block.

