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

If you make changes that are incompatible with the released versions of the
library please indicate that in the `Changelog` as `Breaking Changes` and what
the programmers need to do to adapt to the new change. Any new user visible
changes must be documented in the `Changelog`.

## Coding Style

As long as possible please try to match the style of the file or the
surrounding code.

* Please use 4 spaces for indentation.
* Do not let the code go beyond 79 columns
* Use multiline `let` clauses like this:
  ```
  let stop = stp
      yield a Nothing = ...
      ...
  ```
* Align the contents of the where clause in the same column as the `where`
  keyword for saving unnecessary indentation, like this:
  ```
      where

      x = ...
  ```
