# Maintainers' Guide

## PR Merge Checklist

* All the CI tests pass
* New tests are added where applicable
* Benchmarks are added where applicable
* Run benchmarks locally if you suspect any regressions
* Documentation is added for any new combinators
  * New combinators have time and space complexity annotations
  * New combinators have `since` annotation
* Changelog entry is added for exposed combinators.
* Identify breaking changes, or changes that may require depndency
  version bound changes. See https://pvp.haskell.org/. Also see the
  "Breaking changes section below".
* Optionally, look at hlint output if anything in that is worth fixing.
* Merge the PR by rebasing. Note that github always creates new commits when
  merged with rebase, it records the committer as well as the author in the
  commit. This makes the local branch diverge from master. You can rebase and
  merge the commit manually in your local git tree and push the resulting
  master branch to avoid new commits and divergence from the original tree.

## Release Checklist

* Check if any critical pending bugs or issues are to be included
* If this is a major release check if any previously deprecated features are to
  be removed in this release.
* _Documentation_:

    * README is updated
    * Haddock docs are consistent with the changes in the release
    * Tutorial has been updated for new changes
    * Documents in the `docs` directory are consistent with new changes
    * All combinators have time and space complexity annotations
    * All combinators have `since` annotation

* _Benchmarks_:

    * Check regressions from previous release
    * Run benchmarks with large stream size (`bench.sh -- long`) to
      check for space leaks and to ensure constant memory usage for streaming
      operations.
    * Run benchmarks with `dev` flag on. Some `fileio` benchmarks are disabled
      in regular runs.
    * Check comparative benchmarks using streaming-benchmarks

* _Tests_:

    * Run tests with `dev` flag on. Many tests are disabled in regular runs.

* _Examples_:

    * Update
      [streamly-examples][https://github.com/composewell/streamly-examples]
      to make sure it runs with the latest release.

* _Update Package Metadata:_

    * Update `stack.yaml` to latest stable resolver, cleanup extra-deps
    * Make sure the description in cabal file is in sync with README and other docs
    * Make sure CI configs include last three major releases of GHC in CI testing.
    * Update `tested-with` field
    * Make sure all dependency bounds can use latest versions
    * Make sure any additional files are added to `extra-source-files` in cabal
      file

* Copyrights and Contibutors

    * Make sure contributors to the release are listed in
      `credits/CONTRIBUTORS.md`.
    * Bump the release version in `credits/CONTRIBUTORS.md`.
    * Make sure any third party code included in the release has been listed in
      `credits/COPYRIGHTS.md` and the license is added to the repo.
    * Change the `Unreleased` section, if exists, at the top of
      `credits/COPYRIGHTS.md` to the new release version number.

* _Update changelog & Version_:

    * Make sure all the bug fixes being included in this release are marked
      with a target release on github. So that users can search by release if
      they want.
    * Change the `Unreleased` section at the top of changelog file to the new
      release version number.
    * Bump the package version in cabal file or package.yaml
    * Bump the package version in configure.ac and run autoreconf

* _Upload_:

    * Wait for final CI tests to pass:

        * Create a git tag corresponding to the release where X.Y.Z is the new
          package version (`git tag vX.Y.Z && git push -f origin vX.Y.Z`).
        * Mask out the build status lines from the README
        * Upload to hackage
          * Use a clean workspace to create source distribution
          by freshly cloning the git repository. The reason for
          doing this is that we use wild-cards in cabal file for
          `extra-source-files`, these wild-cards may match additional
          files lying around in the workspace and unintentionally ship
          them as well.
          * `cabal v2-sdist`; `cabal upload --publish <tarpath>`
          * `stack upload .`
        * Add to stackage (`build-constraints.yaml` in Stackage repo) if needed
        * Optionally upload `package-X.Y.Z-sdist.tar.gz` to github release page
            * Update release contributors on github release page
              (`git shortlog -s prev_tag..new_tag | sed $'s/^[0-9 \t]*/* /' | sort -f`)
        * Update and if needed release streaming-benchmarks package
        * Check https://matrix.hackage.haskell.org/package/streamly
        * Check haddocks on Hackage, upload if not built
        * Announce to haskell-cafe@haskell.org

## Breaking Changes

This section lists what constitutes breaking changes.  See
https://pvp.haskell.org/ breaking changes that can be determined by the
API alone. We specify some additional recommendations here:

Behavior changes, this kind of changes are nasty and should be avoided. If such
changes are made they should be emphasized adequately in the changelog:

* Silent changes in the behavior of an API without any changes to the
  signature.
* Even fixing a bug could break things as users may have employed workarounds
  for the bug.
* Changes in the behavior of a dependency may also cause a change in behavior.

Be cautious about the following:

* Change in version bounds of dependencies may cause compilation failure for
  some programs because they may not be able to find a build plan.
* Ideally, new warnings should not be considered breaking, dependencies
  should never be compiled with -Werror. But packages may not be following
  it perfectly.
* Deprecating an API may issue new warnings, however the code can still be
  compiled if warnings are not treated as errors.

Internal APIs:

* Internal APIs can change without having to change the major version. However,
  we should try to align the Internal API changes with a major release as long
  as possible.

## Managing Issues

We label the issues in different dimensions based on the lifecyle and different
management aspects of the issue. The folowing sections discuss the labels used
to manage the issues. If a new label is required, it should be discussed before
creating it.

### Disposition

The level-1 triaging of the issue determines the current disposition
of the issue. If no change is required the issue must have one of the
following labels:

* invalid
* question
* discussion
* duplicate
* wontfix

If a change is required we need to do level-2 triage of the issue, see the
sections below.

### Change type

When a change is required we need to put one of the __change type__
labels as part of level-2 triaging:

* performance: User visible impact on performance.
* usability: It is not convenient to use the library.
* documentation: documentation is not correct or sufficient.
* bug: A functionality issue, not working as expected.
* enhancement: A new feature or enhancement of the product.
* maintenance: A refactor or any other change with no user visible impact.

### Aspect

In addition we can put a __product aspect__ label describing a feature name or
any other product specific classification bucket.

* aspect:<aspect name>

### Severity

Optionally we can add a severity label to indicate how severe is the
impact of the bug/issue, which may determine how important it is to fix it. By
default the severity is normal, if it is high we put a label:

* severity:high

### Change impact

For a user visible issue whether it has a release/changelog impact:

* enhancement: includes a new feature or enhancement
* breaking: has a breaking impact on existing deployments
* deprecating: deprecates an existing functionality

__RULE__: Any commit that may affect the end user in some way MUST have
either a changelog entry OR MUST have an issue marked with one of the
above labels OR both.

### Priority

By default the issues are normal priority. We use a label for high priority
issues:

* priority:high

### Scheduling

If the issue is assigned to someone then it is considered scheduled. Otherwise
it is unscheduled.  Unassigned issues may have the following labels:

* deferred: blocked on any other fix or a decision to be made, or deliberately
  deferred for some reason.
* help-wanted: anyone can take the issue and contribute

## Correlating Changes, Issues and Releases

For planning purposes, open issues may be marked with milestones or target
releases.  However, it may not always be known which release a fix will finally
land in.  For example, we may decide to make a minor release instead of a major
one if there are no breaking changes yet, so we may not always know what would
be the next release version.

Trackability means that we should be able to find which issues got fixed in
which release. Or what all issues got fixed in a particular release. We track
significant changes using the changelog. However, there may be more changes
that can only be tracked via issues, PRs or commits.  When we make a release we
can mark all the issues fixed in that release with a correct release target for
future trackability.

For better trackability of which issue got fixed in which release we need the
following:

* Before you close an issue make sure a commit or a PR fixing the issue is
  attached with it. In the commit message you can reference an issue like
  "fixes #50", you can do the same in a PR as well.
* Before we make a new release EVERY issue with a commit included in that
  release that affects the end user, especially bugs and breaking changes MUST
  have the target release correctly set.

## Changelog Management

Keep the unreleased changes in the `Unreleased` section at the top of changelog
file.  Using `Unreleased` instead of the next release number for unreleased
changes is important. First, we do not know the next release number in advance,
it could be a major or minor release.  Second, if we use the next release
number instead, then when adding a new change to the changelog, at one look we
cannot know whether the release number on top is unreleased or released.

## TODO

* GPG signed releases and tags
