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
* Check pre-release APIs to be exposed, especially from streamly-examples
* PVP is adhered to, additionally do not change pre-release APIs in a
  minor release

* _Documentation_:

    * Documentation is updated.
    * Check if all the links in the docs are stable.
      * External links might point to the `master` branch of a few
        repositories. Pin them to a specific version that works with the current
        release.
      * You can run `rg '/blob/'` and `rg '/tree/'` to search for github links.
      * For example, links that point to `streamly-examples` point to its
        `master` branch. Instead, while making the release we need to pin the
        links to a specific revision that works with the latest commit of the
        current release.
      * Ideally, the distribution process should automatically replace these
        with current stable references.
    * The markdown documentation files have no broken links. See the section
      about [checking for broken
      links](#checking-for-broken-links-in-the-markdown-files) for more info.
    * Haddock docs are consistent with the changes in the release
    * Tutorial has been updated for new changes
    * Documents in the `docs` directory are consistent with new changes
    * All combinators have time and space complexity annotations
    * All combinators have `since` annotation
    * Modules that were renamed or APIs that moved to other modules should be
      marked as "since" the current release.
    * Check all released modules to ensure that they do not have any leftover
      `Internal` or `Pre-release` annotations.
    * Unreleased combinators should be marked with `Pre-release` or
      `Internal` annotations
    * Streamly homepage is updated with the release docs
      * FileSystem.Event.Windows/Darwin modules require special handling

* _Build and Test_:

    * All CIs are green
    * Run tests using bin/test.sh to test with memory restrictions
    * Manually build and test for all flags (esp. `dev` flag) not covered by CIs
    * Test prime GHC version with -O0 and -O1

* _Benchmarks_:

    * Check regressions from previous release
    * Run benchmarks with large stream size (`bench-runner --long --quick`) to
      check for space leaks and to ensure constant memory usage for streaming
      operations.
    * Run benchmarks with `dev` flag on. Some `fileio` benchmarks are disabled
      in regular runs.
    * Check comparative benchmarks using streaming-benchmarks

* _Dependencies_:

    * Make sure all dependency bounds can use latest versions
    * Update `stack.yaml` to latest stable resolver, cleanup extra-deps
    * Make sure fusion-plugin is up to date and uploaded
    * Check the dependency footprint of the library. See
      [Generating Dependency Graph](#generating-dependency-graph) section.

* _Dependent Packages_:

    * Update
      [streamly-examples](https://github.com/composewell/streamly-examples)
      to make sure it runs with the latest release.
    * Check the performance of examples where applicable
    * Update:
      * streamly-bytestring
      * streamly-process
      * streamly-lz4

* _Update Package Metadata:_

    * Make sure the description in cabal file is in sync with the docs
    * Make sure CI configs include last three major releases of GHC in CI testing.
    * Update GHC `tested-with` field
    * Update `docs/Compiling.md` with the distributions tested with
    * Any docs linked inside haddock/cabal/changelog or any other such file
      should go in the extra-doc-files section instead of
      extra-source-files. Otherwise hackage shows the links as broken.
    * Make sure any additional files are added to `extra-source-files` in cabal
      file. Artifacts required for build, test, benchmarks, docs, licenses
      should be packaged. Build environment customization may or may not be
      packaged.

* _Copyrights and Contibutors_

    * Make sure contributors to the release are listed in `docs/CONTRIBUTORS.md`
      under the current release title.
    * Make sure any third party code included in the release has been listed in
      `docs/Credits.md` and the license is added to the repo.

* _Update changelog & Version_:

    * Find API changes using `./packdiff streamly ver1 streamly ver2`
      and record them in `docs/User/ProjectRelated/ApiChangelogs/ver1-ver2.txt`.
    * Make sure all the bug fixes being included in this release are marked
      with a target release on github. So that users can search by release if
      they want.
    * Make sure the package version in configure.ac has the correct release
      version, else set the version correctly and run autoreconf. This might
      need to be done for a minor release.
    * Make sure the `Unreleased` section at the top of changelog file has the
      new release version number and the month/year of publishing.
    * Make sure the package version in the cabal file and package.yaml has the
      correct release version, else set the version correctly. This might need
      to be done on a minor release.
    * Bump the package version in any docs/links, use something like
      `rg '0\.8\.'|grep -v -i since` to find any remaining occurrences of the
      old release.  Check `rg -i unreleased` for any remaining todos.

* _Upload_:

    * Wait for final CI tests to pass:

        * Manually run packcheck for ghcjs, `bin/run-ci.sh --targets ghcjs`.
        * Mask out the build status lines from the
          [README](/docs/User/ProjectRelated/README.md)
        * Upload to hackage
          * Use a clean workspace to create source distribution
          by freshly cloning the git repository. The reason for
          doing this is that we use wild-cards in cabal file for
          `extra-source-files`, these wild-cards may match additional
          files lying around in the workspace and unintentionally ship
          them as well.
          * Upload and verify the candidate
            * `cabal v2-sdist; cabal upload <tarpath>`
            * `cabal v2-haddock --haddock-for-hackage --enable-doc; cabal upload -d <tarpath>`
          * Publish the package
            * `cabal v2-sdist`; `cabal upload --publish <tarpath>`
            * Run packcheck on the uploaded package. To get the latest uploaded
              version from hackage, run `cabal unpack <package-name>`. You might
              have to run `cabal update`.
        * Create a git tag of the form P-X.Y.Z corresponding to the release
          where X.Y.Z is the new package version and P is the package name (`git
          tag P-X.Y.Z && git push -f origin P-X.Y.Z`). If the package name is
          implicit, P can be omitted and the tag can be of the form vX.Y.Z
        * Add to stackage (`build-constraints.yaml` in Stackage repo) if needed
        * Update and if needed release streaming-benchmarks package
        * Check https://matrix.hackage.haskell.org/package/streamly
        * Check haddocks on Hackage, upload if not built
        * Announce to haskell-cafe@haskell.org

### Post Release Tasks

* _Bump package version_:
    * After a major release, bump the package version in cabal file or
      package.yaml to the next major release target.
    * Bump the package version in configure.ac to the next major release target
      and run autoreconf.
    * Sync the package version of `streamly` and `streamly-docs`.
    * Add an `Unreleased` section at the top of changelog file with the next
      major release target.

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

* disposition:invalid
* disposition:question
* disposition:discussion
* disposition:duplicate
* disposition:wontfix

If a change is required we need to do level-2 triage of the issue, see the
sections below.

### Change type

When a change is required we need to put one of the __change type__
labels as part of level-2 triaging:

* type:performance: User visible impact on performance.
* type:testing: Improves testing or related to testing.
* type:usability: It is not convenient to use the library.
* type:documentation: documentation is not correct or sufficient.
* type:bug: A functionality issue, not working as expected.
* type:enhancement: includes a new feature or enhancement
* type:maintenance: A refactor or any other change with no user visible impact.

### Aspect

In addition we can put a __product aspect__ label describing a feature name or
any other product specific classification bucket.

* aspect:<aspect name>

### Severity

Optionally we can add a severity label to indicate how severe is the
impact of the bug/issue, which may determine how important it is to fix it. By
default the severity is normal, if it is high we put a label:

* severity:high

### API impact

For a user visible issue whether it has a release/changelog impact:

* api:new: includes a new feature or enhancement
* api:breaking: has a breaking impact on existing deployments
* api:deprecating: deprecates an existing functionality

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

* sched:deferred: deliberately deferred for some reason.
* sched:blocked: blocked on any other fix or a decision to be mad

### For Contributors

* help-wanted: anyone can take the issue and contribute
* good-first-issue: good for new contributors

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

## Generating Dependency Graph

Ways to get the dependencies. In the streamly package top dir:

```
# Keep only streamly in the packages section of stack.yaml
$ stack --system-ghc dot . --external|dot -Tpdf > streamly.pdf
```

Using nix, remove the test and benchmark components from
`shellFor/packages` in `default.nix` to get library only
dependencies. May show additional ghc boot libraries.

```
$ nix-shell --run "ghc-pkg dot | dot -Tpdf > streamly.pdf"
```

## Checking for broken symlinks

The following command - when run from the repository root - finds all the
broken symlinks in the project.

```
find . -xtype l \
    -not -path "./dist*" \
    -not -path "./.stack*" \
    -not -path "./benchmark/bench-report/dist*"
```

## Checking for broken links in the markdown files

You can use [markdown-link-check](https://github.com/tcort/markdown-link-check)
for finding broken links.

You can run it using `npx` like so:
```
npx markdown-link-check <markdown-file>
```

The following command - when run from the repository root - finds all the
markdown files that we need to consider:

```
find . -type f -name "*.md" \
    -not -path "./dist*" \
    -not -path "./.stack*" \
    -not -path "./benchmark/bench-report/dist*"
```

The following executes the markdown link checking with proper configuration on
the results. This also checks all the links are absolute to the root of the
repo.

```
find . -type f -name "*.md" \
    -not -path "./dist*" \
    -not -path "./.stack*" \
    -not -path "./benchmark/bench-report/dist*" \
    -exec npx markdown-link-check -c /dev/stdin --quiet {} \; <<EOF
{
  "ignorePatterns": [
    {
      "pattern": "^http"
    },
    {
      "pattern": "^ftp"
    },
    {
      "pattern": "^mailto"
    }
  ],
  "replacementPatterns": [
    {
      "pattern": "^", "replacement": "{{BASEURL}}"
    }
  ]
}
EOF
```

Once you run that, you can check all the links again without the config,
filtering in only `http`, `ftp`, and `mailto` links.

```
find . -type f -name "*.md" \
    -not -path "./dist*" \
    -not -path "./.stack*" \
    -not -path "./benchmark/bench-report/dist*" \
    -exec npx markdown-link-check --quiet {} \; 2>&1 | grep -e "http" -e "ftp" -e "mailto"
```
