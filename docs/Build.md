# GHC Options

* Recommended compiler options are: `-O2 -fspec-constr-recursive=10`
* To make use of multiple cores, use: `-threaded -with-rtsopts "-N"`

# Known Issues

GHC 8.2.2 may hog memory and hang when building certain application using
streamly (particularly the streamly benchmarks). Therefore we recommend
avoiding using the GHC version 8.2.x.
