#ifndef STREAMLY_FUSION_ANNOTATIONS_H
#define STREAMLY_FUSION_ANNOTATIONS_H

/*
 * The GHC JavaScript backend evaluates each ANN pragma in its interpreter,
 * and the linker state retained for that grows with every module compiled
 * in a --make session. fusion-plugin cannot run on the JavaScript backend,
 * so the Fuse annotations are omitted there.
 *
 * FUSE_TYPE cannot take a type name containing a quote, traditional CPP
 * treats the quote as the start of a character literal. Guard such
 * annotations with "#ifdef FUSE_ANNOTATIONS" instead.
 */

#ifndef javascript_HOST_ARCH
#define FUSE_ANNOTATIONS
#define FUSE_TYPE(_t) {-# ANN type _t Fuse #-}
#else
#define FUSE_TYPE(_t)
#endif

#endif
