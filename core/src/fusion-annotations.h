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
 *
 * The NoSpecConstr annotations are omitted on the JavaScript backend too.
 * The interpreter serializes an annotation value with a 32-bit Int, the
 * compiler deserializes it with a 64-bit Int, and the SpecConstr pass fails
 * with "deserializeFixedWidthNum: unexpected end of stream" when it reads
 * the annotation.
 */

#ifndef javascript_HOST_ARCH
#define FUSE_ANNOTATIONS
#define FUSE_TYPE(_t) {-# ANN type _t Fuse #-}
#define SPEC_CONSTR_ANNOTATIONS
#define NO_SPEC_CONSTR_TYPE(_t) {-# ANN type _t NoSpecConstr #-}
#else
#define FUSE_TYPE(_t)
#define NO_SPEC_CONSTR_TYPE(_t)
#endif

#endif
