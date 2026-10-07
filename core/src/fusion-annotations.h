#ifndef STREAMLY_FUSION_ANNOTATIONS_H
#define STREAMLY_FUSION_ANNOTATIONS_H

/*
 * ANN_TYPE(type, annotation) expands to an ANN type pragma, it expands to
 * nothing on the GHC JavaScript backend:
 *
 * - The JavaScript backend evaluates each ANN pragma in its interpreter, and
 *   the linker state retained for that grows with every module compiled in a
 *   --make session. fusion-plugin cannot run on the JavaScript backend, so
 *   the Fuse annotations are useless there.
 *
 * - The interpreter serializes an annotation value with a 32-bit Int, the
 *   compiler deserializes it with a 64-bit Int, and the SpecConstr pass fails
 *   with "deserializeFixedWidthNum: unexpected end of stream" when it reads
 *   a NoSpecConstr annotation.
 *
 * ANN_TYPE cannot take a type name containing a quote, traditional CPP
 * treats the quote as the start of a character literal. Guard such
 * annotations with "#ifdef FUSE_ANNOTATIONS" instead.
 */

#ifndef javascript_HOST_ARCH
#define FUSE_ANNOTATIONS
#define ANN_TYPE(_t,_a) {-# ANN type _t _a #-}
#else
#define ANN_TYPE(_t,_a)
#endif

#endif
