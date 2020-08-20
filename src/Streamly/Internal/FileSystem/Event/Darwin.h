#include <config.h>

struct watch;

struct pathName {
    const UInt8 *pathBytes;
    size_t pathLen;
};

int createWatch
    ( struct pathName* folders
    , int n  /* number of entries in "folders" */
    , UInt32 createFlags
    , UInt64 since
    , double latency
    , int* fd
    , void** wp
    );

void destroyWatch(struct watch* w);

/******************************************************************************
 * Create Flags
 *****************************************************************************/

UInt32 FSEventStreamCreateFlagNoDefer ();
UInt32 FSEventStreamCreateFlagWatchRoot ();
UInt32 FSEventStreamCreateFlagFileEvents ();
UInt32 FSEventStreamCreateFlagIgnoreSelf ();
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
UInt32 FSEventStreamCreateFlagFullHistory;
#endif

/******************************************************************************
 * Event Flags
 *****************************************************************************/

UInt32 FSEventStreamEventFlagEventIdsWrapped ();
UInt32 FSEventStreamEventFlagMustScanSubDirs ();
UInt32 FSEventStreamEventFlagKernelDropped ();
UInt32 FSEventStreamEventFlagUserDropped ();
UInt32 FSEventStreamEventFlagHistoryDone ();
UInt32 FSEventStreamEventFlagRootChanged ();
UInt32 FSEventStreamEventFlagMount ();
UInt32 FSEventStreamEventFlagUnmount ();
UInt32 FSEventStreamEventFlagItemChangeOwner ();
UInt32 FSEventStreamEventFlagItemInodeMetaMod ();
UInt32 FSEventStreamEventFlagItemFinderInfoMod ();
UInt32 FSEventStreamEventFlagItemXattrMod ();
UInt32 FSEventStreamEventFlagItemCreated ();
UInt32 FSEventStreamEventFlagItemRemoved ();
UInt32 FSEventStreamEventFlagItemRenamed ();
UInt32 FSEventStreamEventFlagItemModified ();
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
UInt32 FSEventStreamEventFlagItemCloned ();
#endif
UInt32 FSEventStreamEventFlagItemIsDir ();
UInt32 FSEventStreamEventFlagItemIsFile ();
UInt32 FSEventStreamEventFlagItemIsSymlink ();
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
UInt32 FSEventStreamEventFlagItemIsHardlink ();
#endif
UInt32 FSEventStreamEventFlagItemIsLastHardlink ();
