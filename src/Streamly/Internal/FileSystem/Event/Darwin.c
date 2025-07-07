/*
 * Code adapted from the Haskell "hfsevents" package.
 *
 * Copyright (c) 2012, Luite Stegeman
 *
 */

#include <config.h>

#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS

#include <CoreServices/CoreServices.h>
#include <pthread.h>
#include <unistd.h>
#include <FileSystem/Event/Darwin.h>

/*
 * For reference documentaion see:
 * https://developer.apple.com/documentation/coreservices/file_system_events?language=objc
 * https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html
 *
 * An OS thread is started which runs the event loop. A pipe is created
 * and the events are sent to the pipes. The receiver can read the pipe
 * output end to get the events.
 */

/******************************************************************************
 * Create Flags
 *****************************************************************************/

UInt32 FSEventStreamCreateFlagNoDefer () {
  return kFSEventStreamCreateFlagNoDefer;
}
UInt32 FSEventStreamCreateFlagWatchRoot () {
  return kFSEventStreamCreateFlagWatchRoot;
}
UInt32 FSEventStreamCreateFlagFileEvents () {
  return kFSEventStreamCreateFlagFileEvents;
}
UInt32 FSEventStreamCreateFlagIgnoreSelf () {
  return kFSEventStreamCreateFlagIgnoreSelf;
}
#if 0
UInt32 FSEventStreamCreateFlagFullHistory = kFSEventStreamCreateFlagFullHistory;
#endif

/******************************************************************************
 * Event Flags
 *****************************************************************************/

UInt32 FSEventStreamEventFlagEventIdsWrapped () {
  return kFSEventStreamEventFlagEventIdsWrapped;
}
UInt32 FSEventStreamEventFlagMustScanSubDirs () {
  return kFSEventStreamEventFlagMustScanSubDirs;
}
UInt32 FSEventStreamEventFlagKernelDropped () {
  return kFSEventStreamEventFlagKernelDropped;
}
UInt32 FSEventStreamEventFlagUserDropped () {
  return kFSEventStreamEventFlagUserDropped;
}
UInt32 FSEventStreamEventFlagHistoryDone () {
  return kFSEventStreamEventFlagHistoryDone;
}
UInt32 FSEventStreamEventFlagRootChanged () {
  return kFSEventStreamEventFlagRootChanged;
}
UInt32 FSEventStreamEventFlagMount () {
  return kFSEventStreamEventFlagMount;
}
UInt32 FSEventStreamEventFlagUnmount () {
  return kFSEventStreamEventFlagUnmount;
}
UInt32 FSEventStreamEventFlagItemChangeOwner () {
  return kFSEventStreamEventFlagItemChangeOwner;
}
UInt32 FSEventStreamEventFlagItemInodeMetaMod () {
  return kFSEventStreamEventFlagItemInodeMetaMod;
}
UInt32 FSEventStreamEventFlagItemFinderInfoMod () {
  return kFSEventStreamEventFlagItemFinderInfoMod;
}
UInt32 FSEventStreamEventFlagItemXattrMod () {
  return kFSEventStreamEventFlagItemXattrMod;
}
UInt32 FSEventStreamEventFlagItemCreated () {
  return kFSEventStreamEventFlagItemCreated;
}
UInt32 FSEventStreamEventFlagItemRemoved () {
  return kFSEventStreamEventFlagItemRemoved;
}
UInt32 FSEventStreamEventFlagItemRenamed () {
  return kFSEventStreamEventFlagItemRenamed;
}
UInt32 FSEventStreamEventFlagItemModified () {
  return kFSEventStreamEventFlagItemModified;
}
#if __MAC_OS_X_VERSION_MIN_REQUIRED >= 101300
UInt32 FSEventStreamEventFlagItemCloned () {
  return kFSEventStreamEventFlagItemCloned;
}
#endif
UInt32 FSEventStreamEventFlagItemIsDir () {
  return kFSEventStreamEventFlagItemIsDir;
}
UInt32 FSEventStreamEventFlagItemIsFile () {
  return kFSEventStreamEventFlagItemIsFile;
}
UInt32 FSEventStreamEventFlagItemIsSymlink () {
  return kFSEventStreamEventFlagItemIsSymlink;
}
#if __MAC_OS_X_VERSION_MIN_REQUIRED >= 101000
UInt32 FSEventStreamEventFlagItemIsHardlink () {
  return kFSEventStreamEventFlagItemIsHardlink;
}
#endif
UInt32 FSEventStreamEventFlagItemIsLastHardlink () {
  return kFSEventStreamEventFlagItemIsLastHardlink;
}

/******************************************************************************
 * Event watch
 *****************************************************************************/

/* Write an event to the pipe input fd */
static void writeEvent(int fd, UInt64 eventId, UInt64 eventFlags, char* path)
{
    UInt64 buf[3];
    /* XXX Is the path string in UTF-8? */
    size_t len = strlen(path);

    buf[0] = eventId;
    buf[1] = eventFlags;
    buf[2] = (UInt64)len;
    write(fd, buf, 3 * sizeof(UInt64));
    write(fd, path, len);
}

struct watch
{
    FSEventStreamRef eventStream;
    dispatch_queue_t queue;
    int writefd;
};

/* Just writes the event to the pipe input fd */
static void watchCallback
    ( ConstFSEventStreamRef streamRef
    , void *clientCallBackInfo
    , size_t n
    , void *eventPaths
    , const FSEventStreamEventFlags eventFlags[]
    , const FSEventStreamEventId eventIds[]
    )
{
    int i;
    struct watch *w = clientCallBackInfo;
    char **paths = eventPaths;

    for (i = 0; i < n; i++) {
        writeEvent(w->writefd, eventIds[i], eventFlags[i], paths[i]);
    }
}

#define MAX_WATCH_PATHS 4096

static void free_cffolders(CFStringRef *cffolders, int n) {
    int i;
    for (i = 0; i < n; i++) {
        CFRelease (cffolders[i]);
    }
    free(cffolders);
}

int createWatch
    ( struct pathName* folders
    , int n  /* number of entries in folders */
    , UInt32 createFlags
    , UInt64 since
    , double latency
    , int* fd
    , void** wp
    )
{
    if (n > MAX_WATCH_PATHS) {
      return -1;
    }

    int pfds[2];
    if (pipe (pfds)) {
      return -1;
    }

    /*
     * XXX We can possibly use since == 0 to get all events since
     * beginning of time
     */
    if (!since) {
      since = kFSEventStreamEventIdSinceNow;
    }

    struct watch *w = malloc(sizeof(struct watch));
    if (!w) {
      goto cleanup_pipe;
    }

    /* Setup paths array */
    CFStringRef *cffolders = malloc(n * sizeof(CFStringRef));
    if (!cffolders) {
      goto cleanup_watch;
    }

    /* Create event stream using paths and context*/
    int i;
    for(i = 0; i < n; i++) {
      cffolders[i] = CFStringCreateWithBytes
          ( NULL
          , folders[i].pathBytes
          , folders[i].pathLen
          , kCFStringEncodingUTF8
          , false
          );
      if (!cffolders[i]) {
          free_cffolders(cffolders, i);
          goto cleanup_watch;
      }
    }
    CFArrayRef paths = CFArrayCreate(NULL, (const void **)cffolders, n, NULL);
    if (!paths) {
      free_cffolders(cffolders, n);
      goto cleanup_watch;
    }

    FSEventStreamContext ctx;
    ctx.version = 0;
    ctx.info = (void*)w;
    ctx.retain = NULL;
    ctx.release = NULL;
    ctx.copyDescription = NULL;

    w->eventStream = FSEventStreamCreate
        (NULL, &watchCallback, &ctx, paths, since, latency, createFlags);
    free_cffolders(cffolders, n);
    CFRelease(paths);

    if(w->eventStream == NULL) {
      goto cleanup_watch;
    }

    w->queue = dispatch_queue_create("com.composewell.streamly", NULL);
    if (!w->queue) {
      goto cleanup_es;
    }

    w->writefd = pfds[1];
    *fd = pfds[0];
    *wp = w;
    FSEventStreamSetDispatchQueue(w->eventStream, w->queue);
    FSEventStreamStart(w->eventStream);
    return 0;

cleanup_es:
    FSEventStreamRelease(w->eventStream);
cleanup_watch:
    free(w);
cleanup_pipe:
    close(pfds[0]);
    close(pfds[1]);
    return -1;
}

/******************************************************************************
 * Stop a watch event loop
 *****************************************************************************/

void destroyWatch(struct watch* w) {
    /* Stop and invalidate the event stream */
    FSEventStreamFlushSync(w->eventStream);
    FSEventStreamStop(w->eventStream);
    FSEventStreamInvalidate(w->eventStream);
    dispatch_release(w->queue);
    FSEventStreamRelease(w->eventStream);
    close(w->writefd);
    free(w);
}
#endif
