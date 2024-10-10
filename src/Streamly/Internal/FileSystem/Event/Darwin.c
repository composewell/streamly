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
    buf[0] = eventId;
    buf[1] = eventFlags;
    /* XXX Is the path string in UTF-8? */
    buf[2] = (UInt64)strlen(path);
    write(fd, buf, 3 * sizeof(UInt64));
    write(fd, path, strlen(path));
}

/* thread state */
struct watch
{
    FSEventStreamRef eventStream;
    CFRunLoopRef runLoop;
    int writefd;
    pthread_mutex_t mut;
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

/******************************************************************************
 * Start a watch event loop
 *****************************************************************************/

/* Event loop run in a pthread */
static void *watchRunLoop(void *vw)
{
    struct watch* w = (struct watch*) vw;
    CFRunLoopRef rl = CFRunLoopGetCurrent();
    CFRetain(rl);
    w->runLoop = rl;
    FSEventStreamScheduleWithRunLoop(w->eventStream, rl, kCFRunLoopDefaultMode);
    FSEventStreamStart(w->eventStream);
    pthread_mutex_unlock(&w->mut);
    CFRunLoopRun();
    pthread_exit(NULL);
}

#define MAX_WATCH_PATHS 4096

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

    /* Setup paths array */
    CFStringRef *cffolders = malloc(n * sizeof(CFStringRef));
    int i;
    for(i = 0; i < n; i++) {
      cffolders[i] = CFStringCreateWithBytes
          ( NULL
          , folders[i].pathBytes
          , folders[i].pathLen
          , kCFStringEncodingUTF8
          , false
          );
    }
    CFArrayRef paths = CFArrayCreate(NULL, (const void **)cffolders, n, NULL);

    /* Setup context */
    struct watch *w = malloc(sizeof(struct watch));
    FSEventStreamContext ctx;
    ctx.version = 0;
    ctx.info = (void*)w;
    ctx.retain = NULL;
    ctx.release = NULL;
    ctx.copyDescription = NULL;

    /* Create watch using paths and context*/
    FSEventStreamRef es = FSEventStreamCreate
        (NULL, &watchCallback, &ctx, paths, since, latency, createFlags);

    /* Run the event loop in a pthread */
    int retval;
    if(es != NULL) {
        /* Success */
        w->writefd = pfds[1];
        w->eventStream = es;
        w->runLoop = NULL;

        /* Lock to prevent race against watch destroy */
        pthread_mutex_init(&w->mut, NULL);
        pthread_mutex_lock(&w->mut);
        pthread_t t;
        pthread_create(&t, NULL, &watchRunLoop, (void*)w);

        /* return the out fd and the watch struct */
        *fd = pfds[0];
        *wp = w;
        retval = 0;
    } else {
        /* Failure */
        close(pfds[0]);
        close(pfds[1]);
        free(w);
        retval = -1;
    }

    /* Cleanup */
    for (i = 0; i < n; i++) {
        CFRelease (cffolders[i]);
    }
    free(cffolders);
    CFRelease(paths);
    return retval;
}

/******************************************************************************
 * Stop a watch event loop
 *****************************************************************************/

void destroyWatch(struct watch* w) {
    /* Stop the loop so the thread will exit */
    pthread_mutex_lock(&w->mut);
    FSEventStreamStop(w->eventStream);
    FSEventStreamInvalidate(w->eventStream);
    CFRunLoopStop(w->runLoop);
    CFRelease(w->runLoop);
    FSEventStreamRelease(w->eventStream);
    close(w->writefd);
    pthread_mutex_unlock(&w->mut);

    /* Cleanup */
    pthread_mutex_destroy(&w->mut);
    free(w);
}
#endif
