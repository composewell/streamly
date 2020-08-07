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
