DEFINITION MODULE Errno;

   CONST 
      EPERM   = 1;         EACCES  = 13;        ENOTTY  = 25;  
      ENOENT  = 2;         EFAULT  = 14;        ETXTBSY = 26;  
      ESRCH   = 3;         ENOTBLK = 15;        EFBIG   = 27;  
      EINTR   = 4;         EBUSY   = 16;        ENOSPC  = 28;  
      EIO     = 5;         EEXIST  = 17;        ESPIPE  = 29;  
      ENXIO   = 6;         EXDEV   = 18;        EROFS   = 30;  
      E2BIG   = 7;         ENODEV  = 19;        EMLINK  = 31;  
      ENOEXEC = 8;         ENOTDIR = 20;        EPIPE   = 32;  
      EBADF   = 9;         EISDIR  = 21;        EDOM    = 33;  
      ECHILD  = 10;        EINVAL  = 22;        ERANGE  = 34;  
      EAGAIN  = 11;        ENFILE  = 23;
      ENOMEM  = 12;        EMFILE  = 24;  

      (* UNIX System V *)
      ENOMSG  = 35;        EIDRM   = 36;        EDEADLOCK = 45;

   VAR 
      errno: CARDINAL;

END Errno. 
