DEFINITION MODULE Sys;

   CONST (* XELOS system calls *)
      exit    = 1;   sbrk    = 17;  utime   = 30;  rread   = 50;  
      fork    = 2;   stat    = 18;  access  = 33;  acct    = 51;  
      read    = 3;   lseek   = 19;  nice    = 34;  phys    = 52;  
      write   = 4;   getpid  = 20;  sync    = 36;  semsys  = 53;  
      open    = 5;   getppid = 20;  kill    = 37;  ioctl   = 54;  
      close   = 6;   mount   = 21;  setpgrp = 39;  lockf   = 55;  
      wait    = 7;   umount  = 22;  dup     = 41;  rwrite  = 56;  
      creat   = 8;   setuid  = 23;  pipe    = 42;  uname   = 57;  
      link    = 9;   getuid  = 24;  times   = 43;  ustat   = 57;  
      unlink  = 10;  geteuid = 24;  profil  = 44;  execve  = 59;  
      chdir   = 12;  stime   = 25;  plock   = 45;  umask   = 60;  
      time    = 13;  ptrace  = 26;  setgid  = 46;  chroot  = 61;  
      mknod   = 14;  alarm   = 27;  getgid  = 47;  fcntl   = 62;  
      chmod   = 15;  fstat   = 28;  getegid = 47;  ulimit  = 63;  
      chown   = 16;  pause   = 29;  signal  = 48;  ptracex = 65;  
      rti     = 67;

END Sys. 
