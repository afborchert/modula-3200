/*
 *	structure of a Modula-2 process
 *
 *	see rts/newprocess.s
 */

struct process {
       int p_base;
       int p_top;
       int p_limit;
       int p_pc;
       int p_started;
};
