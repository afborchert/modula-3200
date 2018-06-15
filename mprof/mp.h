/*
 *	mprof -- global types
 */

#define	NAMSIZ	14

typedef unsigned int address;
typedef unsigned int val;

struct proc {
	char p_name[NAMSIZ];
	int p_num;
	unsigned p_addr;
	float p_time;
	long p_ncall;
};

struct module {
	char * m_name;
	unsigned m_entry;
	int m_procs; /* # procedures */
	struct proc ** m_ptr;
#ifdef xelos
	char * m_file; 	  /* source file of module */
	union {
		struct pchain {
			int pc_num;	/* procedure number */
			address pc_entry;
			struct pchain * pc_link;
		} * _p_chain;
		struct pfield {
			address pf_entry;
		} * _p_field;
	} m_procs_;
#endif xelos
};

#ifdef xelos
#define	m_pchain	m_procs_._p_chain
#define	m_pfield	m_procs_._p_field
#endif

#ifndef xelos
#define	void int
#endif

