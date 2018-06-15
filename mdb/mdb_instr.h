/*
 *	mdb - instruction structure
 */

struct instr {
	int type;	/* instruction format */
	char op;	/* op-code */
	char reg[3];	/* registers */
	int disp;	/* displacement */
};

/*
 * instruction formats
 */
#define	SF	1
#define RR      2
#define	RX1	3
#define	RX2	4
#define	RX3	5
#define	RI1	6
#define	RI2	7

#define	BT	0x10	/* branch instruction (true)	*/
#define	BF	0x20	/* branch instruction (false)	*/
#define	BBACK	0x40	/* branch backwards short	*/
#define BAL	0x80	/* branch & link */
#define RXRX    0x100   /* RX/RX format                 */

/* symbol types */

#define	NSYM	1
#define	ISYM	2
#define	DSYM	3
