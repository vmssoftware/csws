/*
 * vdbm - ndbm work-alike hashed database library
 * based on Per-Ake Larson's Dynamic Hashing algorithms. BIT 18 (1978).
 * author: oz@nexus.yorku.ca
 * status: public domain. 
 */

/* These settings are -incompatible- with mod_dav [www.webdav.org/mod_dav/]
 * but are required for compatibility with mod_auth_dbm.  Do not link this 
 * build of vdbm into mod_dav and expect success, dav requires big records.
 */
#ifndef __VDBM_H
#define __VDBM_H 1

#define DBLKSIZ 4096
#define PBLKSIZ 1024
#define PAIRMAX 1008			/* arbitrary on PBLKSIZ-N */
#define SPLTMAX	10			/* maximum allowed splits */
					/* for a single insertion */
#define VDBM_DIRFEXT	".dbm"
#ifdef  VDBM_PAGFEXT
#undef  VDBM_PAGFEXT
#endif

typedef struct {
	void *pool;		       /* pool reference */
	int dirf;		       /* directory file descriptor */
	int pagf;		       /* page file descriptor */
	int flags;		       /* status/error flags, see below */
	long maxbno;		       /* size of dirfile in bits */
	long curbit;		       /* current bit number */
	long hmask;		       /* current hash mask */
	long blkptr;		       /* current block for nextkey */
	int keyptr;		       /* current key for nextkey */
	long blkno;		       /* current page to read/write */
	long pagbno;		       /* current page in pagbuf */
	char pagbuf[PBLKSIZ];	       /* page file block buffer */
	long dirbno;		       /* current block in dirbuf */
	char dirbuf[DBLKSIZ];	       /* directory file block buffer */
	int  lckcnt;                   /* number of calls to vdbm_lock */
} vdbm_t;

#define VDBM_RDONLY		0x1    /* data base open read-only */
#define VDBM_IOERR		0x2    /* data base I/O error */
#define VDBM_SHARED		0x2    /* data base open for sharing */
#define VDBM_SHARED_LOCK        0x4    /* data base locked for shared read */
#define VDBM_EXCLUSIVE_LOCK     0x8    /* data base locked for write */

/*
 * utility macros
 */
#if defined(vms) || defined(__VMS)
#else
#define vdbm_rdonly(db)		((db)->flags & VDBM_RDONLY)
#define vdbm_error(db)		((db)->flags & VDBM_IOERR)
#define vdbm_clearerr(db)	((db)->flags &= ~VDBM_IOERR)  /* ouch */
#define vdbm_dirfno(db)	((db)->dirf)
#define vdbm_pagfno(db)	((db)->pagf)
#endif

typedef struct {
	char *dptr;
	int dsize;
} vdbm_datum_t;

typedef vdbm_t VDBM;
typedef vdbm_datum_t DATUM;

extern vdbm_datum_t nullitem;

#ifdef __STDC__
#define proto(p) p
#else
#define proto(p) ()
#endif

/*
 * flags to vdbm_store
 */
#define VDBM_INSERT	0
#define VDBM_REPLACE	1

/*
 * ndbm interface
 */
extern vdbm_t *vdbm_open proto((char *, int, int));
extern void vdbm_close proto((vdbm_t *));
extern vdbm_datum_t vdbm_fetch proto((vdbm_t *, vdbm_datum_t));
extern int vdbm_delete proto((vdbm_t *, vdbm_datum_t));
extern int vdbm_store proto((vdbm_t *, vdbm_datum_t, vdbm_datum_t, int));
extern vdbm_datum_t vdbm_firstkey proto((vdbm_t *));
extern vdbm_datum_t vdbm_nextkey proto((vdbm_t *));
extern int vdbm_lock (vdbm_t *, int);
extern int vdbm_unlock (vdbm_t *);

/*
 * other
 */
extern vdbm_t *vdbm_prep proto((char *, char *, int, int));
extern long vdbm_hash proto((char *, int));

extern int vdbm_clearerr proto((vdbm_t *));
extern int vdbm_error proto((vdbm_t *));

int vdbm_cleanup_callback (void *);

#endif /* __VDBM_H */
