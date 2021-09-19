#ifndef __SPL_LOADED
#define __SPL_LOADED 1

extern apr_status_t apache$spl_proc_create(apr_proc_t *, const char *,
					   const char *const *,
					   const char *const *,
					   apr_procattr_t *, apr_pool_t *);
extern apr_status_t apache$spl_socket_open(apr_file_t **, const char *,
					   apr_pool_t *);
extern apr_status_t apache$spl_file_open(apr_file_t **, const char *,
					 apr_int32_t, apr_fileperms_t,
					 apr_pool_t *);
extern apr_status_t apache$spl_file_close(apr_file_t *);
extern int apache$spl_write_resources(FILE *);
extern int apache$spl_write_messages(void);
extern void apache$spl_flush(void);
extern void apache$spl_new(void);

#endif
