#ifndef __SPS_LOADED
#define __SPS_LOADED 1

extern apr_status_t apache$sps_socket_create(apr_socket_t **, int, int,
					     apr_pool_t *, char *);
extern apr_status_t apache$sps_socket_bind(apr_socket_t *,
					   apr_sockaddr_t *);
extern apr_status_t apache$sps_socket_close(apr_socket_t *);
extern apr_status_t apache$sps_port_lock(char *);
extern apr_status_t apache$sps_port_unlock(char *);
extern apr_status_t apache$sps_write_resources(FILE *);

#endif
