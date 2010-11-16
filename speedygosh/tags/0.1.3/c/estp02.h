/* $Id$ */

#ifndef _ESTP02_H_
#define _ESTP02_H_


#include <unistd.h>
extern char **environ;

#include <errno.h>
extern int errno;

#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/time.h>
#include <signal.h>

#define ERROR_FAILED_TO_CONNECT_SOCKET -1

int estp02_comm (char *sock_path, int argc, char *argv[], char **env);


#endif /* _ESTP02_H_ */

/* vim:set ft=c sw=2 ts=2 sts=2 et: */
