#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#define NSTRS      3
#define ADDRESS    "mysocket"

char *strs[NSTRS] = {
  "Esta es la primera desde el servidor.\n",  
  "Esta es la segunda desde el servidor.\n",  
  "Esta es la tercera desde el servidor.\n"
};

main()
{
  char c;
  FILE *fp;
  int fromlen;
  register int i, s, ns, len;

  struct sockaddr_un saun, fsaun;
  
  /* get socket to work with */
  if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
    perror("server: socket");
    exit(1);
  }

  /* create the address we will be connecting to */
  saun.sun_family = AF_UNIX;
  strcpy(saun.sun_path, ADDRESS);

  /* try to bind */
  unlink(ADDRESS);
  len = sizeof(saun.sun_family) + strlen(saun.sun_path);

  if (bind(s, &saun, len) < 0) {
    perror("server: bind");
    exit(1);
  }

  /* listen on server */
  if (listen(s, 5) < 0) {
    perror("server: listen");
    exit(1);
  }
        
  /* accept connections */
  if ((ns = accept(s, &fsaun, &fromlen)) < 0) {
    perror("server: accept");
    exit(1);
  }

  /* we'll use stdio for reading the socket */
  fp = fdopen(ns, "r");

  /* first we send some strings to the client */
  for (i=0; i<NSTRS; i++) 
    send(ns, strs[i], strlen(strs[i]), 0);

  /* then read strings from the client, and print them out */
  for (i=0; i<NSTRS; i++){
    while ((c=fgetc(fp)) != EOF) {
      putchar(c);

      if (c=='\n')
        break;
    }
  }

  /* close */
  close(s);

  exit(0);
}
