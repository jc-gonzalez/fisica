#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#define NSTRS      3
#define ADDRESS    "mysocket"

char *strs[NSTRS] = {
  "Esta es la primera.\n",  
  "Esta es la segunda.\n",  
  "Esta es la tercera.\n"
};

main()
{
  char c;
  FILE *fp;
  register int i, s, len;

  struct sockaddr_un saun;
  
  /* get socket to work with */
  if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
    perror("client: socket");
    exit(1);
  }

  /* create the address we will be connecting to */
  saun.sun_family = AF_UNIX;
  strcpy(saun.sun_path, ADDRESS);

  /* try to connect */
  len = sizeof(saun.sun_family) + strlen(saun.sun_path);

  if (connect(s, &saun, len) < 0) {
    perror("client: connect");
    exit(1);
  }

  /* we'll use stdio for reading the socket */
  fp = fdopen(s, "r");

  /* read strings from the server, and print them out */
  for (i=0; i<NSTRS; i++){
    while ((c=fgetc(fp)) != EOF) {
      putchar(c);

      if (c=='\n')
        break;
    }
  }

  /* now we send some strings to the server */
  for (i=0; i<NSTRS; i++) 
    send(s, strs[i], strlen(strs[i]), 0);

  /* close */
  close(s);

  exit(0);
}
