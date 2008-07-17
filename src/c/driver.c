#include <stdio.h>

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "lacweb.h"

int lw_port = 8080;
int lw_backlog = 10;
int lw_bufsize = 1024;

void lw_handle(lw_context, char*);

static void worker(int sock) {
  char buf[lw_bufsize+1], *back = buf, *s;

  while (1) {
    int r = recv(sock, back, lw_bufsize - (back - buf), 0);

    if (r < 0) {
      fprintf(stderr, "Recv failed\n");
      close(sock);
      return;
    }

    if (r == 0) {
      printf("Connection closed.\n");
      close(sock);
      return;
    }

    printf("Received %d bytes.\n", r);

    back += r;
    *back = 0;
    
    if (s = strstr(buf, "\r\n\r\n")) {
      char *cmd, *path;
      lw_context ctx;

      *s = 0;
      
      if (!(s = strstr(buf, "\r\n"))) {
        fprintf(stderr, "No newline in buf\n");
        close(sock);
        return;
      }

      *s = 0;
      cmd = s = buf;
      
      if (!strsep(&s, " ")) {
        fprintf(stderr, "No first space in HTTP command\n");
        close(sock);
        return;
      }

      if (strcmp(cmd, "GET")) {
        fprintf(stderr, "Not ready for non-get command: %s\n", cmd);
        close(sock);
        return;
      }

      path = s;
      if (!strsep(&s, " ")) {
        fprintf(stderr, "No second space in HTTP command\n");
        close(sock);
        return;
      }

      printf("Serving URI %s....\n", path);

      ctx = lw_init(1024, 1024);
      lw_write (ctx, "HTTP/1.1 200 OK\r\n");
      lw_write(ctx, "Content-type: text/html\r\n\r\n");
      lw_write(ctx, "<html>");
      lw_handle(ctx, path);
      lw_write(ctx, "</html>");

      lw_send(ctx, sock);

      lw_free(ctx);
      printf("Done with client.\n\n");
      close(sock);
      return;
    }
  }
}

int main() {
  // The skeleton for this function comes from Beej's sockets tutorial.
  int sockfd, new_fd;  // listen on sock_fd, new connection on new_fd
  struct sockaddr_in my_addr;
  struct sockaddr_in their_addr; // connector's address information
  int sin_size, yes = 1;

  sockfd = socket(PF_INET, SOCK_STREAM, 0); // do some error checking!

  if (sockfd < 0) {
    fprintf(stderr, "Listener socket creation failed\n");
    return 1;
  }

  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
    fprintf(stderr, "Listener socket option setting failed\n");
    return 1;
  }

  my_addr.sin_family = AF_INET;         // host byte order
  my_addr.sin_port = htons(lw_port);    // short, network byte order
  my_addr.sin_addr.s_addr = INADDR_ANY; // auto-fill with my IP
  memset(my_addr.sin_zero, '\0', sizeof my_addr.sin_zero);

  if (bind(sockfd, (struct sockaddr *)&my_addr, sizeof my_addr) < 0) {
    fprintf(stderr, "Listener socket bind failed\n");
    return 1;
  }

  if (listen(sockfd, lw_backlog) < 0) {
    fprintf(stderr, "Socket listen failed\n");
    return 1;
  }

  sin_size = sizeof their_addr;

  printf("Listening on port %d....\n", lw_port);

  while (1) {
    new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);

    if (new_fd < 0) {
      fprintf(stderr, "Socket accept failed\n");
      return 1;
    }

    printf("Accepted connection.\n");
    worker(new_fd);
  }
}
