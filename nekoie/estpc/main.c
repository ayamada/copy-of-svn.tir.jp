/* $Id$ */

/* ToDo: �����å���³���Ի��ˤϡ��ޥ���ǻ��ꤵ�줿���ޥ�ɤ�¹Ԥ���褦��
 *       ľ��(������쥯�Ƚ��Ϥϡ������ü���Ȥ��ư���) */

/* read����ȡ�cgi-metavariables��ľ��parameterize�Ǥ���alist�������롣
 * ���θ���̾��̤ꡢSTDIN��STDOUT�Ȥ��ư����롣 */
/* WARN: â�����᥿�ѿ���key�ޤ���val��ascii���ϰϳ��ΥХ����ͤ�¸�ߤ����硢
 * ����key�ޤ���val��ʸ������Դ���ʸ����Ȥ�����������롣
 * ����ξ�硢���ΤޤޤǤ�����Ȥʤ�١�
 * string-incomplete->complete�ˤ�����ɬ�פ����ꡢ
 * �ǥե���Ȥ�ces�ʳ��Υ��󥳡��ǥ��󥰤ǥ᥿�ѿ��������Ƥ����ǽ����
 * ������ϡ����ˡ������ɸ��ces�ؤ��Ѵ���Ԥ�ɬ�פ����롣
 */

/* mysql��cui���饤����Ȥ�""��Ǥϡ�\\��\n��\0��\"�λͤĤΥ��������פ�
 * �Ǿ��¥��������פ�ɬ�פʤ�ΤȤ��ư����Ƥ��롣
 * ����Ϥ����١����Ȥ�(gauche�Ǥ⤳�λͤĤ����������פ���Ƥ����ok�Ȥ���)��
 * ����ʸ�����environ�ʤΤǡ������Ǥ�\0�ʳ��λ��Ĥ�
 * ���������פ���褦�ˤ�����
 * �ġĤ����ޥ���Х���ʸ�����Դ���ʸ����Ȥ��ư���ʤ��ƤϤ����ʤ����ˡ�
 * ���Ȥǵ��դ����Τǡ��嵭��¾�ˡ�\x80-\xff�ޤǤΥХ����ͤϡ�
 * \xɽ�����Ѵ�����褦�ˤ�����
 * \\��\n�ˤĤ��Ƥ����ҤΤޤޤȤ��롣 */

#include <unistd.h>
extern char **environ;

#include <errno.h>
extern int errno;

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/time.h>
#include <signal.h>



/* socket�ؤ���³�˼��Ԥ�����硢����url�ؤȥ�����쥯�Ȥ�����Ϥ��֤� */
#ifndef SOCKET_ERROR_LOCATION_URL
#define SOCKET_ERROR_LOCATION_URL "http://d.tir.jp/path/to/connect_error.html"
#endif

/* ��³���٤�socket��path */
#ifndef SOCKET_PATH
#define SOCKET_PATH "/path/to/socket.sock"
#endif

/* ���Τ�Ϥ���ʸ�������� */
#ifndef PAREN_ALL_BEGIN
#define PAREN_ALL_BEGIN "("
#endif
#ifndef PAREN_ALL_END
#define PAREN_ALL_END ")"
#endif
/* key��val��Ϥ���ʸ�������� */
#ifndef PAREN_KEY_AND_VAL_BEGIN
#define PAREN_KEY_AND_VAL_BEGIN "("
#endif
#ifndef PAREN_KEY_AND_VAL_END
#define PAREN_KEY_AND_VAL_END ")"
#endif

/* key��val�δ֤�ʸ�������� */
#ifndef BETWEEN_KEY_AND_VAL
#define BETWEEN_KEY_AND_VAL " "
#endif

/* key��val��pair�δ֤�ʸ�������� */
#ifndef BETWEEN_PAIR
#define BETWEEN_PAIR " "
#endif

/* asciiʸ����Υ������ơ������ʸ�������� */
#ifndef ASCII_QUOT_BEGIN
#define ASCII_QUOT_BEGIN "\""
#endif
#ifndef ASCII_QUOT_END
#define ASCII_QUOT_END "\""
#endif

/* ��asciiʸ����(�Դ���ʸ����)�Υ������ơ������ʸ�������� */
#ifndef NONASCII_QUOT_BEGIN
#define NONASCII_QUOT_BEGIN "#*\""
#endif
#ifndef NONASCII_QUOT_END
#define NONASCII_QUOT_END "\""
#endif

/* �����ॢ�����Ԥ��ÿ� */
#ifndef SELECT_TIMEOUT
#define SELECT_TIMEOUT 60
#endif


#define ESTPC_BUF_SIZE 4096


/* ToDo: �Хåե���󥰤�������ƹ�®������ɬ�פ����� */
ssize_t putfd (char c, int fd) {
  return write(fd, &c, 1);
}

/* ToDo: �Хåե���󥰤�������ƹ�®������ɬ�פ����� */
void printfd (char *str, int fd) {
  int i = 0;
  while (1) {
    char c = str[i++];
    if (!c) break;
    putfd(c, fd);
  }
  return;
}


#define CONVERT_4BITNUM_TO_HEXCHAR(num) \
  if (10 <= num) { num += ('a' - 10); } else { num += '0'; }

/* ʸ���Υ��������פϡ�mysql��١����ˤ���
 * ( http://dev.mysql.com/doc/refman/4.1/ja/mysql-real-escape-string.html )
 * mysql�ǥ��������פ�����\�ס�'�ס�"�ס�\0�ס�\n�ס�\r�ס�\z�פ��⡢
 * �Ķ��ѿ��˴ޤ����ΤǤ��ʤ���\0�פȡ�
 * ��windows�Ķ�������Ȥ���٤ˡ�\z�פ��������
 * ��\�ס�'�ס�"�ס�\n�ס�\r�פθޤĤΤߥ��������פ�Ԥ����ͤȤ��롣
 * �ġĤ�����ǡ�\x80-\xff�Υ��������פ�ɬ�פˤʤä��١�
 * ���ҤθޤĤ��ɲäǡ����Υ��������פ�Ԥ���
 */
void escaped_putfd (const char c, int fd) {
  if ((c == '\\') || (c == '\'') || (c == '"')) {
    putfd('\\', fd);
    putfd(c, fd);
  }
  else if (c == '\n') {
    putfd('\\', fd);
    putfd('n', fd);
  }
  else if (c == '\r') {
    putfd('\\', fd);
    putfd('r', fd);
  }
  else if (128 <= (unsigned char)c) {
    char upper_char = (char)((unsigned char)c / 16);
    CONVERT_4BITNUM_TO_HEXCHAR(upper_char);
    char lower_char = (char)((unsigned char)c % 16);
    CONVERT_4BITNUM_TO_HEXCHAR(lower_char);
    putfd('\\', fd);
    putfd('x', fd);
    putfd(upper_char, fd);
    putfd(lower_char, fd);
  }
  else {
    putfd(c, fd);
  }
  return;
}

void write_sexp_env (int fd) {
  int cnt = -1;

  printfd(PAREN_ALL_BEGIN, fd);

  while (1) {
    cnt++;
    char *e = environ[cnt];
    if (e == NULL) break;

    /* ����ܰʹߤΥ���ȥ�ϡ����Ϥ������˶��������� */
    if (cnt) printfd(BETWEEN_PAIR, fd);

    int i = 0;
    int name = 1;

    /* ToDo: �ޤ��������ʳ��ǰ�ökey��val��ʬ�䤷��
     *       (����ǥ����ͤȽ�ü�����ݻ������ok)
     *       ���줾��ˤĤ��ơ�ascii�Τߤǹ�������Ƥ��뤫�ɤ�����Ĵ�١�
     *       ��ascii��ascii�ζ��̤��դ��롣
     *       �����Ǥζ�ʬ����Ū�ϡ�gauche��ʸ���󤬴������Դ�������
     *       Ĵ�٤�٤����ʤΤǡ�ascii���ɤ����ζ�ʬ�ϡ�
     *       ��������ʸ����0-127���ϰ��⤫�ɤ�����������Ȥ��롣
     *       (gauche�ϡ�7bit ascii���ϰϤʤ��˴�����ʸ����Ȥ��ư����褦��)
     */
    printfd(PAREN_KEY_AND_VAL_BEGIN, fd);
    printfd(NONASCII_QUOT_BEGIN, fd);
    /* ��ʸ�����Ľ������� */
    while (1) {
      char c = e[i++];

      if (c == 0) break;

      if (name && (c == '=')) {
        name = 0;
        printfd(NONASCII_QUOT_END, fd);
        printfd(BETWEEN_KEY_AND_VAL, fd);
        printfd(NONASCII_QUOT_BEGIN, fd);
      }
      else {
        escaped_putfd(c, fd);
      }
    }
    printfd(NONASCII_QUOT_END, fd);
    printfd(PAREN_KEY_AND_VAL_END, fd);
  }

  printfd(PAREN_ALL_END, fd);

  fsync(fd); /* flush���� */
  return;
}


void redirect_to_errorpage (void) {
  printf("Location: %s\r\n", SOCKET_ERROR_LOCATION_URL);
  printf("\r\n");
  fflush(stdout);
  return;
}

/* src_fd�����������ɤ߼�ꡢdst_fd�˽񤭽Ф���
 * �֤��ͤϡ��ޤ�src_fd�������Ƥ���(=�ޤ��ǡ���������)�ʤ�1��
 * �⤦src_fd�����Ǥ���(=read��0���֤���)�ʤ�0��
 * ���餫�Υ��顼��ȯ��������-1���֤���
 * ����ɬ��������select��src_fd���ɤ߼���ǽ�ʾ��֤Ǥ�������ǧ������� */
int stream_transfer (int src_fd, int dst_fd) {
  char _buf[ESTPC_BUF_SIZE];
  char *buf_ptr = _buf;
  ssize_t read_len = read(src_fd, buf_ptr, ESTPC_BUF_SIZE);
  if (read_len <= 0) return read_len; /* src_fd�����ǡ��ޤ����ɤ߹��ߥ��顼 */
  /* buf��dst_fd�˽񤭽Ф� */
  while (1) {
    ssize_t write_len = write(dst_fd, buf_ptr, read_len);
    if (write_len < 0) return write_len; /* �񤭹��ߥ��顼 */
    if (read_len == write_len) {
      /* ����˽񤭹��߽�λ */
      fsync(dst_fd); /* fflush���� */
      break;
    }
    /* �Ĥ����ʬ��ƽ񤭹��ߤ��ʤ��ƤϤʤ�ʤ� */
    buf_ptr += write_len;
    read_len -= write_len;
  }
  return 1;
}

void interconnect_stream (int sock_fd) {
  /* ����Ū�ˤϡ��ʲ��ε�ư�򤹤���ɤ��Ȼפ��롣
   * - select��Ȥäơ�stdin(STDIN_FILENO)�ȥ����åȤΤɤ��餫����
   *   �ǡ������ɤ߼�����֤ˤʤ�Τ��Ԥġ�
   * -- stdin(STDIN_FILENO)���ɤ߽Ф�ok�ˤʤä��顢
   *    �ɤ߽Ф���¤��ɤ߽Ф��ơ�����򥽥��åȤ˽񤭽Ф���
   * -- �����åȤ��ɤ߽Ф�ok�ˤʤä��顢
   *    �ɤ߽Ф���¤��ɤ߽Ф��ơ������stdout�˽񤭽Ф���
   * -- �ɤ��餫��EOF�ޤ��ϲ�����Υ��顼���褿�顢����˱�������λ������Ԥ�
   */

  /* nfds���������� */
  int nfds = ((STDIN_FILENO < sock_fd) ? sock_fd : STDIN_FILENO) + 1;

  /* sigmask���������� */
  sigset_t empty_sigset;
  sigset_t mask;
  sigemptyset(&empty_sigset); /* ��������������� */
  sigprocmask(SIG_BLOCK, &empty_sigset, &mask); /* ���ߤΥޥ������֤���� */
  sigaddset(&mask, SIGPIPE); /* ���ߤΥޥ����Υ��ԡ���SIGPIPE���ɲ� */
  /* SIGPIPE��̵�뤹����ˤĤ��Ƥϰʲ��򻲾�
   * http://www.kt.rim.or.jp/~ksk/sock-faq/unix-socket-faq-ja-2.html#ss2.19 */

  while (1) {
    /* fd_set����������(����˲������) */
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
    FD_SET(sock_fd, &readfds);

    /* timeval����������(man 2 select�ˤ��ȡ�
     * select����timeout���˲������Ķ�������餷���Τ������������) */
    struct timeval timeout;
    timeout.tv_sec = SELECT_TIMEOUT; /* �� */
    timeout.tv_usec = 0; /* �ޥ������� */

    /* select��¹Ԥ��� */
    int r = select(nfds, &readfds, NULL, NULL, &timeout);
    if (r == -1) {
      if (errno == EINTR) {
        /* �����ʥ��������� */
        /* �Ȥꤢ������λ�Ǥ����Ȼפ� */
        perror("caught a signal");
        break;
      }
      else {
        /* �褯ʬ����ʤ����顼 */
        /* ���顼���Ƥ�stderr�˽񤭽Ф�ɬ�פ����� */
        perror("unknown error in select");
        break;
      }
    }
    else if (FD_ISSET(STDIN_FILENO, &readfds)) {
      /* stdin�����ɤ߼���ǽ */
      /* Ŭ�����ɤ߼�ꡢ�ɤ߼�ä�ʬ�򥽥��åȤ˽񤭽Ф� */
      int r = stream_transfer(STDIN_FILENO, sock_fd);
      if (r <= 0) { break; } else { continue; }
    }
    else if (FD_ISSET(sock_fd, &readfds)) {
      /* �����åȤ����ɤ߼���ǽ */
      /* Ŭ�����ɤ߼�ꡢ�ɤ߼�ä�ʬ��stdout�˽񤭽Ф� */
      int r = stream_transfer(sock_fd, STDOUT_FILENO);
      if (r <= 0) { break; } else { continue; }
    }
    else {
      /* �����ॢ���Ȥ�����Τȹͤ����롣 */
      fprintf(stderr, "socket response timeout\n");
      break;
    }
    /* NOTREACHED */
  }
  return;
}

void failed_to_connect_socket (void) {
  /* ToDo: ���Ȥǥ��ޥ�ɼ¹��б������ */
  redirect_to_errorpage();
  return;
}

void disable_sigmask (void) {
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGPIPE);
  sigprocmask(SIG_BLOCK, &mask, NULL);
  return;
}

int main (void) {
  /* �����åȤ���³���� */
  int sock_fd = socket(PF_UNIX, SOCK_STREAM, 0);

  if (sock_fd < 0) {
    failed_to_connect_socket();
    return 1;
  }
  struct sockaddr_un uds;

  uds.sun_family = AF_UNIX;
  strcpy(uds.sun_path, SOCKET_PATH);
  int addrlen = sizeof(uds.sun_family) + strlen(uds.sun_path);

  int r = connect(sock_fd, (struct sockaddr*)&uds, addrlen);
  if (r < 0) {
    failed_to_connect_socket();
    return 1;
  }

  /* �ޤ��ǽ�ˡ�SIGPIPE��ػߤ��� */
  /* SIGPIPE��̵�뤹����ˤĤ��Ƥϰʲ��򻲾�
   * http://www.kt.rim.or.jp/~ksk/sock-faq/unix-socket-faq-ja-2.html#ss2.19 */
  disable_sigmask();

  /* �����åȤ˴Ķ��ѿ�������(estp/0.0) */
  write_sexp_env(sock_fd);

  /* stdin/stdout�ȥ����åȤ��ӤĤ��� */
  interconnect_stream(sock_fd);

  /* ��λ������Ԥ� */
  shutdown(sock_fd, 2);
  close(sock_fd);
  /* flush����Ȥ�����̣�ǡ�stdin����close���������ɤ��äݤ��褦����
   * (speedycgi�Ϥ������Ƥ���) */
  close(STDIN_FILENO);
  close(STDOUT_FILENO);
  close(STDERR_FILENO);

  return 0;
}

/* vim:set ft=c sw=2 ts=2 sts=2 et: */
