/* $Id$ */

#include "estp02.h"

/* ʸ���Υ��������פϡ�mysql��١����ˤ���
 * ( http://dev.mysql.com/doc/refman/4.1/ja/mysql-real-escape-string.html )
 * mysql�ǥ��������פ�����\�ס�'�ס�"�ס�\0�ס�\n�ס�\r�ס�\z�פ��⡢
 * �Ķ��ѿ��˴ޤ����ΤǤ��ʤ���\0�פȡ�
 * ��windows�Ķ�������Ȥ���٤ˡ�\z�פ��������
 * ��\�ס�'�ס�"�ס�\n�ס�\r�פθޤĤΤߥ��������פ�Ԥ����ͤȤ��롣
 * �ġĤ�����ǡ�\x80-\xff�Υ��������פ�ɬ�פˤʤä��١�
 * ���ҤθޤĤ��ɲäǡ����Υ��������פ�Ԥ���
 */


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



#define CONVERT_4BITNUM_TO_HEXCHAR(num) \
  if (10 <= num) { num += ('a' - 10); } else { num += '0'; }




/* src_fh�����������ɤ߼�ꡢdst_fh�˽񤭽Ф���
 * �֤��ͤϡ��ޤ�src_fh�������Ƥ���(=�ޤ��ǡ���������ǽ��������)�ʤ�1��
 * �⤦src_fh�����Ǥ���(=read��EOF���֤���)�ʤ�0��
 * dst_fh�����Ǥ����ꡢ���餫�Υ��顼��ȯ��������-1���֤���
 * ����ɬ��������select��src_fh���ɤ߼���ǽ�ʾ��֤Ǥ�������ǧ�������
 * ����src_fh������(=read��EOF���֤���)�顢�����dst_fh�ˤ����Ť�����٤�
 * �����dst_fh��close(�ȡ����줬socket�ʤ�shutdown��)����ɬ�פ����������ա�
 * ����ϡ�stream_transfer�γ�¦�ǹԤ���(socket���ɤ�����Ƚ�̤ΰ�)��
 * �����ɤ߹����ǡ�����ESTPC_BUF_SIZE����¿���ä����ϡ�
 * ESTPC_BUF_SIZE�ʾ�Υǡ����Ͻ�����������롣
 * (�����Ƽ���select�Ǥ����ˤޤ�stream_transfer���ƤӽФ������ˤʤ�Ȧ)
 * ����ϡ�ESTPC_BUF_SIZE���٤Υǡ������褿����Ƚ�꤬�񤷤����ؤ��к��Ȥ��ơ�
 * ���Τ褦�ʼ����ˤ��Ƥ��롣 */
int stream_transfer (FILE *src_fh, FILE *dst_fh) {
  char _buf[ESTPC_BUF_SIZE];
  char *buf_ptr = _buf;
  size_t read_len = fread(buf_ptr, sizeof(char), ESTPC_BUF_SIZE, src_fh);
  if (read_len < 0) return read_len; /* src_fh�����ǡ��ޤ����ɤ߹��ߥ��顼 */
  if (read_len == 0) {
    /* 1byte���ɤ߹���ʤ��ä����ϡ����줬eof�ʤΤ�Ĵ�٤�ɬ�פ����� */
    return feof(src_fh) ? 0 : 1;
  }
  /* buf��dst_fh�˽񤭽Ф� */
  while (1) {
    size_t wrote_len = fwrite(buf_ptr, sizeof(char), read_len, dst_fh);
    if (read_len == wrote_len) {
      /* ����˽񤭹��߽�λ */
      fflush(dst_fh); /* �ޤ�˽񤭽Ф�ɬ�פ����� */
      break;
    }
    /* ������񤭽Ф������Ǥ��ʤ��ä���
     * ������Ĵ�١�������н褹��ɬ�פ����롣 */
    if (feof(dst_fh)) {
      /* dst_fh���Ĥ���줿�� */
      return -1;
    }
    if (ferror(dst_fh)) {
      /* dst_fh�ǥ��顼ȯ���� */
      return -1;
    }
    /* ñ�ˡ��Хåե��δط��ǡ�������񤭹��ߤ���ʤ��ä��������ä���
     * ���١��񤭹��ߤ�Ԥ���
     * (����Ȥ⡢����sleep�������������Τ�������) */
    fflush(dst_fh); /* �Ȥꤢ�����������ޤǤ�flush */
    /* �ͤ��������Ƥ����ȥ饤 */
    buf_ptr = &buf_ptr[wrote_len];
    read_len -= wrote_len;
  }
  /* ����˽�λ�������Τߡ���������� */
  return feof(src_fh) ? 0 : 1;
}


void interconnect_stream (FILE *sock_in, FILE *sock_out, int sock_fd) {
  /* ����Ū�ˤϡ��ʲ��ε�ư�򤹤���ɤ��Ȼפ��롣
   * - select��Ȥäơ�stdin(STDIN_FILENO)�ȥ����åȤΤɤ��餫����
   *   �ǡ������ɤ߼�����֤ˤʤ�Τ��Ԥġ�
   * -- stdin(STDIN_FILENO)���ɤ߽Ф�ok�ˤʤä��顢
   *    �ɤ߽Ф���¤��ɤ߽Ф��ơ�����򥽥��åȤ˽񤭽Ф���
   * -- �����åȤ��ɤ߽Ф�ok�ˤʤä��顢
   *    �ɤ߽Ф���¤��ɤ߽Ф��ơ������stdout�˽񤭽Ф���
   * -- �ɤ��餫��EOF�ޤ��ϲ�����Υ��顼���褿�顢����˱�������λ������Ԥ�
   */

  /* sigmask����������
   * �ġĤ�������Ȥ��ơ�ͽ��SIGPIPE��̵�뤷�Ƥ��롢�Ȥ������ˤ��롣
   * �����ǤϾ�ά���롣 */
  /* SIGPIPE��̵�뤹����ˤĤ��Ƥϰʲ��򻲾�
   * http://www.kt.rim.or.jp/~ksk/sock-faq/unix-socket-faq-ja-2.html#ss2.19 */


  /* stdin��EOF�ϥե饰��������ɬ�פ����� */
  char stdin_is_eof = 0;

  while (1) {
    /* nfds���������� */
    int nfds;
    if (stdin_is_eof) {
      nfds = sock_fd + 1;
    }
    else {
      nfds = ((STDIN_FILENO < sock_fd) ? sock_fd : STDIN_FILENO) + 1;
    }

    /* fd_set���������� */
    fd_set readfds;
    FD_ZERO(&readfds);
    if (!stdin_is_eof) FD_SET(STDIN_FILENO, &readfds);
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
    if (!FD_ISSET(sock_fd, &readfds)
        && (stdin_is_eof || !FD_ISSET(STDIN_FILENO, &readfds))) {
      /* ���顼�Ǥ�ʤ������ɤ߼���ǽ�ˤ�ʤäƤʤ������ʤ顢
       * �����ॢ���Ȥ�����Τȹͤ����롣
       * (����ʤ�continue����Ȥä�ʬ����䤹����¤�ˤ���������
       *  �����åȤ�stdin��Ʊ�����ɤ߼���ǽ�ˤʤä����ˡ�
       *  �������Ф餻�����ʤ��Τǡ�
       *  ����select��ξ���Ȥ������ǽ�ˤ���٤ˡ�
       *  ����ʤ��ä�������¤�ˤʤäƤ��ޤä�)
       * �������ν����ϡ�����stdin��ǧ������˥����å�����ɬ�פ����롣
       * (stdin��ǧ��������stdin_is_eof�ե饰���ѹ����Ƥ����) */
      fprintf(stderr, "socket response timeout\n");
      break;
    }
    if (FD_ISSET(sock_fd, &readfds)) {
      /* �����åȤ����ɤ߼���ǽ */
      /* Ŭ�����ɤ߼�ꡢ�ɤ߼�ä�ʬ��stdout�˽񤭽Ф� */
      int r = stream_transfer(sock_in, stdout);
      if (r <= 0) {
        /* socket��EOF�ˤʤä��顢stdout��close��Ԥ�
         * (�ºݤˤ����פ�����������Ƥ���) */
        fclose(stdout);
        break; /* �����åȤ�EOF�ˤʤä���¨��λ */
      }
    }
    if (!stdin_is_eof && FD_ISSET(STDIN_FILENO, &readfds)) {
      /* stdin�����ɤ߼���ǽ */
      /* Ŭ�����ɤ߼�ꡢ�ɤ߼�ä�ʬ�򥽥��åȤ˽񤭽Ф� */
      int r = stream_transfer(stdin, sock_out);
      if (r <= 0) {
        /* stdin��EOF�ˤʤä��顢sock_out��shutdown��Ԥ�
         * (close�����read�����ޤ�close����Ƥ��ޤ��Τǡ�close�Ϥ��ʤ�) */
        fflush(sock_out);
        shutdown(sock_fd, SHUT_WR);
        stdin_is_eof = 1; /* stdin��EOF�ˤʤäƤ�¨��λ���ʤ� */
      }
    }
  }
  return;
}




struct SizeAndFlag {
  size_t size;
  char is_ascii;
};

typedef struct _KeyAndVal {
  char *key;
  char *val;
  char key_is_ascii;
  char val_is_ascii;
} KeyAndVal;

struct SizeAndFlag saf_check (char *str, size_t len) {
  struct SizeAndFlag saf;
  saf.size = 0;
  saf.is_ascii = 1;
  int i;
  for (i = 0; i <= len; i++) {
    unsigned char c = (unsigned char)str[i];
    /* �����Ǥ�saf.size����ʬ�ˤĤ��Ƥ�saf_escaped_strncpy�γ����ս�򻲹� */
    if ((c == '\\') || (c == '\'') || (c == '"')) {
      saf.size += 2;
    }
    else if (c == '\n') {
      saf.size += 2;
    }
    else if (c == '\r') {
      saf.size += 2;
    }
    else if (128 <= c) {
      saf.size += 4;
      saf.is_ascii = 0;
    }
    else {
      saf.size += 1;
    }
  }
  return saf;
}

char *saf_escaped_strncpy (char *dst, const char *src, size_t src_size,
    size_t dst_size) {
  int src_i = 0;
  int dst_i = 0;
  if (dst_size == 0) goto RETURN;
  if (src_size == 0) goto FINALIZE;
  while (1) {
    unsigned char c = (unsigned char)src[src_i];
    if ((c == '\\') || (c == '\'') || (c == '"')) {
      dst[dst_i++] = '\\';
      if (dst_size <= dst_i) break;
      dst[dst_i++] = (char)c;
      if (dst_size <= dst_i) break;
    }
    else if (c == '\n') {
      dst[dst_i++] = '\\';
      if (dst_size <= dst_i) break;
      dst[dst_i++] = 'n';
      if (dst_size <= dst_i) break;
    }
    else if (c == '\r') {
      dst[dst_i++] = '\\';
      if (dst_size <= dst_i) break;
      dst[dst_i++] = 'r';
      if (dst_size <= dst_i) break;
    }
    else if (128 <= c) {
      char upper_char = (char)(c / 16);
      CONVERT_4BITNUM_TO_HEXCHAR(upper_char);
      char lower_char = (char)(c % 16);
      CONVERT_4BITNUM_TO_HEXCHAR(lower_char);
      dst[dst_i++] = '\\';
      if (dst_size <= dst_i) break;
      dst[dst_i++] = 'x';
      if (dst_size <= dst_i) break;
      dst[dst_i++] = upper_char;
      if (dst_size <= dst_i) break;
      dst[dst_i++] = lower_char;
      if (dst_size <= dst_i) break;
    }
    else {
      dst[dst_i++] = (char)c;
      if (dst_size <= dst_i) break;
    }

    src_i++;
    if (src_size <= src_i) break;
  }
FINALIZE:
  while (1) {
    dst[dst_i++] = 0;
    if (dst_size <= dst_i) break;
  }
RETURN:
  return dst;
}

/* ToDo: malloc�Ϥ��֤��ͤΥ����å���ɬ�� */
KeyAndVal *env2key_and_val (char *env_line) {
  KeyAndVal *kav = (KeyAndVal *)malloc(sizeof(KeyAndVal));
  kav->key_is_ascii = 1;
  kav->val_is_ascii = 1;

  size_t all_len = strlen(env_line);
  char *equal_ptr = strchr(env_line, '=');
  char *val_ptr = (equal_ptr == NULL) ? &env_line[all_len] : &equal_ptr[1];
  size_t key_len = (size_t)equal_ptr - (size_t)env_line;
  size_t val_len = strlen(val_ptr);

  struct SizeAndFlag saf_key = saf_check(env_line, key_len);
  struct SizeAndFlag saf_val = saf_check(val_ptr, val_len);
  kav->key_is_ascii = saf_key.is_ascii;
  kav->val_is_ascii = saf_key.is_ascii;
  kav->key = (char *)malloc(saf_key.size + 1);
  kav->key[saf_key.size] = 0; /* for safety */
  kav->val = (char *)malloc(saf_val.size + 1);
  kav->val[saf_val.size] = 0; /* for safety */
  saf_escaped_strncpy(kav->key, env_line, key_len, saf_key.size);
  saf_escaped_strncpy(kav->val, val_ptr, val_len, saf_val.size);

  return kav;
}

void free_key_and_val (KeyAndVal *kav) {
  if (kav->key != NULL) free(kav->key);
  if (kav->val != NULL) free(kav->val);
  free(kav);
  return;
}

void write_sexp_env (FILE *sock_out, char **env) {
  int cnt = -1;

  fputs(PAREN_ALL_BEGIN, sock_out);

  while (1) {
    cnt++;
    char *e = env[cnt];
    if (e == NULL) break;

    /* ����ܰʹߤΥ���ȥ�ϡ����Ϥ������˶��������� */
    if (cnt) fputs(BETWEEN_PAIR, sock_out);

    /* env����hoge=fuge\0������ʸ������ɤ߼�ꡢ
     * key��val��ʬ�䤷�������ʸ���򥨥������פ���
     * �Դ���ʸ����Ǥ���ʤ��Դ���ʸ����ν񼰤Ȥ���
     */
    KeyAndVal *key_and_val = env2key_and_val(env[cnt]);

    /* key_and_val����Ϥ��� */
    fputs(PAREN_KEY_AND_VAL_BEGIN, sock_out);
    fputs(
        (key_and_val->key_is_ascii ? ASCII_QUOT_BEGIN : NONASCII_QUOT_BEGIN),
        sock_out);
    if (key_and_val->key != NULL) fputs(key_and_val->key, sock_out);
    fputs(
        (key_and_val->key_is_ascii ? ASCII_QUOT_END : NONASCII_QUOT_END),
        sock_out);
    fputs(BETWEEN_KEY_AND_VAL, sock_out);
    fputs(
        (key_and_val->val_is_ascii ? ASCII_QUOT_BEGIN : NONASCII_QUOT_BEGIN),
        sock_out);
    if (key_and_val->val != NULL) fputs(key_and_val->val, sock_out);
    fputs(
        (key_and_val->val_is_ascii ? ASCII_QUOT_END : NONASCII_QUOT_END),
        sock_out);
    fputs(PAREN_KEY_AND_VAL_END, sock_out);

    free_key_and_val(key_and_val);
  }

  fputs(PAREN_ALL_END, sock_out);

  fflush(sock_out);
  return;
}


void write_sexp_argv (FILE *sock_out, int argc, char *argv[]) {
  int i;
  fputs(BETWEEN_KEY_AND_VAL, sock_out); /* env�Ȥδ֤ζ��ڤ���� */
  fputs(PAREN_ALL_BEGIN, sock_out);
  for (i = 0; i < argc; i++) {
    /* ����ܰʹߤΥ���ȥ�ϡ����Ϥ������˶��������� */
    if (i) fputs(BETWEEN_KEY_AND_VAL, sock_out);

    size_t len = strlen(argv[i]);
    struct SizeAndFlag saf = saf_check(argv[i], len);
    char *buf = (char *)malloc(saf.size + 1);
    buf[saf.size] = 0; /* for safety */
    saf_escaped_strncpy(buf, argv[i], len, saf.size);
    if (saf.is_ascii) {
      fputs(ASCII_QUOT_BEGIN, sock_out);
      fputs(buf, sock_out);
      fputs(ASCII_QUOT_END, sock_out);
    }
    else {
      fputs(NONASCII_QUOT_BEGIN, sock_out);
      fputs(buf, sock_out);
      fputs(NONASCII_QUOT_END, sock_out);
    }
    free(buf);
  }
  fputs(PAREN_ALL_END, sock_out);
  fflush(sock_out);
  return;
}




int estp02_comm (char *sock_path, int argc, char *argv[], char **env) {
  /* �����åȤ���³���� */
  int sock_fd = socket(PF_UNIX, SOCK_STREAM, 0);
  if (sock_fd < 0) return ERROR_FAILED_TO_CONNECT_SOCKET;

  struct sockaddr_un uds;

  uds.sun_family = AF_UNIX;
  strcpy(uds.sun_path, sock_path);
  int addrlen = sizeof(struct sockaddr_un);

  int r = connect(sock_fd, (struct sockaddr*)&uds, addrlen);
  if (r < 0) return ERROR_FAILED_TO_CONNECT_SOCKET;

  FILE *sock_in  = fdopen(sock_fd, "r");
  FILE *sock_out = fdopen(sock_fd, "w");

  /* �ޤ�greeting������ */
  fprintf(sock_out, "ESTP/0.2\r\n");
  /* �Ķ��ѿ���S�������� */
  write_sexp_env(sock_out, env);
  /* argv��S�������� */
  write_sexp_argv(sock_out, argc, argv);

  /* ���ȥ꡼��ž���������ơ��֥�å��⡼�ɤ��ѹ����� */
  fcntl(STDIN_FILENO, F_SETFL, fcntl(STDIN_FILENO, F_GETFL) | O_NONBLOCK);
  /* ����socket��Υ�֥�å��󥰤ˤ��Ƥ��������������fwrite()����
   * ����᤮���ΤǤ��ʤ��褦�ʤΤǡ�socket�����ϡ�
   * �Υ�֥�å��󥰥⡼�ɤˤϤ��ʤ����ˤ�����
   */

  /* stdin/stdout�ȥ����åȤ��ӤĤ��� */
  interconnect_stream(sock_in, sock_out, sock_fd);

  /* ��λ������Ԥ� */
  fflush(sock_out);
  shutdown(sock_fd, SHUT_RDWR);
  close(sock_fd);
  fflush(stdout);

  return 0;
}

/* vim:set ft=c sw=2 ts=2 sts=2 et: */
