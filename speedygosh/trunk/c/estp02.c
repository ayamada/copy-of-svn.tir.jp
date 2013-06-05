/* $Id$ */

#include "estp02.h"

/* 文字のエスケープは、mysqlをベースにし、
 * ( http://dev.mysql.com/doc/refman/4.1/ja/mysql-real-escape-string.html )
 * mysqlでエスケープされる「\」「'」「"」「\0」「\n」「\r」「\z」の内、
 * 環境変数に含める事のできない「\0」と、
 * 非windows環境を前提とする為に「\z」を除いた、
 * 「\」「'」「"」「\n」「\r」の五つのみエスケープを行う仕様とする。
 * ……が、後で、\x80-\xffのエスケープも必要になった為、
 * 前述の五つに追加で、このエスケープも行う。
 */


/* 全体を囲む括弧文字列の定義 */
#ifndef PAREN_ALL_BEGIN
#define PAREN_ALL_BEGIN "("
#endif
#ifndef PAREN_ALL_END
#define PAREN_ALL_END ")"
#endif
/* keyとvalを囲む括弧文字列の定義 */
#ifndef PAREN_KEY_AND_VAL_BEGIN
#define PAREN_KEY_AND_VAL_BEGIN "("
#endif
#ifndef PAREN_KEY_AND_VAL_END
#define PAREN_KEY_AND_VAL_END ")"
#endif

/* keyとvalの間の文字列の定義 */
#ifndef BETWEEN_KEY_AND_VAL
#define BETWEEN_KEY_AND_VAL " "
#endif

/* keyとvalのpairの間の文字列の定義 */
#ifndef BETWEEN_PAIR
#define BETWEEN_PAIR " "
#endif

/* ascii文字列のクォーテーション文字列の定義 */
#ifndef ASCII_QUOT_BEGIN
#define ASCII_QUOT_BEGIN "\""
#endif
#ifndef ASCII_QUOT_END
#define ASCII_QUOT_END "\""
#endif

/* 非ascii文字列(不完全文字列)のクォーテーション文字列の定義 */
#ifndef NONASCII_QUOT_BEGIN
#define NONASCII_QUOT_BEGIN "#*\""
#endif
#ifndef NONASCII_QUOT_END
#define NONASCII_QUOT_END "\""
#endif



/* タイムアウト待ち秒数 */
#ifndef SELECT_TIMEOUT
#define SELECT_TIMEOUT 60
#endif


#define ESTPC_BUF_SIZE 4096



#define CONVERT_4BITNUM_TO_HEXCHAR(num) \
  if (10 <= num) { num += ('a' - 10); } else { num += '0'; }




/* src_fhから一定数を読み取り、dst_fhに書き出す。
 * 返り値は、まだsrc_fhが生きている(=まだデータが来る可能性がある)なら1を、
 * もうsrc_fhが死んでいる(=readがEOFを返した)なら0を、
 * dst_fhが死んでいたり、何らかのエラーが発生したら-1を返す。
 * 尚、必ず事前にselectでsrc_fhが読み取り可能な状態である事を確認する事。
 * 尚、src_fhが死んだ(=readがEOFを返した)ら、それをdst_fhにも伝播させる為に
 * 勝手にdst_fhをclose(と、それがsocketならshutdownも)する必要がある事に注意。
 * これは、stream_transferの外側で行う事(socketかどうかの判別の為)。
 * 尚、読み込めるデータがESTPC_BUF_SIZEよりも多かった場合は、
 * ESTPC_BUF_SIZE以上のデータは処理せずに戻る。
 * (そして次のselectですぐにまたstream_transferが呼び出される事になる筈)
 * これは、ESTPC_BUF_SIZE丁度のデータが来た時に判定が難しい事への対策として、
 * このような実装にしてある。 */
int stream_transfer (FILE *src_fh, FILE *dst_fh) {
  char _buf[ESTPC_BUF_SIZE];
  char *buf_ptr = _buf;
  size_t read_len = fread(buf_ptr, sizeof(char), ESTPC_BUF_SIZE, src_fh);
  if (read_len < 0) return read_len; /* src_fhは切断、または読み込みエラー */
  if (read_len == 0) {
    /* 1byteも読み込めなかった時は、それがeofなのか調べる必要がある */
    return feof(src_fh) ? 0 : 1;
  }
  /* bufをdst_fhに書き出す */
  while (1) {
    size_t wrote_len = fwrite(buf_ptr, sizeof(char), read_len, dst_fh);
    if (read_len == wrote_len) {
      /* 正常に書き込み終了 */
      fflush(dst_fh); /* まめに書き出す必要がある */
      break;
    }
    /* 全部を書き出す事ができなかった。
     * 原因を調べ、それに対処する必要がある。 */
    if (feof(dst_fh)) {
      /* dst_fhは閉じられた。 */
      return -1;
    }
    if (ferror(dst_fh)) {
      /* dst_fhでエラー発生。 */
      return -1;
    }
    /* 単に、バッファの関係で、全部を書き込みきれなかっただけだった。
     * 再度、書き込みを行う。
     * (それとも、少しsleepした方がいいのか？？？) */
    fflush(dst_fh); /* とりあえず、ここまでをflush */
    /* 値を再定義してからリトライ */
    buf_ptr = &buf_ptr[wrote_len];
    read_len -= wrote_len;
  }
  /* 正常に終了した時のみ、ここに来る */
  return feof(src_fh) ? 0 : 1;
}


void interconnect_stream (FILE *sock_in, FILE *sock_out, int sock_fd) {
  /* 具体的には、以下の挙動をすれば良いと思われる。
   * - selectを使って、stdin(STDIN_FILENO)とソケットのどちらかから
   *   データが読み取れる状態になるのを待つ。
   * -- stdin(STDIN_FILENO)が読み出しokになったら、
   *    読み出せる限り読み出して、それをソケットに書き出す。
   * -- ソケットが読み出しokになったら、
   *    読み出せる限り読み出して、それをstdoutに書き出す。
   * -- どちらかがEOFまたは何からのエラーが来たら、それに応じた終了処理を行う
   */

  /* sigmaskを生成する
   * ……が、前提として、予めSIGPIPEは無視してある、という事にする。
   * ここでは省略する。 */
  /* SIGPIPEを無視する事については以下を参照
   * http://www.kt.rim.or.jp/~ksk/sock-faq/unix-socket-faq-ja-2.html#ss2.19 */


  /* stdinのEOFはフラグ管理する必要がある */
  char stdin_is_eof = 0;

  while (1) {
    /* nfdsを生成する */
    int nfds;
    if (stdin_is_eof) {
      nfds = sock_fd + 1;
    }
    else {
      nfds = ((STDIN_FILENO < sock_fd) ? sock_fd : STDIN_FILENO) + 1;
    }

    /* fd_setを生成する */
    fd_set readfds;
    FD_ZERO(&readfds);
    if (!stdin_is_eof) FD_SET(STDIN_FILENO, &readfds);
    FD_SET(sock_fd, &readfds);

    /* timevalを生成する(man 2 selectによると、
     * select時にtimeoutが破壊される環境があるらしいので毎回生成する) */
    struct timeval timeout;
    timeout.tv_sec = SELECT_TIMEOUT; /* 秒 */
    timeout.tv_usec = 0; /* マイクロ秒 */

    /* selectを実行する */
    int r = select(nfds, &readfds, NULL, NULL, &timeout);
    if (r == -1) {
      if (errno == EINTR) {
        /* シグナルを受信した */
        /* とりあえず終了でいいと思う */
        perror("caught a signal");
        break;
      }
      else {
        /* よく分からないエラー */
        /* エラー内容をstderrに書き出す必要がある */
        perror("unknown error in select");
        break;
      }
    }
    if (!FD_ISSET(sock_fd, &readfds)
        && (stdin_is_eof || !FD_ISSET(STDIN_FILENO, &readfds))) {
      /* エラーでもないし、読み取り可能にもなってない状況なら、
       * タイムアウトしたものと考えられる。
       * (本来ならcontinue等を使って分かりやすい構造にしたいが、
       *  ソケットとstdinが同時の読み取り可能になった時に、
       *  片方に偏らせたくないので、
       *  一回のselectで両方とも処理可能にする為に、
       *  こんなややっこしい構造になってしまった)
       * 尚、この処理は、下のstdin確認よりも先にチェックする必要がある。
       * (stdin確認の内部でstdin_is_eofフラグを変更している為) */
      fprintf(stderr, "socket response timeout\n");
      break;
    }
    if (FD_ISSET(sock_fd, &readfds)) {
      /* ソケットから読み取り可能 */
      /* 適当に読み取り、読み取った分をstdoutに書き出す */
      int r = stream_transfer(sock_in, stdout);
      if (r <= 0) {
        /* socketがEOFになったら、stdoutのcloseを行う
         * (実際には不要だが、一応しておく) */
        fclose(stdout);
        break; /* ソケットがEOFになったら即終了 */
      }
    }
    if (!stdin_is_eof && FD_ISSET(STDIN_FILENO, &readfds)) {
      /* stdinから読み取り可能 */
      /* 適当に読み取り、読み取った分をソケットに書き出す */
      int r = stream_transfer(stdin, sock_out);
      if (r <= 0) {
        /* stdinがEOFになったら、sock_outのshutdownを行う
         * (closeするとreadの方までcloseされてしまうので、closeはしない) */
        fflush(sock_out);
        shutdown(sock_fd, SHUT_WR);
        stdin_is_eof = 1; /* stdinがEOFになっても即終了しない */
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
    /* ここでのsaf.sizeの増分についてはsaf_escaped_strncpyの該当箇所を参考 */
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

/* ToDo: malloc系の返り値のチェックが必要 */
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

    /* 二個目以降のエントリは、出力する前に空白を入れる */
    if (cnt) fputs(BETWEEN_PAIR, sock_out);

    /* envからhoge=fuge\0形式の文字列を読み取り、
     * keyとvalに分割し、特定の文字をエスケープし、
     * 不完全文字列であるなら不完全文字列の書式とする
     */
    KeyAndVal *key_and_val = env2key_and_val(env[cnt]);

    /* key_and_valを出力する */
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
  fputs(BETWEEN_KEY_AND_VAL, sock_out); /* envとの間の区切り空白 */
  fputs(PAREN_ALL_BEGIN, sock_out);
  for (i = 0; i < argc; i++) {
    /* 二個目以降のエントリは、出力する前に空白を入れる */
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
  /* ソケットに接続する */
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

  /* まずgreetingを送信 */
  fprintf(sock_out, "ESTP/0.2\r\n");
  /* 環境変数をS式で送る */
  write_sexp_env(sock_out, env);
  /* argvをS式で送る */
  write_sexp_argv(sock_out, argc, argv);

  /* ストリーム転送に備えて、ブロックモードを変更する */
  fcntl(STDIN_FILENO, F_SETFL, fcntl(STDIN_FILENO, F_GETFL) | O_NONBLOCK);
  /* 元はsocketもノンブロッキングにしていたが、それだとfwrite()時に
   * 送り過ぎを検知できないようなので、socketの方は、
   * ノンブロッキングモードにはしない事にした。
   */

  /* stdin/stdoutとソケットを結びつける */
  interconnect_stream(sock_in, sock_out, sock_fd);

  /* 終了処理を行う */
  fflush(sock_out);
  shutdown(sock_fd, SHUT_RDWR);
  close(sock_fd);
  fflush(stdout);

  return 0;
}

/* vim:set ft=c sw=2 ts=2 sts=2 et: */
