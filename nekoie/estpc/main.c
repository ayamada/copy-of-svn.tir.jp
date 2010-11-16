/* $Id$ */

/* ToDo: ソケット接続失敗時には、マクロで指定されたコマンドを実行するように
 *       直す(リダイレクト出力は、その特殊形として扱う) */

/* readすると、cgi-metavariablesに直接parameterizeできるalistが得られる。
 * その後は通常通り、STDINとSTDOUTとして扱える。 */
/* WARN: 但し、メタ変数のkeyまたはvalにasciiの範囲外のバイト値が存在する場合、
 * そのkeyまたはvalの文字列は不完全文字列として送信される。
 * 大抵の場合、このままでは問題となる為、
 * string-incomplete->completeにかける必要があり、
 * デフォルトのces以外のエンコーディングでメタ変数が送られてくる可能性が
 * ある場合は、更に、それを標準cesへと変換も行う必要がある。
 */

/* mysqlのcuiクライアントの""内では、\\、\n、\0、\"の四つのエスケープが
 * 最小限エスケープが必要なものとして扱われている。
 * 今回はそれをベースとし(gaucheでもこの四つがエスケープされていればokとする)、
 * 操作する文字列はenvironなので、ここでは\0以外の三つを
 * エスケープするようにした。
 * ……が、マルチバイト文字は不完全文字列として扱わなくてはいけない事に、
 * あとで気付いたので、上記の他に、\x80-\xffまでのバイト値は、
 * \x表記に変換するようにした。
 * \\や\nについては前述のままとする。 */

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



/* socketへの接続に失敗した場合、このurlへとリダイレクトする出力を返す */
#ifndef SOCKET_ERROR_LOCATION_URL
#define SOCKET_ERROR_LOCATION_URL "http://d.tir.jp/path/to/connect_error.html"
#endif

/* 接続すべきsocketのpath */
#ifndef SOCKET_PATH
#define SOCKET_PATH "/path/to/socket.sock"
#endif

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


/* ToDo: バッファリングを実装して高速化する必要がある */
ssize_t putfd (char c, int fd) {
  return write(fd, &c, 1);
}

/* ToDo: バッファリングを実装して高速化する必要がある */
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

/* 文字のエスケープは、mysqlをベースにし、
 * ( http://dev.mysql.com/doc/refman/4.1/ja/mysql-real-escape-string.html )
 * mysqlでエスケープされる「\」「'」「"」「\0」「\n」「\r」「\z」の内、
 * 環境変数に含める事のできない「\0」と、
 * 非windows環境を前提とする為に「\z」を除いた、
 * 「\」「'」「"」「\n」「\r」の五つのみエスケープを行う仕様とする。
 * ……が、後で、\x80-\xffのエスケープも必要になった為、
 * 前述の五つに追加で、このエスケープも行う。
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

    /* 二個目以降のエントリは、出力する前に空白を入れる */
    if (cnt) printfd(BETWEEN_PAIR, fd);

    int i = 0;
    int name = 1;

    /* ToDo: まず、この段階で一旦keyとvalを分割し、
     *       (インデクス値と終端だけ保持すればok)
     *       それぞれについて、asciiのみで構成されているかどうかを調べ、
     *       非asciiとasciiの区別を付ける。
     *       ここでの区分の目的は、gaucheの文字列が完全か不完全かを
     *       調べる為だけなので、asciiかどうかの区分は、
     *       構成する文字が0-127の範囲内かどうかだけを基準とする。
     *       (gaucheは、7bit asciiの範囲なら常に完全な文字列として扱うようだ)
     */
    printfd(PAREN_KEY_AND_VAL_BEGIN, fd);
    printfd(NONASCII_QUOT_BEGIN, fd);
    /* 一文字ずつ処理する */
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

  fsync(fd); /* flushする */
  return;
}


void redirect_to_errorpage (void) {
  printf("Location: %s\r\n", SOCKET_ERROR_LOCATION_URL);
  printf("\r\n");
  fflush(stdout);
  return;
}

/* src_fdから一定数を読み取り、dst_fdに書き出す。
 * 返り値は、まだsrc_fdが生きている(=まだデータがある)なら1を、
 * もうsrc_fdが死んでいる(=readが0を返した)なら0を、
 * 何らかのエラーが発生したら-1を返す。
 * 尚、必ず事前にselectでsrc_fdが読み取り可能な状態である事を確認する事。 */
int stream_transfer (int src_fd, int dst_fd) {
  char _buf[ESTPC_BUF_SIZE];
  char *buf_ptr = _buf;
  ssize_t read_len = read(src_fd, buf_ptr, ESTPC_BUF_SIZE);
  if (read_len <= 0) return read_len; /* src_fdは切断、または読み込みエラー */
  /* bufをdst_fdに書き出す */
  while (1) {
    ssize_t write_len = write(dst_fd, buf_ptr, read_len);
    if (write_len < 0) return write_len; /* 書き込みエラー */
    if (read_len == write_len) {
      /* 正常に書き込み終了 */
      fsync(dst_fd); /* fflush代わり */
      break;
    }
    /* 残りの部分を再書き込みしなくてはならない */
    buf_ptr += write_len;
    read_len -= write_len;
  }
  return 1;
}

void interconnect_stream (int sock_fd) {
  /* 具体的には、以下の挙動をすれば良いと思われる。
   * - selectを使って、stdin(STDIN_FILENO)とソケットのどちらかから
   *   データが読み取れる状態になるのを待つ。
   * -- stdin(STDIN_FILENO)が読み出しokになったら、
   *    読み出せる限り読み出して、それをソケットに書き出す。
   * -- ソケットが読み出しokになったら、
   *    読み出せる限り読み出して、それをstdoutに書き出す。
   * -- どちらかがEOFまたは何からのエラーが来たら、それに応じた終了処理を行う
   */

  /* nfdsを生成する */
  int nfds = ((STDIN_FILENO < sock_fd) ? sock_fd : STDIN_FILENO) + 1;

  /* sigmaskを生成する */
  sigset_t empty_sigset;
  sigset_t mask;
  sigemptyset(&empty_sigset); /* 空集合を生成する */
  sigprocmask(SIG_BLOCK, &empty_sigset, &mask); /* 現在のマスク状態を取得 */
  sigaddset(&mask, SIGPIPE); /* 現在のマスクのコピーにSIGPIPEを追加 */
  /* SIGPIPEを無視する事については以下を参照
   * http://www.kt.rim.or.jp/~ksk/sock-faq/unix-socket-faq-ja-2.html#ss2.19 */

  while (1) {
    /* fd_setを生成する(毎回破壊される) */
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);
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
    else if (FD_ISSET(STDIN_FILENO, &readfds)) {
      /* stdinから読み取り可能 */
      /* 適当に読み取り、読み取った分をソケットに書き出す */
      int r = stream_transfer(STDIN_FILENO, sock_fd);
      if (r <= 0) { break; } else { continue; }
    }
    else if (FD_ISSET(sock_fd, &readfds)) {
      /* ソケットから読み取り可能 */
      /* 適当に読み取り、読み取った分をstdoutに書き出す */
      int r = stream_transfer(sock_fd, STDOUT_FILENO);
      if (r <= 0) { break; } else { continue; }
    }
    else {
      /* タイムアウトしたものと考えられる。 */
      fprintf(stderr, "socket response timeout\n");
      break;
    }
    /* NOTREACHED */
  }
  return;
}

void failed_to_connect_socket (void) {
  /* ToDo: あとでコマンド実行対応する事 */
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
  /* ソケットに接続する */
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

  /* まず最初に、SIGPIPEを禁止する */
  /* SIGPIPEを無視する事については以下を参照
   * http://www.kt.rim.or.jp/~ksk/sock-faq/unix-socket-faq-ja-2.html#ss2.19 */
  disable_sigmask();

  /* ソケットに環境変数を送る(estp/0.0) */
  write_sexp_env(sock_fd);

  /* stdin/stdoutとソケットを結びつける */
  interconnect_stream(sock_fd);

  /* 終了処理を行う */
  shutdown(sock_fd, 2);
  close(sock_fd);
  /* flushするという意味で、stdin等もcloseした方が良いっぽいようだ。
   * (speedycgiはそうしていた) */
  close(STDIN_FILENO);
  close(STDOUT_FILENO);
  close(STDERR_FILENO);

  return 0;
}

/* vim:set ft=c sw=2 ts=2 sts=2 et: */
