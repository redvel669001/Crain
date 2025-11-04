#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef enum {
  MAIN,
  QUINE,
  TEST_CASE,
  TEST_NAMES,
} TestName;

const char *tests[TEST_NAMES] = {
  "main",
  "quine",
  "test_case",
};

const char *build = "./build/";
size_t build_len;

const char *src = "./tests/";
size_t src_len;

const char *compiler = "./build/bf";
size_t c_len;

char *cmd = NULL;
size_t len = 0;

void compile_test(const char *test);
void run_test(const char *test);

int main(void) {
  build_len = strlen(build) + 1;
  src_len = strlen(src) + 1;
  c_len = strlen(compiler) + 1;
  for (size_t i = 0; i < TEST_NAMES; i++) {
    compile_test(tests[i]);
    system(cmd);
    run_test(tests[i]);
    system(cmd);
    printf("\n\n");
  }
  
  return 0;
}

void compile_test(const char *test) {
  size_t test_len = strlen(test) + 1;
  len = c_len + src_len + test_len + build_len + test_len + 12;
  cmd = malloc(len);
  snprintf(cmd, len, "%s %s%s.bf -n -c -o %s%s", compiler, src, test, build, test);
  printf("[INFO] %s\n", cmd);
}

void run_test(const char *test) {
  len = build_len + strlen(test);
  cmd = malloc(len);
  snprintf(cmd, len, "%s%s", build, test);
  printf("[INFO] %s\n", cmd);
}

