#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef enum {
  MAIN,
  QUINE,
  TEST_CASE,
  /* BF_TO_C, */
  HELLO_WORLD,
  TEST_NAMES,
} TestName;

const char *tests[TEST_NAMES] = {
  "main",
  "quine",
  "test_case",
  /* "bf_to_c", */
  "hello_world",
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

bool opt = false;
bool compile = true;
bool noisy = false; // Comments in assembly.
bool verbose = false; // Extra comments in assembly.
bool extra_verbose = false; // Extra comments in assembly.
bool readable = false; // Tabs for readability.
bool run = true; // flag for running the file after compiling.
bool dump_asm = false; // flag for dumping the assembly without compiling.
bool simulate = false;

int main(int argc, char **argv) {
  /* argc--; argv++; */
  while (argc > 1) {
    argc--; argv++;
    if (strcmp(*argv, "--opt") == 0) opt = true;
    else if (strcmp(*argv, "-c") == 0 ||
               strcmp(*argv, "--compile") == 0) compile = true;
    else if (strcmp(*argv, "-n") == 0 ||
             strcmp(*argv, "--noisy") == 0) noisy = true;
    else if (strcmp(*argv, "-v") == 0 ||
             strcmp(*argv, "--verbose") == 0) verbose = true;
    else if (strcmp(*argv, "-e") == 0 ||
             strcmp(*argv, "--extra-verbose") == 0) {
      verbose = true;
      extra_verbose = true;
    } else if (strcmp(*argv, "-re") == 0 ||
               strcmp(*argv, "--readable") == 0) readable = true;
    else if (strcmp(*argv, "-r") == 0 ||
             strcmp(*argv, "--run") == 0) run = true;
    else if (strcmp(*argv, "-da") == 0 ||
             strcmp(*argv, "--dump-asm") == 0) {
      dump_asm = true;
      run = false;
    } else if (strcmp(*argv, "-s") == 0 ||
               strcmp(*argv, "--simulate") == 0) {
      simulate = true;
      compile = false;
    }
  }
  
  build_len = strlen(build) + 1;
  src_len = strlen(src) + 1;
  c_len = strlen(compiler) + 1;
  for (size_t i = 0; i < TEST_NAMES; i++) {
    compile_test(tests[i]);
    system(cmd);
    /* if (run) { */
    /*   run_test(tests[i]); */
    /*   system(cmd); */
    /* } */
    printf("\n\n");
  }
  
  return 0;
}

void compile_test(const char *test) {
  size_t test_len = strlen(test) + 1;
  len = c_len + src_len + test_len + build_len + test_len + 120;
  cmd = malloc(len);
  /* snprintf(cmd, len, "%s %s%s.bf -n -c -o %s%s", compiler, src, test, build, test); */
  snprintf(cmd, len, "%s %s%s.bf %s%s%s%s%s%s%s%s-o %s%s",
           compiler, src, test,
           opt ? "--opt " : "", noisy ? "-n " : "",
           compile ? "-c " : "", verbose ? "-v " : "",
           extra_verbose ? "-e " : "", readable ? "-re " : "",
           dump_asm ? "-da " : "", run ? "-r " : "", build, test);
  printf("[INFO] %s\n", cmd);
}

void run_test(const char *test) {
  len = build_len + strlen(test);
  cmd = malloc(len);
  snprintf(cmd, len, "%s%s", build, test);
  printf("[INFO] %s\n", cmd);
}

