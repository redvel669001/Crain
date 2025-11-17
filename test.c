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
  HELLO_WORLD_TEST,
  HELLO_C,
  TEST_NAMES,
} TestName;

const char *tests[TEST_NAMES] = {
  "main",
  "quine",
  "test_case",
  /* "bf_to_c", */
  "hello_world",
  "hello_world_test",
  "hello_c",
};

const char *build = "./build/";
size_t build_len;

const char *src = "./tests/";
size_t src_len;

const char *compiler = "./crain";
size_t c_len;

char *cmd = NULL;
size_t len = 0;

void compile_test(const char *test);
void run_test(const char *test);

bool opt = false; // Optimize before compiling/interpreting.
bool compile = true; // Compile to binary.
bool noisy = false; // Comments in assembly.
bool verbose = false; // Extra comments in assembly.
bool extra_verbose = false; // Extra comments in assembly.
bool readable = false; // Tabs for readability.
bool run = true; // flag for running the file after compiling.
bool dump_asm = false; // flag for dumping the assembly without compiling.
bool simulate = false; // flag to use crain as an interpreter, rather than a compiler.
bool direct_to_binary = false; // flag for compiling directly to binary, rather than generating assembly and using fasm to compile the generated assembly to binary.

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
      run = false;
    } else if (strcmp(*argv, "-b") == 0 ||
               strcmp(*argv, "--binary") == 0) {
      direct_to_binary = true;
      simulate = false;
      compile = true;
    }
  }
  
  build_len = strlen(build) + 1;
  src_len = strlen(src) + 1;
  c_len = strlen(compiler) + 1;
  int ret = 0;
  for (size_t i = 0; i < TEST_NAMES; i++) {
    compile_test(tests[i]);
    ret = system(cmd);
    printf("\n\n");
    if (ret == -1) return 1;
  }
  
  return 0;
}

void compile_test(const char *test) {
  size_t test_len = strlen(test) + 1;
  len = c_len + src_len + test_len + build_len + test_len + 120;
  cmd = malloc(len);
  /* snprintf(cmd, len, "%s %s%s.bf -n -c -o %s%s", compiler, src, test, build, test); */
  snprintf(cmd, len, "%s %s%s.bf %s%s%s%s%s%s%s%s%s%s-o %s%s",
           compiler, src, test,
           opt ? "--opt " : "", noisy ? "-n " : "",
           simulate ? "-s " : "", compile ? "-c " : "",
           direct_to_binary  ? "-b " : "",
           verbose ? "-v " : "", extra_verbose ? "-e " : "",
           readable ? "-re " : "", dump_asm ? "-da " : "",
           run ? "-r " : "", build, test);
  printf("[INFO] %s\n", cmd);
}

void run_test(const char *test) {
  len = build_len + strlen(test);
  cmd = malloc(len);
  snprintf(cmd, len, "%s%s", build, test);
  printf("[INFO] %s\n", cmd);
}

