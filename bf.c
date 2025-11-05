#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>

#define BF_DEF static inline

const char *name = "bf";

// Report with formatting.
#define REPORTF(r, fmt, ...) report((r), __FILE__, __LINE__, 1, (fmt), __VA_ARGS__)

// Report with no formatting
#define REPORT(r, fmt) REPORTF((r), (fmt), NULL)

// Report diagnostics: report with the tokenizer in mind.
#define diagnostic(r, to, fmt, ...)                                     \
  report((r), (to)->path, (to)->t->row, (to)->t->col, (fmt), __VA_ARGS__)

// Report diagnostics - error: report diagnostics for errors, using the tokenizer.
#define diag_err(to, fmt, ...)                                          \
  report(ERROR, (to)->path, (to)->t->row, (to)->t->col, (fmt), __VA_ARGS__)

// Report diagnostics - warn: report diagnostics for warnings, using the tokenizer.
#define diag_warn(to, fmt, ...)                                         \
  report(WARNING, (to)->path, (to)->t->row, (to)->t->col, (fmt), __VA_ARGS__)

// Report diagnostics - note: report diagnostics for notes, using the tokenizer.
#define diag_note(to, fmt, ...)                                         \
  report(NOTE, (to)->path, (to)->t->row, (to)->t->col, (fmt), __VA_ARGS__)


#define PROGRAM_SIZE 30000

#define DA_INIT_CAPACITY 1024

#ifndef da_append
#define da_append(da, item)                                             \
  do {                                                                  \
    if ((da)->capacity < (da)->count + 1) {                             \
      if ((da)->capacity == 0) {                                        \
        (da)->capacity = DA_INIT_CAPACITY;                              \
      }                                                                 \
      while ((da)->capacity < (da)->count + 1) {                        \
        (da)->capacity *= 2;                                            \
      }                                                                 \
      (da)->items = realloc((da)->items, (da)->capacity * sizeof(*(da)->items)); \
    }                                                                   \
    (da)->items[(da)->count++] = (item);                                \
  } while (0)
#endif // da_append

#define DA_MAKE(type, name)                     \
  typedef struct {                              \
    type *items;                                \
    size_t count;                               \
    size_t capacity;                            \
  } name

typedef enum {
  RIGHT, // Move the pointer to the right
  LEFT, // Move the pointer to the left
  INC, // Increment the memory cell at the pointer
  DEC, // Decrement the memory cell at the pointer
  OUT, // Output the character signified by the cell at the pointer
  IN, // Input a character and store it in the cell at the pointer
  LOOP_BGN, // Jump past the matching ] if the cell at the pointer is 0
  LOOP_END, // Jump back to the matching [ if the cell at the pointer is nonzero
  TOKEN_TYPES,
} TokenType;

typedef struct Token Token;

struct Token {
  TokenType type;
  size_t row, col;
  char c;
  size_t jmp;
};

DA_MAKE(Token, Tokens);

typedef struct {
  Tokens ts;
  size_t index;
  Token *t;
  const char *path;
  char *p;
} Tokenizer;

typedef struct Op Op;

typedef enum {
  R, // Move the pointer to the right
  L, // Move the pointer to the left
  I, // Increment the memory cell at the pointer
  D, // Decrement the memory cell at the pointer
  OP_OUT, // Output the character signified by the cell at the pointer
  OP_IN, // Input a character and store it in the cell at the pointer
  LP_BGN, // Jump past the matching ] if the cell at the pointer is 0
  LP_END, // Jump back to the matching [ if the cell at the pointer is nonzero
  ZERO, // Custom operation, as an optimization.
  R_ZERO, // Custom operation, as an optimization.
  L_ZERO, // Custom operation, as an optimization.
  OP_TYPES,
} OpType;

struct Op {
  OpType type;
  size_t count;
  const Token *t;
  size_t jmp;
};

typedef struct {
  Op *items;
  size_t count, capacity;
  char *p;
  const char *path;
  size_t index;
  Op *op;
} Program;

typedef enum {
  NOTE,
  WARNING,
  ERROR,
} ReportLevel;

BF_DEF int print_usage(void);
BF_DEF bool tokenize_file(Tokenizer *t);
BF_DEF TokenType type_index(char c);

BF_DEF bool check_bounds(Tokenizer *t);
BF_DEF bool first_token(Tokenizer *t);
BF_DEF bool next_token(Tokenizer *t);
BF_DEF bool prev_token(Tokenizer *t);
BF_DEF bool to_token(Tokenizer *t, size_t index);
BF_DEF bool tokenizer_jump(Tokenizer *t, const Token *jmp);

BF_DEF void print_token(Tokenizer *t);

BF_DEF bool simulate_program(Tokenizer *t);
BF_DEF bool compile_program_fasm(Tokenizer *t);
BF_DEF bool patch_tokenizer_jmp(Tokenizer *t);
BF_DEF bool patch_token_jmp(Tokenizer *t);

BF_DEF const char *token_type_to_cstr(TokenType type);

BF_DEF void report(ReportLevel r, const char *path, const size_t row, const size_t col, const char *fmt, ...);

BF_DEF bool optimize_program(Tokenizer *t, Program *prog);

BF_DEF void accumulate_shifts(Tokenizer *t, Op *op);
BF_DEF void accumulate_arithmetic(Tokenizer *t, Op *op);
BF_DEF void optimize_loop(Tokenizer *t, Op *op);

BF_DEF bool simulate_optimized_program(Program *prog, Tokenizer *t);
BF_DEF bool gen_optimized_move_pointer(Program *prog, Tokenizer *t, FILE *f);
BF_DEF void gen_optimized_arithmetic(Program *prog, Tokenizer *t, FILE *f);
BF_DEF void gen_write(Program *prog, Tokenizer *t, FILE *f);
BF_DEF void gen_read(Program *prog, Tokenizer *t, FILE *f);
BF_DEF void gen_lp_bgn(Program *prog, Tokenizer *t, FILE *f);
BF_DEF void gen_lp_end(Program *prog, Tokenizer *t, FILE *f);
BF_DEF bool compile_optimized_program_fasm(Program *prog, Tokenizer *t);
BF_DEF bool patch_program_jmp(Program *prog);
BF_DEF bool patch_op_jmp(Program *prog);

BF_DEF bool check_optimized_program_bounds(Program *prog);
BF_DEF bool first_op(Program *prog);
BF_DEF bool next_op(Program *prog);
BF_DEF bool prev_op(Program *prog);
BF_DEF bool to_op(Program *prog, size_t index);
BF_DEF bool program_jump(Program *prog, const Op *jmp);

char program[PROGRAM_SIZE];
bool opt = false;
bool compile = false;
const char *output = "";
char *out_s = NULL;
size_t out_len = 0;
bool def_out = true;

bool noisy = false; // Comments in assembly.
bool verbose = false; // Extra comments in assembly.
bool extra_verbose = false; // Extra comments in assembly.
bool readable = false; // Tabs for readability.
bool run = false; // flag for running the file after compiling.
bool dump_asm = false; // flag for dumping the assembly without compiling.

size_t count = 0;
size_t pointer = 0;
const char *tab = "   ";
const char *spc = " ";

void change_ext(void) {
  out_len = strlen(output);
  size_t final_dot = 0;
  for (size_t i = 0; i < out_len; i++) {
    if (output[i] == '.') final_dot = i;
  }
  out_len = final_dot + 1;
  char *new_out = malloc(out_len);
  snprintf(new_out, out_len, "%.*s", (int) (out_len), output);
  output = new_out;
}

int main(int argc, char **argv) {
  argc--; argv++;
  if (argc < 1) return print_usage();
  Tokenizer t = {0};
  t.path = *argv;

  output = t.path;
  
  while (argc > 1) {
    argc--; argv++;
    if (strcmp(*argv, "--opt") == 0) opt = true;
    else if (strcmp(*argv, "-o") == 0 ||
             strcmp(*argv, "--output") == 0) {
      argc--; argv++;
      if (argc < 1) fprintf(stderr, "ERROR: output flag (-o) used, but no output name specified.\n");
      else output = *argv;
      def_out = false;
      out_len = strlen(output) + 1;
    } else if (strcmp(*argv, "-c") == 0 ||
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
             strcmp(*argv, "--dump-asm") == 0) dump_asm = true;
  }

  if (def_out) change_ext();
  
  if (!tokenize_file(&t)) return 1;
  if (compile) {
    if (opt) {
      Program prog = {0};
      if (!optimize_program(&t, &prog)) return 1;
      if (!compile_optimized_program_fasm(&prog, &t)) return 1;
    } else if (!compile_program_fasm(&t)) return 1;
    return 0;
  }
  if (opt) {
    Program prog = {0};
    if (!optimize_program(&t, &prog)) return 1;
    if (!simulate_optimized_program(&prog, &t)) return 1;
    return 0;
  }
  if (!simulate_program(&t)) return 1;
  return 0;
}

BF_DEF int print_usage(void) {
  fprintf(stderr, "Usage: <%s> <file>", name);
  return 1;
}

BF_DEF bool tokenize_file(Tokenizer *t) {
  FILE *f = fopen(t->path, "rb");
  if (f == NULL) {
    fprintf(stderr, "Could not open file `%s`.\n", t->path);
    if (f) fclose(f);
    return false;
  }

  if (fseek(f, 0, SEEK_END) != 0) {
    fprintf(stderr, "Could not seek to the end of file `%s`.\n", t->path);
    fclose(f);
    return false;
  }
  
  long file_size = ftell(f);
  if (file_size == -1) {
    fprintf(stderr, "Could not measure size of file `%s`.\n", t->path);
    fclose(f);
    return false;
  }

  char *text = malloc(file_size);
  fseek(f, 0, SEEK_SET);
  if (fread(text, file_size, 1, f) == 0) {
    fprintf(stderr, "Could not read file `%s`. It there was an error, or the file was empty.\n", t->path);
  }
  fclose(f);

  char c = 0;
  Token tok = {.row = 1, .col = 1};
  for (size_t i = 0; i < file_size; i++) {
    c = text[i];
    tok.type = type_index(c);
    
    if (tok.type > LOOP_END) {
      tok.col++;
      if (c == '\n') {
        tok.row++;
        tok.col = 1;
      }
      continue;
    }
    
    tok.c = c;
    da_append(&t->ts, tok);
    tok.col++;
  }
  
  return true;
}

BF_DEF TokenType type_index(char c) {
  switch (c) {
  case '>': return RIGHT;
  case '<': return LEFT;
  case '+': return INC;
  case '-': return DEC;
  case '.': return OUT;
  case ',': return IN;
  case '[': return LOOP_BGN;
  case ']': return LOOP_END;
  default: return TOKEN_TYPES;
  }
  return TOKEN_TYPES;
}

BF_DEF bool check_bounds(Tokenizer *t) {
  return (t->index >= 0) && (t->index < t->ts.count);
}

BF_DEF bool first_token(Tokenizer *t) {
  t->index = 0;
  t->t = t->ts.items + t->index;
  return check_bounds(t);
}

BF_DEF bool next_token(Tokenizer *t) {
  t->index++;
  t->t = t->ts.items + t->index;
  return check_bounds(t);
}

BF_DEF bool prev_token(Tokenizer *t) {
  t->index--;
  t->t = t->ts.items + t->index;
  return check_bounds(t);
}

BF_DEF bool to_token(Tokenizer *t, size_t index) {
  t->index = index;
  t->t = t->ts.items + t->index;
  return check_bounds(t);
}

BF_DEF bool tokenizer_jump(Tokenizer *t, const Token *jmp) {
  size_t index = jmp - t->ts.items;
  return to_token(t, index);
}

BF_DEF void print_token(Tokenizer *t) {
  printf("%s:%zu:%zu: token: %c\n",
         t->path, t->t->row, t->t->col, t->t->c);
}

BF_DEF bool simulate_program(Tokenizer *t) {
  first_token(t);
  t->p = program;
  patch_tokenizer_jmp(t);
  while (true) {
    switch (t->t->type) {
    case RIGHT:
      t->p++;
      if (t->p - program >= PROGRAM_SIZE) {
        diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", PROGRAM_SIZE);
        return false;
      }
      break;
    case LEFT:
      t->p--;
      if (t->p - program < 0) {
        diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
        return false;
      }
      break;
    case INC:
      (*t->p)++;
      break;
    case DEC:
      (*t->p)--;
      break;
    case OUT:
      putc((*t->p), stdout);
      break;
    case IN:
      (*t->p) = getchar();
      break;
    case LOOP_BGN:
      if (*t->p == 0) {
        if (!to_token(t, t->t->jmp)) return false;
        prev_token(t);
      }
      break;
    case LOOP_END:
      if (*t->p != 0) {
        if (!to_token(t, t->t->jmp)) return false;
        prev_token(t);
      }
      break;
    case TOKEN_TYPES:
      diag_err(t, "%s", "Unreachable!\n");
      return false;
    default: return false; break;
    }
    if (!next_token(t)) break;
  }
  return true;
}

BF_DEF bool compile_program_fasm(Tokenizer *t) {
  size_t out_s_len = out_len + 2;
  out_s = malloc(out_s_len);
  snprintf(out_s, out_s_len, "%s.s", output);
  FILE *f = fopen(out_s, "wb");
  if (f == NULL) {
    // Maybe do a better error reporting here?
    fprintf(stderr, "Could not open file `%s`.\n", out_s);
    if (f) fclose(f);
    return false;
  }
  
  size_t pointer = 0;
  const char *tab = readable ? "   " : "";
  const char *spc = readable ? " " : "";

  fprintf(f, "format ELF64 executable 3\nentry start\nstart:\n");

  first_token(t);
  t->p = program;
  patch_tokenizer_jmp(t);
  if (verbose) fprintf(f, ";; The pointer should start at 0.\n");
  fprintf(f, "%smov rbx,%s0\n", tab, spc);
  while (true) {
    if (noisy)
      fprintf(f, ";; %s:%zu:%zu: %c\n", t->path, t->t->row, t->t->col, t->t->c);
    switch (t->t->type) {
    case RIGHT:
      if (verbose)
        fprintf(f, "%s;; increment `rbx`, which acts as the index.\n", tab);
      fprintf(f, "%sinc rbx\n", tab);
      pointer++;
      if (pointer >= PROGRAM_SIZE) {
        diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", PROGRAM_SIZE);
        return false;
      }
      break;
    case LEFT:
      if (verbose)
        fprintf(f, "%s;; decrement `rbx`, which acts as the index.\n", tab);
      fprintf(f, "%sdec rbx\n", tab);
      pointer--;
      if (pointer < 0) {
        diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
        return false;
      }
      break;
    case INC:
      if (verbose) fprintf(f, "%s;; read `program`, add the index (`rbx`), then increment the character read from there.\n", tab);
      fprintf(f, "%sinc [program+rbx]\n", tab);
      break;
    case DEC:
      if (verbose) fprintf(f, "%s;; read `program`, add the index (`rbx`), then decrement the character read from there.\n", tab);
      fprintf(f, "%sdec [program+rbx]\n", tab);
      break;
    case OUT:
      if (extra_verbose) fprintf(f, "%s;; 1 is the index of the write syscall, so move 1 to `rax`.\n", tab);
      fprintf(f, "%smov rax,%s1\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; 1 is the fd of stdout, so move 1 to `rdi`.\n", tab);
      fprintf(f, "%smov rdi,%s1\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so load that address into `rsi`.\n", tab);
      fprintf(f, "%slea rsi,%s[program+rbx]\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; printing the current character means printing 1 character, so move 1 to `rdx`.\n", tab);
      fprintf(f, "%smov rdx,%s1\n", tab, spc);
      if (verbose) fprintf(f, "%s;; call the write syscall, to print the current character to stdout.\n", tab);
      fprintf(f, "%ssyscall\n", tab);
      break;
    case IN:
      if (extra_verbose) fprintf(f, "%s;; 0 is the index of the read syscall, so move 0 to `rax`.\n", tab);
      fprintf(f, "%smov rax,%s0\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; 0 is the fd of stdin, so move 0 to `rdi`.\n", tab);
      fprintf(f, "%smov rdi,%s0\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so load that address into `rsi`.\n", tab);
      fprintf(f, "%slea rsi,%s[program+rbx]\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; reading into the current character means reading 1 character, so move 1 to `rdx`.\n", tab);
      fprintf(f, "%smov rdx,%s1\n", tab, spc);
      if (verbose) fprintf(f, "%s;; call the read syscall, to read into the current character from stdin.\n", tab);
      fprintf(f, "%ssyscall\n", tab);
      break;
    case LOOP_BGN:
      if (verbose) fprintf(f, "%s;; mark the address for the loop ending to know where to jump.\n", tab);
      fprintf(f, "%saddr_%zu:\n", tab, t->index);
      if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so move the that to `rax`.\n", tab);
      fprintf(f, "%smov rax,%sQWORD [program+rbx]\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; check byte 0 of `rax` (`al`), which had the current character moved to it.\n", tab);
      fprintf(f, "%stest al,%sal\n", tab, spc);
      if (verbose) fprintf(f, "%s;; if the current character is 0, jump to the ending of the loop.\n", tab);
      fprintf(f, "%sjz addr_%zu\n", tab, t->t->jmp);
      break;
    case LOOP_END:
      if (verbose) fprintf(f, "%s;; mark the address for the loop beginning to know where to jump.\n", tab);
      fprintf(f, "%saddr_%zu:\n", tab, t->index);
      if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so move the that to `rax`.\n", tab);
      fprintf(f, "%smov rax,%sQWORD [program+rbx]\n", tab, spc);
      if (extra_verbose) fprintf(f, "%s;; check byte 0 of `rax` (`al`), which had the current character moved to it.\n", tab);
      fprintf(f, "%stest al,%sal\n", tab, spc);
      if (verbose) fprintf(f, "%s;; if the current character isn't 0, jump to the beginning of the loop.\n", tab);
      fprintf(f, "%sjnz addr_%zu\n", tab, t->t->jmp);
      break;
    case TOKEN_TYPES:
      diag_err(t, "%s", "Unreachable!\n");
      return false;
    default: return false; break;
    }
    if (!next_token(t)) break;
  }
  
  if (extra_verbose) fprintf(f, "%s;; 60 is the index of the exit syscall, so move 60 to `rax`.\n", tab);
  fprintf(f, "%smov rax,%s60\n", tab, spc);
    if (extra_verbose) fprintf(f, "%s;; exit code 0 indicates success, so move 0 to `rdi`.\n", tab);
  fprintf(f, "%smov rdi,%s0\n", tab, spc);
    if (verbose) fprintf(f, "%s;; call the exit syscall, with exit code 0, to indicate the program ran successfully and exited without error.\n", tab);
  fprintf(f, "%ssyscall\n", tab);

  fprintf(f, "segment readable writable\nprogram db %d dup (0)", PROGRAM_SIZE);
  fclose(f);
  
  if (noisy) printf("[INFO] successfully generated file `%.*s`\n", (int) (out_len + 2), out_s);
  
  if (dump_asm) return true;

  // Compile the assembly.
  const char *comp = "fasm -m 524288";
  size_t comp_len = strlen(comp);
  size_t command_len = out_s_len + comp_len + 1;
  char *command = malloc(command_len);
  snprintf(command, command_len, "%s %s", comp, out_s);
  if (noisy) printf("[INFO] %s\n", command);
  system(command);

  // Find the name of the generated executable.
  char *final_out = malloc(out_len);
  snprintf(final_out, out_len, "%.*s", (int) out_len, output);

  // Use chmod +x to make it executable.
  const char *chmod_cmd = "chmod +x";
  size_t chmod_cmd_len = strlen(chmod_cmd);
  size_t cmd_len = chmod_cmd_len + 1 + out_len;
  char *cmd = malloc(cmd_len);
  snprintf(cmd, cmd_len, "%s %s", chmod_cmd, final_out);
  if (noisy) printf("[INFO] %s\n", cmd);
  system(cmd);
  
  if (run) {
    if (noisy) printf("[INFO] %s\n", final_out);
    system(final_out);
  }
  
  return true;
}

BF_DEF bool patch_tokenizer_jmp(Tokenizer *t) {
  size_t point = t->index;

  while (true) {
    if (t->t->type == LOOP_BGN) if (!patch_token_jmp(t)) return false;
    if (!next_token(t)) break;
  }

  return to_token(t, point);
}

BF_DEF bool patch_token_jmp(Tokenizer *t) {
  size_t lpb = 0;
  size_t lpe = 0;
  size_t point = t->index;
  
  size_t jmp = t->index;

  while (true) {
    if (t->t->type == LOOP_END) lpe++;
    else if (t->t->type == LOOP_BGN) lpb++;
    if (lpe == lpb) break;
    if (!next_token(t)) return false;
  }

  t->t->jmp = jmp;

  jmp = t->index;

  if (!to_token(t, point)) return false;
  t->t->jmp = jmp;
  return true;
}

/*

  For reference:

  > 	Move the pointer to the right
  < 	Move the pointer to the left
  + 	Increment the memory cell at the pointer
  - 	Decrement the memory cell at the pointer
  . 	Output the character signified by the cell at the pointer
  , 	Input a character and store it in the cell at the pointer
  [ 	Jump past the matching ] if the cell at the pointer is 0
  ] 	Jump back to the matching [ if the cell at the pointer is nonzero 
  
 */


BF_DEF const char *token_type_to_cstr(TokenType type) {
  switch (type) {
  case RIGHT: return "RIGHT";
  case LEFT: return "LEFT";
  case INC: return "INC";
  case DEC: return "DEC";
  case OUT: return "OUT";
  case IN: return "IN";
  case LOOP_BGN: return "LOOP_BGN";
  case LOOP_END: return "LOOP_END";
  case TOKEN_TYPES: return "TOKEN_TYPES";
  default: return "UNREACHABLE";
  }
  return "UNREACHABLE";
}

BF_DEF void report(ReportLevel r, const char *path, const size_t row, const size_t col, const char *fmt, ...) {
  FILE *stream;
  const char *diag = "";
  switch (r) {
  case NOTE:
    stream = stdout;
    diag = "note";
    break;
  case WARNING:
    stream = stderr;
    diag = "warning";
    break;
  case ERROR:
    stream = stderr;
    diag = "error";
    break;
  default:
    stream = stderr;
    fprintf(stream, "WRONG USAGE!\n");
    exit(1);
    break;
  }

  fprintf(stream, "%s:%ld:%ld: %s: ", path, row, col, diag);
  va_list args;
  va_start(args, fmt);
  vfprintf(stream, fmt, args);
  va_end(args);
}

BF_DEF bool optimize_program(Tokenizer *t, Program *prog) {
  if (!first_token(t)) return false;
  if (!patch_tokenizer_jmp(t)) return false;
  Op op = {0};
  while (true) {
    op.t = t->t;
    op.count = 0;
    op.type = t->t->type;
    switch (t->t->type) {
    case RIGHT:
    case LEFT: accumulate_shifts(t, &op); break;
    case INC:
    case DEC: accumulate_arithmetic(t, &op); break;
    case OUT:
    case IN: break;
    case LOOP_BGN: optimize_loop(t, &op); break;
    case LOOP_END: break;
    }
    da_append(prog, op);
    if (!next_token(t)) break;
  }
}

BF_DEF void accumulate_shifts(Tokenizer *t, Op *op) {
  bool r = t->t->type == RIGHT;
  while (true) {
    switch (t->t->type) {
    case RIGHT:
      if (r) op->count++;
      else {
        if (op->count > 0) op->count--;
        else {
          op->type = LEFT;
          op->count = 1;
          r = !r;
        }
      }
      break;
    case LEFT: 
      if (!r) op->count++;
      else {
        if (op->count > 0) op->count--;
        else {
          op->type = RIGHT;
          op->count = 1;
          r = !r;
        }
      }
      break;
    case DEC:
    case OUT:
    case IN:
    case LOOP_BGN:
    case LOOP_END:
    default: prev_token(t); return; break;
    }
    
    if (!next_token(t)) break;
  }
}

BF_DEF void accumulate_arithmetic(Tokenizer *t, Op *op) {
  bool inc = t->t->type == INC;
  while (true) {
    switch (t->t->type) {
    case INC:
      if (inc) op->count++;
      else {
        if (op->count > 0) op->count--;
        else {
          op->type = LEFT;
          op->count = 1;
          inc = !inc;
        }
      }
      break;
    case DEC: 
      if (!inc) op->count++;
      else {
        if (op->count > 0) op->count--;
        else {
          op->type = RIGHT;
          op->count = 1;
          inc = !inc;
        }
      }
      break;
    case RIGHT:
    case LEFT:
    case IN:
    case LOOP_BGN:
    case LOOP_END:
    default: prev_token(t); return; break;
    }
    
    if (!next_token(t)) break;
  }
}

BF_DEF void optimize_loop(Tokenizer *t, Op *op) {
  size_t point = t->index;
  bool arithmetic = true;
  bool shift = true;
  /* bool simple = true; */
  bool loop = true;
  size_t r = 0, l = 0;
  while ((arithmetic || shift) && loop) {
  /* while (simple && loop) { */
    if (!next_token(t)) break;
    switch (t->t->type) {
    /* case RIGHT: */
    /* case LEFT: */
    case OUT:
    case IN:
    case LOOP_BGN: arithmetic = false; shift = false; break;
    /* case LOOP_BGN: simple = false; break; */
    case LOOP_END: loop = false; break;
    case INC:
    case DEC: shift = false; break;
    /* case DEC: break; */
    case RIGHT: arithmetic = false; r++; break;
    case LEFT: arithmetic = false; l++; break;
    }
  }

  /* if (simple) op->type = ZERO; */
  /* else to_token(t, point); */

  if (arithmetic) op->type = ZERO;
  else if (shift) {
    bool right = r > l;
    /* op->count = right ? r - l : l - r; */
    if (!compile) {
      if (right) op->type = R_ZERO;
      else op->type = L_ZERO;
    } else to_token(t, point);
  } else to_token(t, point);
}

BF_DEF bool simulate_optimized_program(Program *prog, Tokenizer *t) {
  first_op(prog);
  prog->p = program;
  patch_program_jmp(prog);
  while (true) {
    tokenizer_jump(t, prog->op->t);
    switch (prog->op->type) {
    case RIGHT:
      prog->p += prog->op->count;
      if (prog->p - program >= PROGRAM_SIZE) {
        diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", PROGRAM_SIZE);
        return false;
      }
      break;
    case LEFT:
      prog->p -= prog->op->count;
      if (prog->p - program < 0) {
        diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
        return false;
      }
      break;
    case INC:
      (*prog->p) += prog->op->count;
      break;
    case DEC:
      (*prog->p) -= prog->op->count;
      break;
    case OUT:
      putc((*prog->p), stdout);
      break;
    case IN:
      (*prog->p) = getchar();
      break;
    case LOOP_BGN:
      if (*prog->p == 0) {
        if (!to_op(prog, prog->op->jmp)) return false;
        prev_op(prog);
      }
      break;
    case LOOP_END:
      if (*prog->p != 0) {
        if (!to_op(prog, prog->op->jmp)) return false;
        prev_op(prog);
      }
      break;
    case ZERO:
      (*prog->p) = 0;
      break;
    case R_ZERO:
      while (*prog->p) prog->p++;
      break;
    case L_ZERO:
      while (*prog->p) prog->p--;
      break;
    case OP_TYPES:
      diag_err(t, "%s", "Unreachable!\n");
      return false;
    default: return false; break;
    }
    if (!next_op(prog)) break;
  }
  return true;
}

BF_DEF bool gen_optimized_move_pointer(Program *prog, Tokenizer *t, FILE *f) {
  bool right = prog->op->type == R;
  
  if (verbose) fprintf(f, "%s;; %s `rbx`, which acts as the index, by %zu.\n", tab, right ? "increment" : "decrement", count);
  fprintf(f, "%s%s rbx,%s%zu\n", tab, right ? "add" : "sub", spc, count);
  pointer += count;
  
  if (pointer >= PROGRAM_SIZE) {
    diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", PROGRAM_SIZE);
    return false;
  } else if (pointer < 0) {
    diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
    return false;
  }
  
  return true;
}

BF_DEF void gen_optimized_arithmetic(Program *prog, Tokenizer *t, FILE *f) {
  bool add = prog->op->type == I;
  if (verbose) fprintf(f, "%s;; read `program`, add the index (`rbx`), then %s the character read from there by %zu.\n", tab, add ? "increment" : "decrement", count);
  fprintf(f, "%s%s [program+rbx],%s%zu\n", add ? "add" : "sub", tab, spc, count);
  if (add) (*prog->p) += count;
  else (*prog->p) -= count;
}

BF_DEF void gen_write(Program *prog, Tokenizer *t, FILE *f) {
  if (extra_verbose) fprintf(f, "%s;; 1 is the index of the write syscall, so move 1 to `rax`.\n", tab);
  fprintf(f, "%smov rax,%s1\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; 1 is the fd of stdout, so move 1 to `rdi`.\n", tab);
  fprintf(f, "%smov rdi,%s1\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so load that address into `rsi`.\n", tab);
  fprintf(f, "%slea rsi,%s[program+rbx]\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; printing the current character means printing 1 character, so move 1 to `rdx`.\n", tab);
  fprintf(f, "%smov rdx,%s1\n", tab, spc);
  if (verbose) fprintf(f, "%s;; call the write syscall, to print the current character to stdout.\n", tab);
  fprintf(f, "%ssyscall\n", tab);
}

BF_DEF void gen_read(Program *prog, Tokenizer *t, FILE *f) {
  if (extra_verbose) fprintf(f, "%s;; 0 is the index of the read syscall, so move 0 to `rax`.\n", tab);
  fprintf(f, "%smov rax,%s0\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; 0 is the fd of stdin, so move 0 to `rdi`.\n", tab);
  fprintf(f, "%smov rdi,%s0\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so load that address into `rsi`.\n", tab);
  fprintf(f, "%slea rsi,%s[program+rbx]\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; reading into the current character means reading 1 character, so move 1 to `rdx`.\n", tab);
  fprintf(f, "%smov rdx,%s1\n", tab, spc);
  if (verbose) fprintf(f, "%s;; call the read syscall, to read into the current character from stdin.\n", tab);
  fprintf(f, "%ssyscall\n", tab);
}

BF_DEF void gen_lp_bgn(Program *prog, Tokenizer *t, FILE *f) {
  if (verbose) fprintf(f, "%s;; mark the address for the loop ending to know where to jump.\n", tab);
  fprintf(f, "%saddr_%zu:\n", tab, prog->index);
  if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so move the that to `rax`.\n", tab);
  fprintf(f, "%smov rax,%sQWORD [program+rbx]\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; check byte 0 of `rax` (`al`), which had the current character moved to it.\n", tab);
  fprintf(f, "%stest al,%sal\n", tab, spc);
  if (verbose) fprintf(f, "%s;; if the current character is 0, jump to the ending of the loop.\n", tab);
  fprintf(f, "%sjz addr_%zu\n", tab, prog->op->jmp);
}

BF_DEF void gen_lp_end(Program *prog, Tokenizer *t, FILE *f) {
  if (verbose) fprintf(f, "%s;; mark the address for the loop beginning to know where to jump.\n", tab);
  fprintf(f, "%saddr_%zu:\n", tab, prog->index);
  if (extra_verbose) fprintf(f, "%s;; the current character is stored at `program`, offset by the index (`rbx`), so move the that to `rax`.\n", tab);
  fprintf(f, "%smov rax,%sQWORD [program+rbx]\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; check byte 0 of `rax` (`al`), which had the current character moved to it.\n", tab);
  fprintf(f, "%stest al,%sal\n", tab, spc);
  if (verbose) fprintf(f, "%s;; if the current character isn't 0, jump to the beginning of the loop.\n", tab);
  fprintf(f, "%sjnz addr_%zu\n", tab, prog->op->jmp);
}

BF_DEF bool compile_optimized_program_fasm(Program *prog, Tokenizer *t) {
  size_t out_s_len = out_len + 2;
  out_s = malloc(out_s_len);
  snprintf(out_s, out_s_len, "%s.s", output);
  FILE *f = fopen(out_s, "wb");
  if (f == NULL) {
    // Maybe do a better error reporting here?
    fprintf(stderr, "Could not open file `%s`.\n", out_s);
    if (f) fclose(f);
    return false;
  }
  
  tab = readable ? "   " : "";
  spc = readable ? " " : "";

  fprintf(f, "format ELF64 executable 3\nentry start\nstart:\n");

  first_op(prog);
  prog->p = program;
  patch_program_jmp(prog);
  size_t max = 0;
  if (verbose) fprintf(f, ";; The pointer should start at 0.\n");
  fprintf(f, "%smov rbx,%s0\n", tab, spc);
  while (true) {
    tokenizer_jump(t, prog->op->t);
    count = prog->op->count;
    prog->p = program + pointer;
    if (pointer > max) max = pointer;
    if (noisy)
      fprintf(f, ";; %s:%zu:%zu: %c\n", t->path, t->t->row, t->t->col, t->t->c);
    switch (prog->op->type) {
    case RIGHT: 
    case LEFT: if (!gen_optimized_move_pointer(prog, t, f)) return false; break;
    case INC:
    case DEC: gen_optimized_arithmetic(prog, t, f); break;
    case OUT: gen_write(prog, t, f); break;
    case IN: gen_read(prog, t, f); break;
      break;
    case LOOP_BGN: gen_lp_bgn(prog, t, f); break;
    case LOOP_END: gen_lp_end(prog, t, f); break;
      break;
    case ZERO:
      if (verbose) fprintf(f, "%s;; read `program`, add the index (`rbx`), then set it to 0.\n", tab);
      fprintf(f, "%smov [program+rbx],%s0\n", tab, spc);
      break;
    case R_ZERO:
    case L_ZERO:
      gen_lp_bgn(prog, t, f);
      OpType temp = prog->op->type;
      prog->op->type -= R_ZERO;
      if (!gen_optimized_move_pointer(prog, t, f)) return false;
      gen_lp_end(prog, t, f);
      prog->op->type = temp;
      break;
    case OP_TYPES:
      diag_err(t, "%s", "Unreachable!\n");
      return false;
    default: return false; break;
    }
    if (!next_op(prog)) break;
  }
  
  if (extra_verbose) fprintf(f, "%s;; 60 is the index of the exit syscall, so move 60 to `rax`.\n", tab);
  fprintf(f, "%smov rax,%s60\n", tab, spc);
    if (extra_verbose) fprintf(f, "%s;; exit code 0 indicates success, so move 0 to `rdi`.\n", tab);
  fprintf(f, "%smov rdi,%s0\n", tab, spc);
    if (verbose) fprintf(f, "%s;; call the exit syscall, with exit code 0, to indicate the program ran successfully and exited without error.\n", tab);
  fprintf(f, "%ssyscall\n", tab);

  fprintf(f, "segment readable writable\nprogram db %zu dup (0)", max);
  fclose(f);
  
  if (noisy) printf("[INFO] successfully generated file `%.*s`\n", (int) (out_len + 2), out_s);
  
  if (dump_asm) return true;

  // Compile the assembly.
  const char *comp = "fasm -m 524288";
  size_t comp_len = strlen(comp);
  size_t command_len = out_s_len + comp_len + 1;
  char *command = malloc(command_len);
  snprintf(command, command_len, "%s %s", comp, out_s);
  if (noisy) printf("[INFO] %s\n", command);
  system(command);

  // Find the name of the generated executable.
  char *final_out = malloc(out_len);
  snprintf(final_out, out_len, "%.*s", (int) out_len, output);

  // Use chmod +x to make it executable.
  const char *chmod_cmd = "chmod +x";
  size_t chmod_cmd_len = strlen(chmod_cmd);
  size_t cmd_len = chmod_cmd_len + 1 + out_len;
  char *cmd = malloc(cmd_len);
  snprintf(cmd, cmd_len, "%s %s", chmod_cmd, final_out);
  if (noisy) printf("[INFO] %s\n", cmd);
  system(cmd);
  
  if (run) {
    if (noisy) printf("[INFO] %s\n", final_out);
    system(final_out);
  }
  
  return true;
}

BF_DEF bool patch_program_jmp(Program *prog) {
  size_t point = prog->index;

  while (true) {
    if (prog->op->type == LOOP_BGN) if (!patch_op_jmp(prog)) return false;
    if (!next_op(prog)) break;
  }

  prog->index = point;
  prog->op = prog->items + prog->index;
  return to_op(prog, point);
  return true;
}

BF_DEF bool patch_op_jmp(Program *prog) {
  size_t lpb = 0;
  size_t lpe = 0;
  size_t point = prog->index;
  
  size_t jmp = prog->index;

  while (true) {
    if (prog->op->type == LOOP_END) lpe++;
    else if (prog->op->type == LOOP_BGN) lpb++;
    if (lpe == lpb) break;
    if (!next_op(prog)) return false;
  }

  prog->op->jmp = jmp;

  jmp = prog->index;

  if (!to_op(prog, point)) return false;
  prog->op->jmp = jmp;
  return true;
}

BF_DEF bool check_optimized_program_bounds(Program *prog) {
  return (prog->index >= 0) && (prog->index < prog->count);
}

BF_DEF bool first_op(Program *prog) {
  prog->index = 0;
  prog->op = prog->items + prog->index;
  return check_optimized_program_bounds(prog);
}

BF_DEF bool next_op(Program *prog) {
  prog->index++;
  prog->op = prog->items + prog->index;
  return check_optimized_program_bounds(prog);
}

BF_DEF bool prev_op(Program *prog) {
  prog->index--;
  prog->op = prog->items + prog->index;
  return check_optimized_program_bounds(prog);
}

BF_DEF bool to_op(Program *prog, size_t index) {
  prog->index = index;
  prog->op = prog->items + prog->index;
  return check_optimized_program_bounds(prog);
}

BF_DEF bool program_jump(Program *prog, const Op *jmp) {
  size_t index = jmp - prog->items;
  return to_op(prog, index);
}
