#include <assert.h>
#include <elf.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BF_DEF static inline

const char *name = "crain";

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

#define TAPE_SIZE 30000

#define DA_INIT_CAPACITY 1024

// Largely copied from the nob_da_append macro in nob.h, albeit highly simplified.
// https://github.com/tsoding/nob.h/blob/main/nob.h
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
  union {
    size_t t; // Start of equivalent token sequence.
    size_t b; // Binary length. Useful for direct-to-binary compilation.
  };
  union {
    size_t count; // Not applicable to every OpType, but useful when it does.
    size_t jmp; // Applicable only on LP_BGN and LP_END.
  };
};

typedef struct {
  Op *items;
  size_t count, capacity;
  char *p;
  const char *path;
  size_t index;
  Op *op;
} Program;

DA_MAKE(char, Bytes);

typedef enum {
  NOTE,
  WARNING,
  ERROR,
} ReportLevel;

typedef enum {
  RAX,
  RBX,
  RSI,
  RDX,
  RDI,
  REGISTERS,
} Register;

BF_DEF void append_bytes(Bytes *s, const char *bytes, size_t len);

BF_DEF int print_usage(void);
BF_DEF bool tokenize_file(Tokenizer *t);
BF_DEF TokenType type_index(char c);

BF_DEF bool check_bounds(const Tokenizer *t);
BF_DEF bool first_token(Tokenizer *t);
BF_DEF bool next_token(Tokenizer *t);
BF_DEF bool prev_token(Tokenizer *t);
BF_DEF bool to_token(Tokenizer *t, size_t index);

BF_DEF void print_token(Tokenizer *t);

BF_DEF bool simulate_program(Tokenizer *t);
BF_DEF bool compile_program_fasm(Tokenizer *t);
BF_DEF bool compile_program_elf(Tokenizer *t);

BF_DEF bool gen_move_pointer_fasm(Tokenizer *t, FILE *f);
BF_DEF void gen_arithmetic_fasm(Tokenizer *t, FILE *f);
BF_DEF void gen_write_fasm(Tokenizer *t, FILE *f);
BF_DEF void gen_read_fasm(Tokenizer *t, FILE *f);
BF_DEF void gen_lp_bgn_fasm(Tokenizer *t, FILE *f, size_t jmp, size_t index);
BF_DEF void gen_lp_end_fasm(Tokenizer *t, FILE *f, size_t jmp, size_t index);

BF_DEF Elf64_Ehdr gen_elf_header(void);
BF_DEF Elf64_Phdr gen_elf_entry_program_header(void);
BF_DEF Elf64_Phdr gen_elf_tape_program_header(void);

BF_DEF void gen_start_elf(Bytes *s, bool write, bool read);
BF_DEF void gen_mov_elf(Bytes *s, Register r, size_t count);
BF_DEF void gen_inc_or_dec_elf(Bytes *s, Register r, bool inc);
BF_DEF void gen_add_or_sub_elf(Bytes *s, Register r, bool add, bool lf, size_t a);

BF_DEF void gen_little_endian(Bytes *s, size_t big_endian, size_t len);

BF_DEF void gen_move_pointer_elf(Bytes *s, int type);
BF_DEF void gen_arithmetic_elf(Bytes *s, int type);
BF_DEF void gen_read_write_syscall_elf(Bytes *s, int type);
BF_DEF void gen_lp_elf(Bytes *s, int type, size_t jmp);
BF_DEF void gen_exit_ok_elf(Bytes *s, bool write, bool read);

BF_DEF void gen_syscall_elf(Bytes *s);

BF_DEF void patch_tape_elf(Elf64_Phdr *e, Elf64_Phdr *t, Bytes *s, size_t insts);
BF_DEF bool write_headers_to_elf(FILE *f, Elf64_Phdr entry, Elf64_Phdr thdr);
BF_DEF bool write_bytes_to_elf(FILE *f, Bytes *start, Bytes *insts, size_t tlen);

BF_DEF bool make_executable_and_optionally_run(void);

BF_DEF bool patch_tokenizer_jmp(Tokenizer *t);
BF_DEF void patch_tokenizer_jmp_for_binary(Tokenizer *t);
BF_DEF bool patch_token_jmp(Tokenizer *t);
BF_DEF void patch_token_jmp_for_binary(Tokenizer *t);

BF_DEF const char *token_type_to_cstr(TokenType type);
BF_DEF size_t bin_len_by_token_type(TokenType type);
BF_DEF size_t bin_len_by_op_type(OpType type);

BF_DEF void report(ReportLevel r, const char *path, const size_t row, const size_t col, const char *fmt, ...);

BF_DEF bool optimize_program(Tokenizer *t, Program *prog);

BF_DEF void accumulate_shifts(Tokenizer *t, Op *op);
BF_DEF void accumulate_arithmetic(Tokenizer *t, Op *op);
BF_DEF void optimize_loop(Tokenizer *t, Op *op);

BF_DEF bool extra_optimize_loop(Program *prog, Op *op);

BF_DEF bool simulate_optimized_program(Program *prog, Tokenizer *t);
BF_DEF bool compile_optimized_program_fasm(Program *prog, Tokenizer *t);
BF_DEF bool compile_optimized_program_elf(Program *prog);
BF_DEF bool compile_size_optimized_program_elf(Program *prog);

BF_DEF bool patch_program_jmp(Program *prog);
BF_DEF void patch_program_jmp_for_binary(Program *prog);
BF_DEF void patch_program_jmp_for_size_opt_bin(Program *prog, bool *w, bool *r);
BF_DEF bool patch_op_jmp(Program *prog);
BF_DEF void patch_op_jmp_for_binary(Program *prog);
BF_DEF void patch_op_jmp_for_size_opt_bin(Program *prog);

BF_DEF bool gen_optimized_move_pointer_fasm(Program *prog, Tokenizer *t, FILE *f);
BF_DEF void gen_optimized_arithmetic_fasm(Program *prog, FILE *f);

BF_DEF void gen_optimized_move_pointer_elf(Op *op, Bytes *s);
BF_DEF void gen_size_optimized_move_pointer_elf(Op *op, Bytes *s);
BF_DEF void gen_optimized_arithmetic_elf(Op *op, Bytes *s);
BF_DEF void gen_size_optimized_arithmetic_elf(Op *op, Bytes *s);
BF_DEF void gen_zero_elf(Bytes *s);
BF_DEF void gen_size_optimized_read_write_syscall_elf(Op *op, Bytes *s, bool *w, bool *r);
BF_DEF void gen_size_optimized_lp_elf(Op *op, Bytes *s);

BF_DEF bool check_optimized_program_bounds(const Program *prog);
BF_DEF bool first_op(Program *prog);
BF_DEF bool next_op(Program *prog);
BF_DEF bool prev_op(Program *prog);
BF_DEF bool to_op(Program *prog, size_t index);

char tape[TAPE_SIZE];
bool opt = false;
bool size_opt = false;
bool compile = false;
bool direct_to_binary = false;
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

/* size_t count = 0; */
size_t pointer = 0;
const char *tab = "   ";
const char *spc = " ";

size_t bin_lens[TOKEN_TYPES/2] = {3, 2, 16, 11};
size_t bin_lens_opt[OP_TYPES/2] = {7, 3, 16, 11};

void remove_ext(void) {
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
    else if (strcmp(*argv, "-b") == 0 ||
             strcmp(*argv, "--binary") == 0) {
      compile = true;
      direct_to_binary = true;
    } else if (strcmp(*argv, "-so") == 0 ||
               strcmp(*argv, "--size-opt") == 0) {
      opt = true;
      size_opt = true;
    }
  }

  if (def_out) remove_ext();
  
  if (!tokenize_file(&t)) return 1;
  Program prog = {0};
  if (opt) if (!optimize_program(&t, &prog)) return 1;
  
  if (compile) {
    if (direct_to_binary) {
      if (opt) {
        if (size_opt) {
          if (!compile_size_optimized_program_elf(&prog)) return 1;
        }
        else if (!compile_optimized_program_elf(&prog)) return 1;
      }
      else if (!compile_program_elf(&t)) return 1;
    }
    else {
      if (opt) {
        if (!compile_optimized_program_fasm(&prog, &t)) return 1;
      } else if (!compile_program_fasm(&t)) return 1;
    }
    return 0;
  }
  
  if (opt) {
    if (!simulate_optimized_program(&prog, &t)) return 1;
    return 0;
  }
  if (!simulate_program(&t)) return 1;
  return 0;
}

// Largely copied from the nob_da_append_many macro in nob.h, mildly modified:
// 1. It is simplified.
// 2. It is implemented as a function, rather than a macro, since it sees no real use in this codebase, other than appending bytes.
// https://github.com/tsoding/nob.h/blob/main/nob.h
BF_DEF void append_bytes(Bytes *s, const char *bytes, size_t len) {
  if (s->count + len > s->capacity) {
    if (s->capacity == 0) {
      s->capacity = DA_INIT_CAPACITY;
    }
    while (s->count + len > s->capacity) {
      s->capacity *= 2;
    }
    s->items = realloc(s->items, s->capacity);
  }
  memcpy(s->items + s->count, bytes, len);
  s->count += len;
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

BF_DEF bool check_bounds(const Tokenizer *t) {
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

BF_DEF void print_token(Tokenizer *t) {
  printf("%s:%zu:%zu: token: %c\n", t->path, t->t->row, t->t->col, t->t->c);
}

BF_DEF bool simulate_program(Tokenizer *t) {
  first_token(t);
  t->p = tape;
  patch_tokenizer_jmp(t);
  while (true) {
    switch (t->t->type) {
    case RIGHT:
      t->p++;
      if (t->p - tape >= TAPE_SIZE) {
        diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", TAPE_SIZE);
        return false;
      }
      break;
    case LEFT:
      t->p--;
      if (t->p - tape < 0) {
        diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
        return false;
      }
      break;
    case INC: (*t->p)++; break;
    case DEC: (*t->p)--; break;
    case OUT: putc((*t->p), stdout); break;
    case IN: (*t->p) = getchar(); break;
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
    case TOKEN_TYPES: diag_err(t, "%s", "Unreachable!\n"); return false;
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
  tab = readable ? "   " : "";
  spc = readable ? " " : "";

  fprintf(f, "format ELF64 executable 3\nentry start\nstart:\n");

  first_token(t);
  t->p = tape;
  patch_tokenizer_jmp(t);
  if (verbose) fprintf(f, ";; Make `rsi` point at the tape.\n");
  fprintf(f, "%smov rsi,%stape\n", tab, spc);
  if (verbose) fprintf(f, ";; Reads and writes operate on only one character, so just move 1 to rdx once and for all.\n");
  fprintf(f, "%smov rdx,%s1\n", tab, spc);
  while (true) {
    if (noisy)
      fprintf(f, ";; %s:%zu:%zu: %c\n", t->path, t->t->row, t->t->col, t->t->c);
    switch (t->t->type) {
    case RIGHT:
    case LEFT: if (!gen_move_pointer_fasm(t, f)) return false; break;
    case INC:
    case DEC: gen_arithmetic_fasm(t, f); break;
    case OUT: gen_write_fasm(t, f); break;
    case IN: gen_read_fasm(t, f); break;
    case LOOP_BGN: gen_lp_bgn_fasm(t, f, t->t->jmp, t->index); break;
    case LOOP_END: gen_lp_end_fasm(t, f, t->t->jmp, t->index); break;
    case TOKEN_TYPES: diag_err(t, "%s", "Unreachable!\n"); return false;
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

  fprintf(f, "segment readable writable\ntape db %d dup (0)", TAPE_SIZE);
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
  int ret = system(command);
  if (ret == -1) return false;

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
  ret = system(cmd);
  if (ret == -1) return false;
  
  if (run) {
    if (noisy) printf("[INFO] %s\n", final_out);
    ret = system(final_out);
    if (ret == -1) return false;
  }
  
  return true;
}

BF_DEF bool compile_program_elf(Tokenizer *t) {
  FILE *f = fopen(output, "wb");
  if (f == NULL) {
    // Maybe do a better error reporting here?
    fprintf(stderr, "Could not open file `%s`.\n", output);
    if (f) fclose(f);
    return false;
  }

  // Mandatory headers.
  Elf64_Ehdr elfh = gen_elf_header();
  Elf64_Phdr entry = gen_elf_entry_program_header();
  Elf64_Phdr thdr = gen_elf_tape_program_header();

  size_t s = fwrite(&elfh, 1, sizeof(elfh), f);
  if (s != sizeof(elfh)) {
    perror("fwrite");
    return false;
  }

  // Point rsi at the tape.
  Bytes start = {0};
  gen_start_elf(&start, true, true);
  
  size_t pointer = 0;
  first_token(t);
  patch_tokenizer_jmp(t);
  patch_tokenizer_jmp_for_binary(t);

  Bytes insts = {0};
  for (size_t i = 0; i < t->ts.count; i++) {
    Token *tok = t->ts.items + i;
    switch (tok->type) {
    case RIGHT: case LEFT: gen_move_pointer_elf(&insts, tok->type); break;
    case INC: case DEC: gen_arithmetic_elf(&insts, tok->type); break;
    case OUT: case IN: gen_read_write_syscall_elf(&insts, tok->type); break;
    case LOOP_BGN: case LOOP_END: gen_lp_elf(&insts, tok->type, tok->jmp); break;
    case TOKEN_TYPES: default: assert (0 && "unreachable"); break;
    }
  }

  gen_exit_ok_elf(&insts, false, false);
  patch_tape_elf(&entry, &thdr, &start, insts.count);

  if (!write_headers_to_elf(f, entry, thdr)) return false;
  if (!write_bytes_to_elf(f, &start, &insts, thdr.p_filesz)) return false;
  fclose(f);
  if (noisy) printf("[INFO] Successfully generated binary %s\n", output);
  if (!make_executable_and_optionally_run()) return false;
  return true;
}

BF_DEF bool gen_move_pointer_fasm(Tokenizer *t, FILE *f) {
  bool right = t->t->type == RIGHT;
  
  if (verbose) fprintf(f, "%s;; %s rsi, which points at the tape.\n", tab, right ? "increment" : "decrement");
  fprintf(f, "%s%s rsi\n", tab, right ? "inc" : "dec");
  if (right) pointer++; else pointer--;
  
  if (pointer >= TAPE_SIZE) {
    diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", TAPE_SIZE);
    return false;
  } else if (pointer < 0) {
    diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
    return false;
  }

  return true;
}

BF_DEF void gen_arithmetic_fasm(Tokenizer *t, FILE *f) {
  bool add = t->t->type == INC;
  if (verbose) fprintf(f, "%s;; %s the current character.\n", tab, add ? "increment" : "decrement");
  fprintf(f, "%s%s byte%s[rsi]\n", tab, add ? "inc" : "dec", spc);
  if (add) (*t->p)++; else (*t->p)--;
}

BF_DEF void gen_write_fasm(Tokenizer *t, FILE *f) {
  if (extra_verbose) fprintf(f, "%s;; 1 is the index of the write syscall, so move 1 to `rax`.\n", tab);
  fprintf(f, "%smov rax,%s1\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; 1 is the fd of stdout, so move 1 to `rdi`.\n", tab);
  fprintf(f, "%smov rdi,%s1\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; `rsi` already points at the tape, so nothing needs to be done about it, hence its absence.\n", tab);
  if (extra_verbose) fprintf(f, "%s;; `rdx` has already been set to one, so no need to change it here, hence its absence.\n", tab);
  if (verbose) fprintf(f, "%s;; call the write syscall, to print the current character to stdout.\n", tab);
  fprintf(f, "%ssyscall\n", tab);
}

BF_DEF void gen_read_fasm(Tokenizer *t, FILE *f) {
  if (extra_verbose) fprintf(f, "%s;; 0 is the index of the read syscall, so move 0 to `rax`.\n", tab);
  fprintf(f, "%smov rax,%s0\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; 0 is the fd of stdin, so move 0 to `rdi`.\n", tab);
  fprintf(f, "%smov rdi,%s0\n", tab, spc);
  if (extra_verbose) fprintf(f, "%s;; `rsi` already points at the tape, so nothing needs to be done about it, hence its absence.\n", tab);
  if (extra_verbose) fprintf(f, "%s;; `rdx` has already been set to one, so no need to change it here, hence its absence.\n", tab);
  if (verbose) fprintf(f, "%s;; call the read syscall, to read into the current character from stdin.\n", tab);
  fprintf(f, "%ssyscall\n", tab);
}

BF_DEF void gen_lp_bgn_fasm(Tokenizer *t, FILE *f, size_t jmp, size_t index) {
  if (verbose) fprintf(f, "%s;; mark the address for the loop ending to know where to jump.\n", tab);
  fprintf(f, "%saddr_%zu:\n", tab, index);
  if (extra_verbose) fprintf(f, "%s;; mov the current character to `rax`.\n", tab);
  fprintf(f, "%smov rax,%sQWORD%s[rsi]\n", tab, spc, spc);
  if (extra_verbose) fprintf(f, "%s;; check byte 0 of `rax` (`al`), which had the current character moved to it.\n", tab);
  fprintf(f, "%stest al,%sal\n", tab, spc);
  if (verbose) fprintf(f, "%s;; if the current character is 0, jump to the ending of the loop.\n", tab);
  fprintf(f, "%sjz addr_%zu\n", tab, jmp);
}

BF_DEF void gen_lp_end_fasm(Tokenizer *t, FILE *f, size_t jmp, size_t index) {
  if (verbose) fprintf(f, "%s;; mark the address for the loop beginning to know where to jump.\n", tab);
  fprintf(f, "%saddr_%zu:\n", tab, index);
  if (extra_verbose) fprintf(f, "%s;; mov the current character to `rax`.\n", tab);
  fprintf(f, "%smov rax,%sQWORD%s[rsi]\n", tab, spc, spc);
  if (extra_verbose) fprintf(f, "%s;; check byte 0 of `rax` (`al`), which had the current character moved to it.\n", tab);
  fprintf(f, "%stest al,%sal\n", tab, spc);
  if (verbose) fprintf(f, "%s;; if the current character isn't 0, jump to the beginning of the loop.\n", tab);
  fprintf(f, "%sjnz addr_%zu\n", tab, jmp);
}

BF_DEF Elf64_Ehdr gen_elf_header(void) {
  return (Elf64_Ehdr) {
    .e_ident = {
      ELFMAG0,
      ELFMAG1,
      ELFMAG2,
      ELFMAG3,
      ELFCLASS64,
      ELFDATA2LSB,
      EV_CURRENT,
      ELFOSABI_LINUX,
      0, 0, 0, 0, 0, 0, 0, 0//, EI_NIDENT
    },                       /* Magic number and other info */
    .e_type = ET_EXEC,       /* Object file type */
    .e_machine = EM_X86_64,  /* Architecture */
    .e_version = EV_CURRENT, /* Object file version */
    .e_entry = 0x4000b0,     /* Entry point virtual address */
    .e_phoff = 64,           /* Program header table file offset */
    .e_shoff = 0,            /* Section header table file offset */
    .e_flags = 0,            /* Processor-specific flags */
    .e_ehsize = 64,          /* ELF header size in bytes */
    .e_phentsize = 56,       /* Program header table entry size */
    .e_phnum = 2,            /* Program header table entry count */
    .e_shentsize = 64,        /* Section header table entry size */
    .e_shnum = 0,            /* Section header table entry count */
    .e_shstrndx = SHN_UNDEF, /* Section header string table index */
  };
}

BF_DEF Elf64_Phdr gen_elf_entry_program_header(void) {
  return (Elf64_Phdr) {
    .p_type = PT_LOAD,             /* Segment type */
    .p_offset = 0,                 /* Segment file offset */
    .p_vaddr = 0x400000,           /* Segment virtual address */
    .p_paddr = 0x400000,           /* Segment physical address */
    .p_filesz = 0x1bea,            /* Segment size in file */
    .p_memsz = 0x1bea,             /* Segment size in memory */
    .p_flags = PF_X | PF_W | PF_R, /* Segment flags */
    .p_align = 0x1000,             /* Segment alignment */
  };
}

BF_DEF Elf64_Phdr gen_elf_tape_program_header(void) {
  return (Elf64_Phdr) {
    .p_type = PT_LOAD,      /* Segment type */
    .p_offset = 0,          /* Segment file offset */
    .p_vaddr = 0x401000,    /* Segment virtual address */
    .p_paddr = 0x401000,    /* Segment physical address */
    .p_filesz = 0x7530,     /* Segment size in file */
    .p_memsz = 0x7530,      /* Segment size in memory */
    .p_flags = PF_W | PF_R, /* Segment flags */
    .p_align = 0x1000,      /* Segment alignment */
  };
}

BF_DEF void gen_start_elf(Bytes *s, bool write, bool read) {
  // Point rsi at the tape < needs to be patched later
  gen_mov_elf(s, RSI, 0); // mov rsi, 0
  if (write || read) gen_mov_elf(s, RDX, 1);
  if (size_opt && write) {
    gen_mov_elf(s, RAX, 1);
    gen_mov_elf(s, RDI, 1);
  } else if (size_opt && read) {
    gen_mov_elf(s, RAX, 0);
    gen_mov_elf(s, RDI, 0);
  }
}

BF_DEF void gen_mov_elf(Bytes *s, Register r, size_t count) {
  append_bytes(s, "\x48\xC7", 2); // mov
  switch (r) {
  case RAX: da_append(s, 0xC0); break;
  case RSI: da_append(s, 0xC6); break;
  case RDX: da_append(s, 0xC2); break;
  case RDI: da_append(s, 0xC7); break;
  case RBX:
  case REGISTERS: default: assert(0 && "UNREACHABLE");
  }
  gen_little_endian(s, count, 4);
}

BF_DEF void gen_inc_or_dec_elf(Bytes *s, Register r, bool inc) {
  append_bytes(s, "\x48\xFF", 2);
  switch (r) {
  case RAX: da_append(s, inc ? 0xC0 : 0xC8); break;
  case RSI: da_append(s, inc ? 0xC6 : 0xCE); break;
  case RDI: da_append(s, inc ? 0xC7 : 0xCF); break;
  case RDX:
  case RBX:
  case REGISTERS: default: assert(0 && "UNREACHABLE");
  }
}

BF_DEF void gen_add_or_sub_elf(Bytes *s, Register r, bool add, bool lf, size_t a) {
  da_append(s, 0x48);
  if (lf) da_append(s, 0x81);
  else {
    if (r != RAX) da_append(s, 0x83);
    else {
      if (add) da_append(s, 0x05);
      else da_append(s, 0x2D);
      return;
    }
  }
  switch (r) {
  case RAX: da_append(s, add ? 0xC0 : 0xE8); break;
  case RBX: da_append(s, add ? 0xC3 : 0xEB); break;
  case RSI: da_append(s, add ? 0xC6 : 0xEE); break;
  case RDX: da_append(s, add ? 0xC7 : 0xEF); break;
  case RDI: da_append(s, add ? 0xC2 : 0xEA); break;
  case REGISTERS: default: assert(0 && "UNREACHABLE");
  }

  if (!lf) da_append(s, (char) (a & 0xFF));
  else gen_little_endian(s, a, 4);
}

BF_DEF void gen_little_endian(Bytes *s, size_t big_endian, size_t len) {
  for (size_t i = 0; i < len; i++) {
    char c = (big_endian >> (i * 8)) & 0xFF;
    da_append(s, c);
  }
}

BF_DEF void gen_move_pointer_elf(Bytes *s, int type) {
  append_bytes(s, "\x48\xFF", 2); // inc or dec, seemingly inferred by the next byte.
  if (type == RIGHT) da_append(s, 0xC6); 
  else da_append(s, 0xCE); 
}

BF_DEF void gen_arithmetic_elf(Bytes *s, int type) {
  da_append(s, 0xFE); // inc or dec [byte], seemingly inferred by the next byte.
  if (type == INC) da_append(s, 0x06); // implies inc and byte [rsi].
  else da_append(s, 0x0E); // implies dec and byte [rsi].
}


BF_DEF void gen_read_write_syscall_elf(Bytes *s, int type) {
  size_t call = type == OUT ? 1 : 0;
  gen_mov_elf(s, RAX, call);
  gen_mov_elf(s, RDI, call);
  gen_syscall_elf(s);
}

BF_DEF void gen_lp_elf(Bytes *s, int type, size_t jmp) {
  append_bytes(s, "\x48\x8b\x06", 3); // mov rax, QWORD [rsi]
  append_bytes(s, "\x84\xc0", 2); // test al, al
  if (type == LOOP_BGN) append_bytes(s, "\x0f\x84", 2); // jz
  else append_bytes(s, "\x0f\x85", 2); // jnz
  gen_little_endian(s, jmp, 4);
}

BF_DEF void gen_exit_ok_elf(Bytes *s, bool write, bool read) {
  /* gen_mov_elf(s, RAX, 60); */
  /* gen_mov_elf(s, RDI, 0); */
  /* gen_syscall_elf(s); */

  if (write) {
    gen_add_or_sub_elf(s, RAX, true, true, 59);
    gen_inc_or_dec_elf(s, RDI, false);
  } else if (read) {
    gen_add_or_sub_elf(s, RAX, true, true, 60);
  } else {
    gen_mov_elf(s, RAX, 60);
    gen_mov_elf(s, RDI, 0);
  }
  gen_syscall_elf(s);
}

BF_DEF void gen_syscall_elf(Bytes *s) {
  append_bytes(s, "\x0f\x05", 2);
}

BF_DEF void patch_tape_elf(Elf64_Phdr *e, Elf64_Phdr *t, Bytes *s, size_t insts) {
  // patch entry's p_filesz
  e->p_filesz = 176 + s->count + insts;
  e->p_memsz = e->p_filesz;

  // patch thdr's vaddr
  t->p_vaddr += e->p_filesz;
  t->p_paddr = t->p_vaddr;
  t->p_offset = e->p_filesz;

  // patch the pointer to the tape.
  size_t tape_pointer = t->p_vaddr;
  for (size_t i = 0; i < 4; i++) {
    s->items[3 + i] = (tape_pointer >> (i * 8)) & 0xFF;
  }
}

BF_DEF bool write_headers_to_elf(FILE *f, Elf64_Phdr entry, Elf64_Phdr thdr) {
  size_t s = fwrite(&entry, 1, sizeof(entry), f);
  if (s != sizeof(entry)) {
    perror("fwrite");
    return false;
  }
  
  s = fwrite(&thdr, 1, sizeof(thdr), f);
  if (s != sizeof(thdr)) {
    perror("fwrite");
    return false;
  }

  return true;
}

BF_DEF bool write_bytes_to_elf(FILE *f, Bytes *start, Bytes *insts, size_t tlen) {
  size_t s = fwrite(start->items, 1, start->count, f);
  if (s != start->count) {
    perror("fwrite");
    return false;
  }
  
  s = fwrite(insts->items, 1, insts->count, f);
  if (s != insts->count) {
    perror("fwrite");
    return false;
  }

  s = fwrite(tape, 1, tlen, f);
  if (s != tlen) {
    perror("fwrite");
    return false;
  }

  return true;
}

BF_DEF bool make_executable_and_optionally_run(void) {
  const char *cmd = "chmod +x";
  size_t cmd_len = strlen(cmd);
  size_t command_len = out_len + cmd_len + 1;
  char *command = malloc(command_len);
  snprintf(command, command_len, "%s %s", cmd, output);
  if (noisy) printf("[INFO] %s\n", command);
  int ret = system(command);
  if (ret == -1) return false;

  if (run) {
    if (noisy) printf("[INFO] %s\n", output);
    ret = system(output);
    if (ret == -1) return false;
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

BF_DEF void patch_tokenizer_jmp_for_binary(Tokenizer *t) {
  size_t point = t->index;

  while (true) {
    if (t->t->type == LOOP_BGN) patch_token_jmp_for_binary(t);
    if (!next_token(t)) break;
  }
  
  to_token(t, point);
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

BF_DEF void patch_token_jmp_for_binary(Tokenizer *t) {
  size_t point = t->index;
  size_t jmp = t->t->jmp;
  size_t bin_jmp = 0;
  for (size_t i = point; i < jmp; i++) {
    Token *tok = t->ts.items + i;
    size_t bin_len = bin_len_by_token_type(tok->type);
    bin_jmp += bin_len;
  }

  t->t->jmp = bin_jmp;

  bin_jmp = 0;
  
  for (size_t i = jmp; i > point; i--) {
    Token *tok = t->ts.items + i;
    size_t bin_len = bin_len_by_token_type(tok->type);
    bin_jmp -= bin_len;
  }

  to_token(t, jmp);
  t->t->jmp = bin_jmp;
  to_token(t, point);
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

BF_DEF size_t bin_len_by_token_type(TokenType type) {
  return bin_lens[type/2];
}

BF_DEF size_t bin_len_by_op_type(OpType type) {
  if (type < ZERO) return bin_lens_opt[type/2];
  else if (type == ZERO) return 3;
  return -1;
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
    op.t = t->index;
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
  return true;
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
  bool loop = true;
  size_t r = 0, l = 0;
  while ((arithmetic || shift) && loop) {
    if (!next_token(t)) break;
    switch (t->t->type) {
    case OUT:
    case IN:
    case LOOP_BGN: arithmetic = false; shift = false; break;
    case LOOP_END: loop = false; break;
    case INC:
    case DEC: shift = false; break;
    case RIGHT: arithmetic = false; r++; break;
    case LEFT: arithmetic = false; l++; break;
    }
  }

  if (arithmetic) op->type = ZERO;
  else if (shift) {
    bool right = r > l;
    if (!compile) {
      if (right) op->type = R_ZERO;
      else op->type = L_ZERO;
      op->count = right ? r - l : l - r;      
    } else to_token(t, point);
  } else to_token(t, point);
}


BF_DEF bool simulate_optimized_program(Program *prog, Tokenizer *t) {
  if (!first_op(prog)) return false;
  prog->p = tape;
  putchar(10);
  patch_program_jmp(prog);
  while (true) {
    to_token(t, prog->op->t);
    switch (prog->op->type) {
    case R:
      prog->p += prog->op->count;
      if (prog->p - tape >= TAPE_SIZE) {
        diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", TAPE_SIZE);
        return false;
      }
      break;
    case L:
      prog->p -= prog->op->count;
      if (prog->p - tape < 0) {
        diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
        return false;
      }
      break;
    case I: (*prog->p) += prog->op->count; break;
    case D: (*prog->p) -= prog->op->count; break;
    case OP_OUT: putc((*prog->p), stdout); break;
    case OP_IN: (*prog->p) = getchar(); break;
    case LP_BGN:
      if (*prog->p == 0) {
        if (!to_op(prog, prog->op->jmp - 1)) return false;
      }
      break;
    case LP_END:
      if (*prog->p != 0) {
        if (!to_op(prog, prog->op->jmp - 1)) return false;
      }
      break;
    case ZERO: (*prog->p) = 0; break;
    case R_ZERO: while (*prog->p) prog->p++; break;
    case L_ZERO: while (*prog->p) prog->p--; break;
    case OP_TYPES: diag_err(t, "%s", "Unreachable!\n"); return false;
    default: assert(0 && "Unreachable!\n"); return false; break;
    }
    if (!next_op(prog)) break;
  }
  return true;
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
  prog->p = tape;
  patch_program_jmp(prog);
  size_t max = 1;
  if (verbose) fprintf(f, ";; Make rsi point at the tape.\n");
  fprintf(f, "%smov rsi,%stape\n", tab, spc);
  if (verbose) fprintf(f, ";; Reads and writes operate on only one character, so just move 1 to rdx once and for all.\n");
  fprintf(f, "%smov rdx,%s1\n", tab, spc);
  for (size_t i = 0; i < prog->count; i++) {
    Op *op = prog->items + i;
    to_token(t, op->t);
    /* size_t count = op->count; */
    prog->p = tape + pointer;
    if (pointer > max) max = pointer;
    if (noisy)
      fprintf(f, ";; %s:%zu:%zu: %c\n", t->path, t->t->row, t->t->col, t->t->c);
    switch (op->type) {
    case RIGHT:
    case LEFT:
      if (!gen_optimized_move_pointer_fasm(prog, t, f)) return false; break;
    case INC:
    case DEC: gen_optimized_arithmetic_fasm(prog, f); break;
    case OUT: gen_write_fasm(t, f); break;
    case IN: gen_read_fasm(t, f); break;
    case LOOP_BGN: gen_lp_bgn_fasm(t, f, op->jmp, prog->index); break;
    case LOOP_END: gen_lp_end_fasm(t, f, op->jmp, prog->index); break;
    case ZERO:
      if (verbose) fprintf(f, "%s;; Set the current character to 0.\n", tab);
      fprintf(f, "%smov byte%s[rsi],%s0\n", tab, spc, spc);
      break;
    case R_ZERO: case L_ZERO: break; // These don't make sense when compiling.
    case OP_TYPES:
    default: diag_err(t, "%s", "Unreachable!\n"); return false; break;
    }
    if (!next_op(prog)) break;
  }
  
  if (extra_verbose) fprintf(f, "%s;; 60 is the index of the exit syscall, so move 60 to `rax`.\n", tab);
  fprintf(f, "%smov rax,%s60\n", tab, spc);
    if (extra_verbose) fprintf(f, "%s;; exit code 0 indicates success, so move 0 to `rdi`.\n", tab);
  fprintf(f, "%smov rdi,%s0\n", tab, spc);
    if (verbose) fprintf(f, "%s;; call the exit syscall, with exit code 0, to indicate the program ran successfully and exited without error.\n", tab);
  fprintf(f, "%ssyscall\n", tab);

  fprintf(f, "segment readable writable\ntape db %zu dup (0)", max);
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
  int ret = system(command);
  if (ret == -1) return false;

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
  ret = system(cmd);
  if (ret == -1) return false;
  
  if (run) {
    if (noisy) printf("[INFO] %s\n", final_out);
    ret = system(final_out);
    if (ret == -1) return false;
  }
  
  return true;
}

BF_DEF bool compile_optimized_program_elf(Program *prog) {
  FILE *f = fopen(output, "wb");
  if (f == NULL) {
    // Maybe do a better error reporting here?
    fprintf(stderr, "Could not open file `%s`.\n", output);
    if (f) fclose(f);
    return false;
  }

  // Mandatory headers.
  Elf64_Ehdr elfh = gen_elf_header();
  Elf64_Phdr entry = gen_elf_entry_program_header();
  Elf64_Phdr thdr = gen_elf_tape_program_header();

  size_t s = fwrite(&elfh, 1, sizeof(elfh), f);
  if (s != sizeof(elfh)) {
    perror("fwrite");
    return false;
  }

  // Point rsi at the tape.
  Bytes start = {0};
  gen_start_elf(&start, true, true);
  
  size_t pointer = 0;
  first_op(prog);
  patch_program_jmp(prog);
  patch_program_jmp_for_binary(prog);

  Bytes insts = {0};
  size_t bin_jmp = 0;
  size_t count = 0;
  size_t max = 1;

  for (size_t i = 0; i < prog->count; i++) {
    Op *op = prog->items + i;
    bin_jmp = op->jmp;
    count = op->count;
    if (op->type == R) pointer += count;
    else if (op->type == L) pointer -= count;
    if (pointer > max) max = pointer;
    switch (op->type) {
    case R: case L: gen_optimized_move_pointer_elf(op, &insts); break;
    case I: case D: gen_optimized_arithmetic_elf(op, &insts); break;
    case OP_OUT: case OP_IN: gen_read_write_syscall_elf(&insts, op->type); break;
    case LP_BGN: case LP_END: gen_lp_elf(&insts, op->type, op->jmp); break;
    case ZERO: gen_zero_elf(&insts); break;
      /* append_bytes(&insts, "\xc6\x06\x00", 3); // mov byte [rsi], 0 */
      /* break; */
    case R_ZERO: case L_ZERO: // these don't make sense when compiling
    case OP_TYPES: default: assert (0 && "unreachable"); break;
    }
  }

  gen_exit_ok_elf(&insts, false, false);
  patch_tape_elf(&entry, &thdr, &start, insts.count);
  if (!write_headers_to_elf(f, entry, thdr)) return false;
  // patch tape size
  thdr.p_filesz = max;
  thdr.p_memsz = max;
  if (!write_bytes_to_elf(f, &start, &insts, thdr.p_filesz)) return false;
  fclose(f);
  if (noisy) printf("[INFO] Successfully generated binary %s\n", output);
  if (!make_executable_and_optionally_run()) return false;
  return true;
}

BF_DEF bool compile_size_optimized_program_elf(Program *prog) {
  FILE *f = fopen(output, "wb");
  if (f == NULL) {
    // Maybe do a better error reporting here?
    fprintf(stderr, "Could not open file `%s`.\n", output);
    if (f) fclose(f);
    return false;
  }

  // Mandatory headers.
  Elf64_Ehdr elfh = gen_elf_header();
  Elf64_Phdr entry = gen_elf_entry_program_header();
  Elf64_Phdr thdr = gen_elf_tape_program_header();

  size_t s = fwrite(&elfh, 1, sizeof(elfh), f);
  if (s != sizeof(elfh)) {
    perror("fwrite");
    return false;
  }

  size_t pointer = 0;
  first_op(prog);
  patch_program_jmp(prog);
  bool write = false;
  bool read = false;
  patch_program_jmp_for_size_opt_bin(prog, &write, &read);
  
  Bytes start = {0};
  gen_start_elf(&start, write, read);
  
  Bytes insts = {0};
  size_t max = 1;
  for (size_t i = 0; i < prog->count; i++) {
    Op *op = prog->items + i;
    if (op->type == R) pointer += op->count;
    else if (op->type == L) pointer -= op->count;
    if (pointer > max) max = pointer;

    /* if (pointer > max) max = pointer; */
    switch (op->type) {
    case R: case L: gen_size_optimized_move_pointer_elf(op, &insts); break;
    case I: case D: gen_size_optimized_arithmetic_elf(op, &insts); break;
    case OP_OUT: case OP_IN:
      gen_size_optimized_read_write_syscall_elf(op, &insts, &write, &read); break;
    case LP_BGN: case LP_END: gen_size_optimized_lp_elf(op, &insts); break;
    case ZERO: gen_zero_elf(&insts); break;
    case R_ZERO: case L_ZERO: // these don't make sense when compiling
    case OP_TYPES: default: assert (0 && "unreachable"); break;
    }
  }
  
  gen_exit_ok_elf(&insts, write, read);
  patch_tape_elf(&entry, &thdr, &start, insts.count);
  if (!write_headers_to_elf(f, entry, thdr)) return false;
  // patch tape size
  thdr.p_filesz = max;
  thdr.p_memsz = max;
  if (!write_bytes_to_elf(f, &start, &insts, thdr.p_filesz)) return false;
  fclose(f);
  if (noisy) printf("[INFO] Successfully generated binary %s\n", output);
  if (!make_executable_and_optionally_run()) return false;
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
}

BF_DEF void patch_program_jmp_for_binary(Program *prog) {
  size_t point = prog->index;

  while (true) {
    if (prog->op->type == LP_BGN) patch_op_jmp_for_binary(prog);
    if (!next_op(prog)) break;
  }
  
  to_op(prog, point);
}

BF_DEF void patch_program_jmp_for_size_opt_bin(Program *prog, bool *w, bool *r) {
  size_t point = prog->index;

  bool write = false;
  bool read = false;
  for (size_t i = 0; i < prog->count; i++) {
    Op *op = prog->items + i;
    if (op->type == OP_OUT) {
      write = true;
      break;
    } else if (op->type == OP_IN) {
      read = true;
      break;
    }
  }

  *w = write;
  *r = read;
  
  size_t count = 0;
  for (size_t i = 0; i < prog->count; i++) {
    Op *op = prog->items + i;
    count = op->count;
    switch (op->type) {
    case R:
    case L:
      // If more than 255 is added to or subtracted from rsi, the long
      // form needs to be used.
      if (count > 0xFF) op->b = 7;
      // inc/dec rsi is 3 bytes, but add/sub rsi is 4 bytes if it can
      // fit, so that's more efficient when count is 2 or more.
      else if (count > 1) op->b = 4;
      else op->b = 3;
      break;
    case I:
    case D:
      // inc/dec byte [rsi] is 2 bytes, but add/sub byte [rsi] is 3
      // bytes if it can fit, so that's more efficient when count is 2
      // or more.
      if (count > 1) op->b = 3;
      else op->b = 2;
      break;
    case OP_OUT:
      // If write was the last syscall, no need to change anything
      // about the register, so the size only needs to be the syscall,
      // which is 2 bytes.
      if (write) op->b = 2;
      else op->b = 8; // 3 for `inc rax`, 3 for `inc rdi`, and 2 for `syscall`
      write = true;
      read = false;
      break;
    case OP_IN:
      // Same reasoning as above, just the other way around.
      if (read) op->b = 2;
      else op->b = 8;
      read = true;
      write = false;
      break;
    case LP_BGN: case LP_END: op->b = 7; break; // These are patched later.
    case ZERO: op->b = 3; break; // mov byte [rsi], 0 is only 3 bytes.
    case R_ZERO: case L_ZERO: case OP_TYPES: default:
      assert(0 && "UNREACHABLE");
    }
  }

  size_t nest = 0;
  size_t max_nest = 0;
  
  for (size_t i = 0; i < prog->count; i++) {
    Op *op = prog->items + i;
    if (op->type == LP_BGN) nest++;
    if (op->type == LP_END) nest--;
    if (nest > max_nest) max_nest = nest;
  }

  while (max_nest) {
    for (size_t i = 0; i < prog->count; i++) {
      Op *op = prog->items + i;
      if (op->type == LP_BGN) {
        nest++;
        if (nest == max_nest) {
          to_op(prog, i);
          patch_op_jmp_for_size_opt_bin(prog);
        }
      }
      if (op->type == LP_END) nest--;
    }
    max_nest--;
  }

  to_op(prog, point);
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

BF_DEF void patch_op_jmp_for_binary(Program *prog) {
  size_t point = prog->index;
  size_t jmp = prog->op->jmp;
  size_t bin_jmp = 0;
  for (size_t i = point; i < jmp; i++) {
    Op *op = prog->items + i;
    size_t bin_len = bin_len_by_op_type(op->type);
    bin_jmp += bin_len;
  }

  prog->op->jmp = bin_jmp;

  bin_jmp = 0;
  
  for (size_t i = jmp; i > point; i--) {
    Op *op = prog->items + i;
    size_t bin_len = bin_len_by_op_type(op->type);
    bin_jmp -= bin_len;
  }

  to_op(prog, jmp);
  prog->op->jmp = bin_jmp;
  to_op(prog, point);
}

BF_DEF void patch_op_jmp_for_size_opt_bin(Program *prog) {
  size_t point = prog->index;
  size_t jmp = prog->op->jmp;
  size_t bin_jmp = 0;
  for (size_t i = point; i < jmp; i++) {
    Op *op = prog->items + i;
    bin_jmp += op->b;
  }

  if (bin_jmp > 0x7F) {
    prog->op->b += 4;
    bin_jmp += 4;
  }

  prog->op->jmp = bin_jmp;

  bin_jmp = 0;
  
  for (size_t i = jmp; i > point; i--) {
    Op *op = prog->items + i;
    bin_jmp -= op->b;
  }

  to_op(prog, jmp);

  if (-bin_jmp > 0x80) {
    prog->op->b += 4;
    bin_jmp -= 4;
  }
  
  prog->op->jmp = bin_jmp;
  to_op(prog, point);
}

BF_DEF bool gen_optimized_move_pointer_fasm(Program *prog, Tokenizer *t, FILE *f) {
  bool right = prog->op->type == R;
  
  if (verbose) fprintf(f, "%s;; %s rsi, which points to the tape, by %zu.\n", tab, right ? "increment" : "decrement", prog->op->count);
  fprintf(f, "%s%s rsi,%s%zu\n", tab, right ? "add" : "sub", spc, prog->op->count);
  if (right) pointer += prog->op->count; else pointer -= prog->op->count;
  
  if (pointer >= TAPE_SIZE) {
    diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", TAPE_SIZE);
    return false;
  } else if (pointer < 0) {
    diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
    return false;
  }
  
  return true;
}

BF_DEF void gen_optimized_arithmetic_fasm(Program *prog, FILE *f) {
  bool add = prog->op->type == I;
  if (verbose) fprintf(f, "%s;; %s the current character by %zu.\n", tab, add ? "increment" : "decrement", prog->op->count);
  fprintf(f, "%s%s byte%s[rsi],%s%zu\n", tab, add ? "add" : "sub", spc, spc, prog->op->count);
  if (add) (*prog->p) += prog->op->count; else (*prog->p) -= prog->op->count;
}

BF_DEF void gen_optimized_move_pointer_elf(Op *op, Bytes *s) {
  append_bytes(s, "\x48\x81", 2); // add or sub, seemingly inferred by the next byte
  if (op->type == R) da_append(s, 0xC6); // rsi
  else da_append(s, 0xEE); // rsi
  gen_little_endian(s, op->count, 4); // how much to add or sub
}

BF_DEF void gen_size_optimized_move_pointer_elf(Op *op, Bytes *s) {
  if (op->count > 0xFF) {
    gen_optimized_move_pointer_elf(op, s);
  } else if (op->count > 1) {
    append_bytes(s, "\x48\x83", 2); // add or sub, inferred by the nest byte
    if (op->type == R) da_append(s, 0xC6); // rsi
    else da_append(s, 0xEE); // rsi
    da_append(s, (char) op->count); // how much to sub
  } else {
    gen_move_pointer_elf(s, op->type);
  }
}

BF_DEF void gen_optimized_arithmetic_elf(Op *op, Bytes *s) {
  da_append(s, 0x80); // add or sub [byte], seemingly inferred by the next byte
  if (op->type == I) da_append(s, 0x06); // byte [rsi].
  else da_append(s, 0x2E); // byte [rsi].
  da_append(s, (char) (op->count % 256)); // how much to add or sub
}

BF_DEF void gen_size_optimized_arithmetic_elf(Op *op, Bytes *s) {
  if (op->count > 1) gen_optimized_arithmetic_elf(op, s);
  else gen_arithmetic_elf(s, op->type);
}

BF_DEF void gen_zero_elf(Bytes *s) {
  append_bytes(s, "\xc6\x06\x00", 3); // mov byte [rsi], 0
}

BF_DEF void gen_size_optimized_read_write_syscall_elf(Op *op, Bytes *s, bool *w, bool *r) {
  if (op->type == OP_OUT) {
    if (*r) {
      append_bytes(s, "\x48\xff\xc0", 3); // inc rax
      append_bytes(s, "\x48\xff\xc7", 3); // inc rdi
    }
  } else {
    if (*w) {
      append_bytes(s, "\x48\xff\xc8", 3); // dec rax
      append_bytes(s, "\x48\xff\xcf", 3); // dec rdi
    }
  }
  gen_syscall_elf(s);
  if (*w) *w = !*w;
  if (*r) *r = !*r;
}

BF_DEF void gen_size_optimized_lp_elf(Op *op, Bytes *s) {
  append_bytes(s, "\x48\x8b\x1e", 3); // mov rbx, QWORD [rsi]
  append_bytes(s, "\x84\xdb", 2);     // test bl, bl
  bool bgn = op->type == LP_BGN;
  size_t bin_jmp = op->jmp;
  bool long_form = bgn && bin_jmp > 0x7F || !bgn && -bin_jmp > 0x80;
  if (long_form) {
    da_append(s, 0x0F); // jz or jnz, seemingly inferred by the next byte
    if (bgn) da_append(s, 0x84); // jz
    else da_append(s, 0x85); // jnz
    gen_little_endian(s, bin_jmp, 4); // how many bytes to jump
  } else {
    if (bgn) da_append(s, 0x74); // jz
    else da_append(s, 0x75); // jnz
    char j = bin_jmp & 0xFF;
    da_append(s, j); // // how many bytes to jump
  }
}

BF_DEF bool check_optimized_program_bounds(const Program *prog) {
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
