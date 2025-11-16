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

DA_MAKE(char, Bytes);

typedef enum {
  NOTE,
  WARNING,
  ERROR,
} ReportLevel;

BF_DEF void append_bytes(Bytes *s, const char *bytes, size_t len);

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

BF_DEF bool simulate_optimized_program(Program *prog, Tokenizer *t);
BF_DEF bool compile_optimized_program_fasm(Program *prog, Tokenizer *t);
BF_DEF bool compile_optimized_program_elf(Program *prog);

BF_DEF bool patch_program_jmp(Program *prog);
BF_DEF void patch_program_jmp_for_binary(Program *prog);
BF_DEF bool patch_op_jmp(Program *prog);
BF_DEF void patch_op_jmp_for_binary(Program *prog);

BF_DEF bool gen_optimized_move_pointer_fasm(Program *prog, Tokenizer *t, FILE *f);
BF_DEF void gen_optimized_arithmetic_fasm(Program *prog, Tokenizer *t, FILE *f);

BF_DEF bool check_optimized_program_bounds(Program *prog);
BF_DEF bool first_op(Program *prog);
BF_DEF bool next_op(Program *prog);
BF_DEF bool prev_op(Program *prog);
BF_DEF bool to_op(Program *prog, size_t index);
BF_DEF bool program_jump(Program *prog, const Op *jmp);

char tape[TAPE_SIZE];
bool opt = false;
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

size_t count = 0;
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
    }
  }

  if (def_out) remove_ext();
  
  if (!tokenize_file(&t)) return 1;
  Program prog = {0};
  if (opt) if (!optimize_program(&t, &prog)) return 1;
  
  if (compile) {
    if (direct_to_binary) {
      if (opt) {
        if (!compile_optimized_program_elf(&prog)) return 1;
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

BF_DEF void append_bytes(Bytes *s, const char *bytes, size_t len) {
  for (size_t i = 0; i < len; i++) da_append(s, *(bytes + i));
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
  append_bytes(&start, "\x48\xc7", 2); // MOV
  append_bytes(&start, "\xc6", 1); // rsi
  append_bytes(&start, "\0\0\0\0", 4); // tape address, needs to be patched
  append_bytes(&start, "\x48\xc7", 2); // MOV
  append_bytes(&start, "\xc2", 1); // rdx
  append_bytes(&start, "\1\0\0\0", 4); // 1
  
  size_t pointer = 0;
  first_token(t);
  t->p = tape;
  patch_tokenizer_jmp(t);
  patch_tokenizer_jmp_for_binary(t);

  Bytes insts = {0};
  size_t bin_jmp = 0;
  
  while (true) {
    bin_jmp = t->t->jmp;
    switch (t->t->type) {
    case RIGHT:
      // 48ff for INC
      // c6 for rsi
      append_bytes(&insts, "\x48\xff\xc6", 3); 
      break;
    case LEFT:
      // 48ff for DEC
      // ce for rsi
      append_bytes(&insts, "\x48\xff\xce", 3); 
      break;
    case INC:
      // inc byte[rsi]
      append_bytes(&insts, "\xfe\x06", 2);
      break;
    case DEC:
      // dec byte[rsi]
      append_bytes(&insts, "\xfe\x0e", 2);
      break;
    case OUT:
      // mov rax, 1
      append_bytes(&insts, "\x48\xc7\xc0\1\0\0\0", 7);
      // mov rdi, 1
      append_bytes(&insts, "\x48\xc7\xc7\1\0\0\0", 7);
      // syscall
      append_bytes(&insts, "\x0f\x05", 2);
      break;
    case IN:
      // mov rax, 0
      append_bytes(&insts, "\x48\xc7\xc0\0\0\0\0", 7);
      // mov rdi, 0
      append_bytes(&insts, "\x48\xc7\xc7\0\0\0\0", 7);
      // syscall
      append_bytes(&insts, "\x0f\x05", 2);
      break;
    case LOOP_BGN:
      // mov rax, QWORD [rsi]
      append_bytes(&insts, "\x48\x8b\x06", 3);
      // test al, al
      append_bytes(&insts, "\x84\xc0", 2);
      // jz
      append_bytes(&insts, "\x0f\x84", 2);

      for (size_t i = 0; i < 4; i++) {
        char c = (bin_jmp >> (i * 8)) & 0xFF;
        da_append(&insts, c);
      }
      break;
    case LOOP_END:
      // mov rax, QWORD [rsi]
      append_bytes(&insts, "\x48\x8b\x06", 3);
      // test al, al
      append_bytes(&insts, "\x84\xc0", 2);
      // jnz
      append_bytes(&insts, "\x0f\x85", 2);

      for (size_t i = 0; i < 4; i++) {
        char c = (bin_jmp >> (i * 8)) & 0xFF;
        da_append(&insts, c);
      }
      break;
    case TOKEN_TYPES: default: assert (0 && "unreachable"); break;
    }

    if (!next_token(t)) break;
  }

  // exit with exit code 0
  append_bytes(&insts, "\x48\xc7\xc0", 3); // mov rax
  append_bytes(&insts, "\x3c\0\0\0", 4); // 60
  append_bytes(&insts, "\x48\xc7\xc7", 3); // mov rdi
  append_bytes(&insts, "\0\0\0\0", 4); // 0
  append_bytes(&insts, "\x0f\5", 2); // syscall

  // patch entry.p_filesz
  entry.p_filesz = 176 + start.count + insts.count;
  entry.p_memsz = entry.p_filesz;

  // patch tape.vaddr
  thdr.p_vaddr += entry.p_filesz;
  thdr.p_paddr = thdr.p_vaddr;
  thdr.p_offset = entry.p_filesz;

  // patch the pointer to the tape.
  size_t tape_pointer = thdr.p_vaddr;
  for (size_t i = 0; i < 4; i++) {
    start.items[3 + i] = (tape_pointer >> (i * 8)) & 0xFF;
  }
  
  s = fwrite(&entry, 1, sizeof(entry), f);
  if (s != sizeof(entry)) {
    perror("fwrite");
    return false;
  }
  
  s = fwrite(&thdr, 1, sizeof(thdr), f);
  if (s != sizeof(thdr)) {
    perror("fwrite");
    return false;
  }

  s = fwrite(start.items, 1, start.count, f);
  if (s != start.count) {
    perror("fwrite");
    return false;
  }
  
  s = fwrite(insts.items, 1, insts.count, f);
  if (s != insts.count) {
    perror("fwrite");
    return false;
  }

  s = fwrite(tape, 1, thdr.p_filesz, f);
  if (s != thdr.p_filesz) {
    perror("fwrite");
    return false;
  }

  if (noisy) printf("[INFO] Successfully generated binary %s\n", output);

  const char *cmd = "chmod +x";
  size_t cmd_len = strlen(cmd);
  size_t command_len = out_len + cmd_len + 1;
  char *command = malloc(command_len);
  snprintf(command, command_len, "%s %s", cmd, output);
  if (noisy) printf("[INFO] %s\n", command);
  system(command);
  
  fclose(f);

  if (run) {
    if (noisy) printf("[INFO] %s\n", output);
    system(output);
  }
  
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
  first_op(prog);
  prog->p = tape;
  patch_program_jmp(prog);
  while (true) {
    tokenizer_jump(t, prog->op->t);
    switch (prog->op->type) {
    case RIGHT:
      prog->p += prog->op->count;
      if (prog->p - tape >= TAPE_SIZE) {
        diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", TAPE_SIZE);
        return false;
      }
      break;
    case LEFT:
      prog->p -= prog->op->count;
      if (prog->p - tape < 0) {
        diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
        return false;
      }
      break;
    case INC: (*prog->p) += prog->op->count; break;
    case DEC: (*prog->p) -= prog->op->count; break;
    case OUT: putc((*prog->p), stdout); break;
    case IN: (*prog->p) = getchar(); break;
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
    case ZERO: (*prog->p) = 0; break;
    case R_ZERO: while (*prog->p) prog->p++; break;
    case L_ZERO: while (*prog->p) prog->p--; break;
    case OP_TYPES: diag_err(t, "%s", "Unreachable!\n"); return false;
    default: return false; break;
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
  size_t max = 0;
  if (verbose) fprintf(f, ";; Make rsi point at the tape.\n");
  fprintf(f, "%smov rsi,%stape\n", tab, spc);
  if (verbose) fprintf(f, ";; Reads and writes operate on only one character, so just move 1 to rdx once and for all.\n");
  fprintf(f, "%smov rdx,%s1\n", tab, spc);
  while (true) {
    tokenizer_jump(t, prog->op->t);
    count = prog->op->count;
    prog->p = tape + pointer;
    if (pointer > max) max = pointer;
    if (noisy)
      fprintf(f, ";; %s:%zu:%zu: %c\n", t->path, t->t->row, t->t->col, t->t->c);
    switch (prog->op->type) {
    case RIGHT: 
    case LEFT: if (!gen_optimized_move_pointer_fasm(prog, t, f)) return false; break;
    case INC:
    case DEC: gen_optimized_arithmetic_fasm(prog, t, f); break;
    case OUT: gen_write_fasm(t, f); break;
    case IN: gen_read_fasm(t, f); break;
    case LOOP_BGN: gen_lp_bgn_fasm(t, f, prog->op->jmp, prog->index); break;
    case LOOP_END: gen_lp_end_fasm(t, f, prog->op->jmp, prog->index); break;
    case ZERO:
      if (verbose) fprintf(f, "%s;; Set the current character to 0.\n", tab);
      fprintf(f, "%smov byte%s[rsi],%s0\n", tab, spc, spc);
      break;
    case R_ZERO: case L_ZERO: break; // These don't make sense when compiling.
    case OP_TYPES: diag_err(t, "%s", "Unreachable!\n"); return false;
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
  append_bytes(&start, "\x48\xc7", 2); // MOV
  append_bytes(&start, "\xc6", 1); // rsi
  append_bytes(&start, "\0\0\0\0", 4); // tape address, needs to be patched
  append_bytes(&start, "\x48\xc7", 2); // MOV
  append_bytes(&start, "\xc2", 1); // rdx
  append_bytes(&start, "\1\0\0\0", 4); // 1
  
  size_t pointer = 0;
  /* first_token(t); */
  first_op(prog);
  patch_program_jmp(prog);
  patch_program_jmp_for_binary(prog);

  Bytes insts = {0};
  size_t bin_jmp = 0;
  size_t count = 0;
  size_t max = 0;
  
  while (true) {
    bin_jmp = prog->op->jmp;
    count = prog->op->count;
    if (pointer > max) max = pointer;
    switch (prog->op->type) {
    case R:
      // 4881 for ADD
      // c6 for rsi
      append_bytes(&insts, "\x48\x81\xc6", 3);
      
      for (size_t i = 0; i < 4; i++) {
        char c = (count >> (i * 8)) & 0xFF;
        da_append(&insts, c);
      }

      pointer += count;
      break;
    case L:
      // 4881 for SUB
      // ee for rsi
      append_bytes(&insts, "\x48\x81\xee", 3);
      
      for (size_t i = 0; i < 4; i++) {
        char c = (count >> (i * 8)) & 0xFF;
        da_append(&insts, c);
      }

      pointer -= count;
      break;
    case I:
      append_bytes(&insts, "\x80\x06", 2); // add [byte] rsi
      da_append(&insts, (char) (count % 256)); // how much to add
      break;
    case D:
      append_bytes(&insts, "\x80\x2e", 2); // sub [byte] rsi
      da_append(&insts, (char) (count % 256)); // how much to sub
      break;
    case OP_OUT:
      // mov rax, 1
      append_bytes(&insts, "\x48\xc7\xc0\1\0\0\0", 7);
      // mov rdi, 1
      append_bytes(&insts, "\x48\xc7\xc7\1\0\0\0", 7);
      // syscall
      append_bytes(&insts, "\x0f\x05", 2);
      break;
    case OP_IN:
      // mov rax, 0
      append_bytes(&insts, "\x48\xc7\xc0\0\0\0\0", 7);
      // mov rdi, 0
      append_bytes(&insts, "\x48\xc7\xc7\0\0\0\0", 7);
      // syscall
      append_bytes(&insts, "\x0f\x05", 2);
      break;
    case LP_BGN:
      // mov rax, QWORD [rsi]
      append_bytes(&insts, "\x48\x8b\x06", 3);
      // test al, al
      append_bytes(&insts, "\x84\xc0", 2);
      // jz
      append_bytes(&insts, "\x0f\x84", 2);

      for (size_t i = 0; i < 4; i++) {
        char c = (bin_jmp >> (i * 8)) & 0xFF;
        da_append(&insts, c);
      }
      break;
    case LP_END:
      // mov rax, QWORD [rsi]
      append_bytes(&insts, "\x48\x8b\x06", 3);
      // test al, al
      append_bytes(&insts, "\x84\xc0", 2);
      // jnz
      append_bytes(&insts, "\x0f\x85", 2);

      for (size_t i = 0; i < 4; i++) {
        char c = (bin_jmp >> (i * 8)) & 0xFF;
        da_append(&insts, c);
      }
      break;
    case ZERO:
      append_bytes(&insts, "\xc6\x06\x00", 3); // mov byte [rsi], 0
      break;
    case R_ZERO: case L_ZERO: break; // these don't make sense when compiling
    case OP_TYPES: default: assert (0 && "unreachable"); break;
   }

    if (!next_op(prog)) break;
  }

  // exit with exit code 0
  append_bytes(&insts, "\x48\xc7\xc0", 3); // mov rax
  append_bytes(&insts, "\x3c\0\0\0", 4); // 60
  append_bytes(&insts, "\x48\xc7\xc7", 3); // mov rdi
  append_bytes(&insts, "\0\0\0\0", 4); // 0
  append_bytes(&insts, "\x0f\5", 2); // syscall

  // patch entry.p_filesz
  entry.p_filesz = 176 + start.count + insts.count;
  entry.p_memsz = entry.p_filesz;

  // patch tape.vaddr
  thdr.p_vaddr += entry.p_filesz;
  thdr.p_paddr = thdr.p_vaddr;
  thdr.p_offset = entry.p_filesz;

  // patch tape size
  thdr.p_filesz = max;
  thdr.p_memsz = max;

  // patch the pointer to the tape.
  size_t tape_pointer = thdr.p_vaddr;
  for (size_t i = 0; i < 4; i++) {
    start.items[3 + i] = (tape_pointer >> (i * 8)) & 0xFF;
  }

  s = fwrite(&entry, 1, sizeof(entry), f);
  if (s != sizeof(entry)) {
    perror("fwrite");
    return false;
  }
  
  s = fwrite(&thdr, 1, sizeof(thdr), f);
  if (s != sizeof(thdr)) {
    perror("fwrite");
    return false;
  }

  s = fwrite(start.items, 1, start.count, f);
  if (s != start.count) {
    perror("fwrite");
    return false;
  }
  
  s = fwrite(insts.items, 1, insts.count, f);
  if (s != insts.count) {
    perror("fwrite");
    return false;
  }

  s = fwrite(tape, 1, thdr.p_filesz, f);
  if (s != thdr.p_filesz) {
    perror("fwrite");
    return false;
  }

  if (noisy) printf("[INFO] Successfully generated binary %s\n", output);

  const char *cmd = "chmod +x";
  size_t cmd_len = strlen(cmd);
  size_t command_len = out_len + cmd_len + 1;
  char *command = malloc(command_len);
  snprintf(command, command_len, "%s %s", cmd, output);
  if (noisy) printf("[INFO] %s\n", command);
  system(command);
  
  fclose(f);

  if (run) {
    if (noisy) printf("[INFO] %s\n", output);
    system(output);
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

BF_DEF void patch_program_jmp_for_binary(Program *prog) {
  size_t point = prog->index;

  while (true) {
    if (prog->op->type == LP_BGN) patch_op_jmp_for_binary(prog);
    if (!next_op(prog)) break;
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

BF_DEF bool gen_optimized_move_pointer_fasm(Program *prog, Tokenizer *t, FILE *f) {
  bool right = prog->op->type == R;
  
  if (verbose) fprintf(f, "%s;; %s rsi, which points to the tape, by %zu.\n", tab, right ? "increment" : "decrement", count);
  fprintf(f, "%s%s rsi,%s%zu\n", tab, right ? "add" : "sub", spc, count);
  if (right) pointer += count; else pointer -= count;
  
  if (pointer >= TAPE_SIZE) {
    diag_err(t, "Stack overflow! The allocated array has only %d elements, no more!\n", TAPE_SIZE);
    return false;
  } else if (pointer < 0) {
    diag_err(t, "%s", "Stack underflow! Cannot move pointer to before its starting point!\n");
    return false;
  }
  
  return true;
}

BF_DEF void gen_optimized_arithmetic_fasm(Program *prog, Tokenizer *t, FILE *f) {
  bool add = prog->op->type == I;
  if (verbose) fprintf(f, "%s;; %s the current character by %zu.\n", tab, add ? "increment" : "decrement", count);
  fprintf(f, "%s%s byte%s[rsi],%s%zu\n", tab, add ? "add" : "sub", spc, spc, count);
  if (add) (*prog->p) += count; else (*prog->p) -= count;
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
