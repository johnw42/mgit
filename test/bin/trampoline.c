#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

//#define SHELL "/bin/echo"
#define SHELL "/bin/sh"
#define COMMAND_PREFIX "IFS=; exec "
#define MAX_LINE_LENGTH 4096

int main(int argc, char **argv) {
  char last_char = argv[0][strlen(argv[0]) - 1];
  if (last_char < '2' || last_char > '9') {
    errx(1, "invalid name of trampoline binary: %s", argv[0]);
  }
  int line_number = last_char - '0';
  if (argc < 2) {
    errx(1, "not enough arguments");
  }
  char *script_name = argv[1];
  FILE *file = fopen(script_name, "r");
  if (file == NULL) {
    err(1, "error opening %s", script_name);
  }
  char line[MAX_LINE_LENGTH];
  for (int i = 0; i < line_number; i++) {
    if (fgets(line, sizeof(line), file) == NULL) {
      errx(1, "file ends prematurely: %s", script_name);
    }
  }
  char *start = line;
  while (start[0] && start[1]) {
    if (start[0] == '#' && start[1] == '!') {
      char command[MAX_LINE_LENGTH + strlen(COMMAND_PREFIX)];
      strcpy(command, COMMAND_PREFIX);
      strcat(command, start + 2);
      char *script_argv[argc+4];
      script_argv[0] = SHELL;
      script_argv[1] = "-c";
      script_argv[2] = command;
      for (int i = 1; i <= argc; i++) {
        script_argv[i+2] = argv[i];
      }
      script_argv[argc+3] = NULL;
      execv(SHELL, script_argv);
    }
    ++start;
  }
  errx(1, "no command found on line %d of %s", line_number, script_name);
}
