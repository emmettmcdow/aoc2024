#include <stdio.h>
#include <stdbool.h>

static inline int next_state(int, int);
static inline int next_state(int state, int want) {
  int out = 0;
  if (state == want) {
    out = state + 1;
  } else {
    out = 0;
  }
  return out;
}

enum Command {
  DO,
  DONT,
  MUL
};

static int get_muls(char *);
static int get_muls(char *in) {
  int c, n1, n2;
  unsigned int state = 0;
  int output = 0;
  bool off = false;
  enum Command cmd = DO;

  FILE *file = fopen(in, "r");
  while ((c = fgetc(file)) != EOF) {
    printf("Char: %c, State: %d, Mode: %d\n", (char) c, state, cmd);
      if (off) {
        switch((char) c) {
          case 'd':
            cmd = DO;
            state = 1;
            break;
          case 'o':
            state = next_state(state, 1);
            break;
          case '(':
            state = next_state(state, 2);
            break;
          case ')':
            if (state == 3) {
              printf("Got do\n");
              off = false;
            }
            state = 0;
            break;
          default:
            state = 0;
        }
      
      } else {
        switch((char) c) {
          case 'd':
            cmd = DONT;
            state = 1;
            break;
          case 'o':
            if (cmd == DONT) {
              state = next_state(state, 1);
            } else {
              state = 0;
            }
            break;
          case 'n':
            if (cmd == DONT) {
              state = next_state(state, 2);
            } else {
              state = 0;
            }
            break;
          case '\'':
            if (cmd == DONT) {
              state = next_state(state, 3);
            } else {
              state = 0;
            }
            break;
          case 't':
            if (cmd == DONT) {
              state = next_state(state, 4);
            } else {
              state = 0;
            }
            break;
          case 'm':
            cmd = MUL;
            state = 1;
            n1 = 0;
            n2 = 0;
            break;
          case 'u':
            if (cmd == MUL) {
              state = next_state(state, 1);
            } else {
              state = 0;
            }
            break;
          case 'l':
            if (cmd == MUL) {
              state = next_state(state, 2);
            } else {
              state = 0;
            }
            break;
          case '(':
            if (cmd == DONT) {
              state = next_state(state, 5);
            } else if (cmd == MUL) {
              state = next_state(state, 3);
            } else {
              state = 0;
            }
            break;
          case ',':
            if (cmd == MUL){
              state = next_state(state, 4);
            } else {
              state = 0;
            }
            break;
          case ')':
            if (cmd == DONT) {
              if (state == 6) {
                printf("got don't\n");
                off = true;
              }
            } else if (cmd == MUL) {
              if (state == 5) {
                // Save the result
                printf("Multiplying %d and %d\n", n1, n2);
                output += (n1 * n2);
              }
            }
            state = 0;
            n1 = 0;
            n2 = 0;
            break;
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            if (cmd == MUL) {
              if (state == 4) {
                // char to int
                n1 *= 10;
                n1 += (c - '0');
              } else if (state == 5) {
                n2 *= 10;
                n2 += (c - '0');
              }
            } else {
              n1 = 0;
              n2 = 0;
              state = 0;
            }
            break;
          default:
            state = 0;
        }
      }
  }
  return output;
}

int main() {
  printf("result is: %d\n", get_muls("./input.txt"));
}
