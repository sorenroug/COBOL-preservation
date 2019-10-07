/*
 * Display the configuration of COBOL runtime
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define uint8_t unsigned char
#ifdef __STDC__
#define uint32_t unsigned int
#else
#define uint32_t unsigned long
#endif
#define const 

#define HEADER 512

void heading(char *label) {
    printf("%s:\n", label);
}

void onebyte(char *label, int value) {
    printf("%-30s: $%02x - %d\n", label, value, value);
}

int main(int argc, char *argv[]) {
    int argi;
    int num;
    uint32_t crc;
    FILE *inputFp;
    char *fn;
    long filesize;
    uint8_t buffer[HEADER];

    if (argc != 2) {
        fprintf(stderr, "Usage: %s file\n", argv[0]);
        fprintf(stderr, "Display configuration of COBOL runtime files.\n");
        exit(1);
    }

    for (argi = 1; argi < argc; argi++) {
        fn = argv[argi];
        if ((inputFp = fopen(fn,  "rb")) == NULL) {
            fprintf(stderr, "Can't open %s\n", fn);
            exit(1);
        }
        num = fread(buffer, 1, HEADER, inputFp);
        if (num < HEADER) {
            fprintf(stderr, "File too small\n");
            exit(1);
        }
        fclose(inputFp);
        heading("Screen dimensions");
        onebyte("Columns of screen", buffer[0x90] + 1);
        onebyte("Rows of screen", buffer[0x91] + 1);
        heading("Keys");
        onebyte("Carriage return", buffer[0x96]);
        onebyte("Backspace one character", buffer[0x97]);
        onebyte("Forward space one character", buffer[0x98]);
    }
    exit(0);
}
