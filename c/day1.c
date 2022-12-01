#include <ctype.h>
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const size_t MAX_ELVES = 3000;
intptr_t elves[MAX_ELVES] = {0};

void usage() {
    puts("usage: day1 <input>");
    exit(EXIT_FAILURE);
}

/* read a newline terminated unsigned number */
intptr_t parse_number(FILE* fd, int* c) {
    intptr_t n = 0;
    while(isdigit(*c)) {
        n = (n * 10) + (*c - '0');
        *c = fgetc(fd);
    }
    if (*c == '\n') *c = fgetc(fd);
    return n;
}

/* read a series of numbers returning total
 * and eat trailing newline */
intptr_t parse_elf(FILE* fd, int* c) {
    intptr_t cals = 0; /* current elf total cals */
    while(isdigit(*c)) cals += parse_number(fd, c);
    if (*c == '\n') *c = fgetc(fd);
    return cals;
}

/* read all elves in the file and put each one's
 * total calories into elves. return number of
 * elves read */
size_t parse_elves(FILE* fd) {
    int c = fgetc(fd);
    size_t elf = 0;
    while(c != EOF) elves[elf++] = parse_elf(fd, &c); 
    return elf;
}

/* sort descending */
int comp(const void* a, const void* b) {
    return *(intptr_t*)b - *(intptr_t*)a;
}

int main(int argc, char* argv[]) {
    if (argc != 2) usage();

    FILE* fd = fopen(argv[1], "r");
    if (!fd) {
        if (errno) fprintf(stderr, "couldn't open %s\n", argv[1]);
        else fprintf(stderr, "error opening %s: %s", argv[1], strerror(errno));
        return EXIT_FAILURE;
    }

    size_t count = parse_elves(fd);
    qsort(elves, count, sizeof(intptr_t), comp);

    printf("part 1: %td\n", elves[0]);
    printf("part 2: %td\n", elves[0]+elves[1]+elves[2]);

    fclose(fd);
    return EXIT_SUCCESS;
}
