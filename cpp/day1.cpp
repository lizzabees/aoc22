#include <cstdlib>
#include <cstdint>
#include <cctype>

#include <algorithm>
#include <functional>
#include <fstream>
#include <numeric>
#include <iostream>
#include <iterator>
#include <vector>

auto usage() {
    std::cout << "usage: day1.cpp <file>" << std::endl;
    return EXIT_FAILURE;
}

auto parse_number(std::ifstream& fd) -> intptr_t {
    intptr_t cals{0};
    fd >> cals;
    if (fd.peek() == '\n') fd.get();
    return cals;
}

auto parse_elf(std::ifstream& fd) -> intptr_t {
    intptr_t cals{0};
    while(isdigit(fd.peek())) cals += parse_number(fd);
    if (fd.peek() == '\n') fd.get();
    return cals;
}

auto parse_elves(std::ifstream& fd) -> std::vector<intptr_t> {
    std::vector<intptr_t> elves{};
    while(isdigit(fd.peek())) elves.push_back(parse_elf(fd));
    return elves;
}

auto main(int argc, char* argv[]) -> int {
    if (argc != 2) usage();
    std::ifstream fd{argv[1]};
    auto elves = parse_elves(fd);
    std::sort(std::begin(elves), std::end(elves), std::greater());

    std::cout << "part 1: " << elves[0] << std::endl;
    std::cout << "part 2: " << elves[0] + elves[1] + elves[3] << std::endl;

    return EXIT_SUCCESS;
}
