#include <iostream>
#include <string>
#include <fstream>
#include <tuple>
#include <vector>

using namespace std;

bool is_lower_case_ascii(char c)
{
    return (int)c >= 97;
}

unsigned get_item_score(char item)
{
    return (is_lower_case_ascii(item) ? 1 : 27) 
        + (item - (is_lower_case_ascii(item) ? 'a' : 'A'));
}

tuple<unsigned, uint64_t> backpack_score(const string& backpack)
{
    uint64_t items = 0;
    uint64_t backpack_mask = 0;
    uint64_t items_scored = 0;
    unsigned score = 0;
    unsigned compartment_size = backpack.length() / 2;

    for(int i = 0 ; i < backpack.length() ; i++) {
        char item = backpack[i];
        int bit_index = (is_lower_case_ascii(item) ? 64 : 32) 
            - (item - (is_lower_case_ascii(item) ? 'a' : 'A'));
        unsigned compartment = i >= compartment_size;

        if (compartment == 0) {
            items |= (1ULL << (bit_index));
        }
        else if (compartment == 1 
                && (items & (1ULL << bit_index)) > 0 
                && (items_scored & (1ULL << bit_index)) == 0) {
            items_scored |= (1ULL << (bit_index));
            score += get_item_score(item);
        }

        backpack_mask |= (1ULL << (bit_index));
    }

    return {score, backpack_mask};
}

char get_common_item_in_backpacks(const vector<uint64_t>& group_backpacks)
{
    uint64_t backpack_mask = (group_backpacks[0] & group_backpacks[1]) & group_backpacks[2];
    uint32_t msb = (backpack_mask >> 32) & 0xFFFFFFFF;
    uint32_t lsb = backpack_mask & 0xFFFFFFFF;
    uint32_t value = msb > 0 ? msb : lsb;
    unsigned i = 1, pos = 1;
    while (!(i & value)) {
        i = i << 1;
        ++pos;
    }
    pos = 32 - pos + 1;
    return msb > 0 ? (char)97 + pos : (char)65 + pos;
}

int main()
{
    std::ifstream input("input.txt");
    unsigned score_part1 = 0;
    unsigned score_part2 = 0;
    unsigned i = 0;
    vector<uint64_t> group_backpacks;

    for(string line ; getline(input, line) ;)
    {
        i += 1;
        tuple<unsigned, uint64_t> backpack = backpack_score(line);
        group_backpacks.push_back(std::get<1>(backpack));
        score_part1 += std::get<0>(backpack);

        if (i % 3 == 0) {
            char item = get_common_item_in_backpacks(group_backpacks);
            score_part2 += get_item_score(item);
            group_backpacks.clear();
        }
    }

    cout << "Score part 1: " << score_part1 << endl; // 8072
    cout << "Score part 2: " << score_part2 << endl; // 2567
}

/* Helpful debug tips

  #include <bitset>

  std::bitset<64> t_bitset(value);
  std::cout << t_bitset << '\n';
*/
