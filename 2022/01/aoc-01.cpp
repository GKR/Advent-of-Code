#include <tuple>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include <numeric>

using std::vector;
using std::string;
using std::tuple;

// g++ -std=c++17 aoc-01.cpp -o aoc-01 && ./aoc-01

int main()
{
    vector<tuple<unsigned, unsigned>> raindeer;

    std::ifstream t("input.txt");
    std::stringstream buffer;
    buffer << t.rdbuf();
    
    unsigned sum = 0;
    unsigned raindeer_counter = 1;
    string line;

    while (getline(buffer, line, '\n')) 
    {
        if (line.size()) {
            sum += atoi(line.c_str());
        } 
        else 
        {
            raindeer.push_back(std::tuple<unsigned, unsigned>{raindeer_counter, sum});
            raindeer_counter++;
            sum = 0;
        }
    }

    sort(raindeer.begin(), raindeer.end(), [](auto a, const auto b) -> bool { 
        return std::get<1>(a) > std::get<1>(b   ); 
    });

    // Del 1
    std::cout << "Raindeer number " 
        << std::get<0>(raindeer[0]) << " carries most calories (" 
        << std::get<1>(raindeer[0]) << ")" 
        << std::endl;

    // Del 2
    auto range = { 0, 1, 2 };
    std::cout << "Sum calories top three: " << std::accumulate(range.begin(), range.end(), 0, 
        [&](int sum, int i)
        {
            return sum + std::get<1>(raindeer[i]);
        }
    ) << std::endl;
}