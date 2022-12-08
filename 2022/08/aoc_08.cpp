#include <algorithm>
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <vector>
#include <tuple>

using namespace std;

// Returns: { is_visible_from_one_direction, scenic_score }
tuple<bool, int> tree_visibility(int t_row, int t_col, const vector<vector<int>>& trees)
{
    int tree = trees[t_row][t_col];
    int clear_directions = 4;
    int score_lr = 0, score_rl = 0, score_tb = 0, score_bt = 0;

    for (int col = t_col + 1 ; col < trees[t_row].size() ; col++) { // Left to right
        score_lr += 1;
        if (trees[t_row][col] >= tree) {
            clear_directions -= 1; break;
        }
    }

    for (int col = t_col - 1 ; col >= 0 ; col--) { // Right to left
        score_rl += 1;
        if (trees[t_row][col] >= tree) {
            clear_directions -= 1; break;
        }
    }

    for (int row = t_row + 1 ; row < trees.size() ; row++) { // Top to bottom
        score_tb += 1;
        if (trees[row][t_col] >= tree) {
            clear_directions -= 1; break;
        }
    }

    for (int row = t_row - 1 ; row >= 0 ; row--) { // Bottom to top
        score_bt += 1;
        if (trees[row][t_col] >= tree) {
            clear_directions -= 1; break;
        }
    }

    return {clear_directions > 0, (score_lr * score_rl * score_tb * score_bt)};
}

int main()
{
    vector<int> line;
    vector<vector<int>> lines;

    char stream_char;
    fstream fin("input.txt", fstream::in);
    while (fin >> noskipws >> stream_char)
    {
        if (stream_char == '\n') {
            lines.push_back(line);
            line.clear();
        } else {
            int number = stream_char - '0';
            line.push_back(number);
        }
    }

    unsigned rows = lines.size();
    unsigned cols = lines[0].size();

    int trees_visible = ((rows - 1) + (cols - 1)) * 2;
    int scenic_score = 0;

    for (int row = 1 ; row < lines.size() - 1 ; row++) {
        for (int col = 1 ; col < lines[row].size() - 1 ; col++) {
            tuple<bool, int> t_tree_visibility = tree_visibility(row, col, lines);
            trees_visible += get<0>(t_tree_visibility) ? 1 : 0;

            if (get<1>(t_tree_visibility) > scenic_score) {
                scenic_score = get<1>(t_tree_visibility);
            }
        }
    }

    cout << "trees_visible: " << trees_visible << endl;
    cout << "scenic_score: " << scenic_score << endl;

    return 0;
}

