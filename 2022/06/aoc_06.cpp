#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

bool are_all_bits_unique(const vector<uint64_t> bits, uint64_t acc = 0, unsigned index = 0)
{
    if ((acc & bits[index]) != 0) { return false; }
    if (index == bits.size() - 1) { return true; }
    return are_all_bits_unique(bits, (acc | bits[index]), index + 1);
}

int main()
{
    vector<uint64_t> buf_markers;
    vector<uint64_t> buf_messages;
    unsigned markers = 0, messages = 0;
    unsigned char_counter = 0;
    char stream_char;

    fstream fin("input.txt", fstream::in);
    while (fin >> noskipws >> stream_char) {
        char_counter += 1;
        uint64_t stream_char_to_bits = (1ULL << (64 - ((int)stream_char - (int)'a' + 1)));

        if (buf_markers.size() == 3) {
            bool marker_found = are_all_bits_unique(buf_markers, stream_char_to_bits);
            if (marker_found && markers == 0) {
                cout << "First marker found after " << char_counter << " characters" << endl;
            }
            markers += marker_found ? 1 : 0;
            buf_markers.erase(buf_markers.begin() + 0);
        }

        if (buf_messages.size() == 13) {
            bool messages_found = are_all_bits_unique(buf_messages, stream_char_to_bits);
            if (messages_found && messages == 0) {
                cout << "First message found after " << char_counter << " characters" << endl;
            }
            messages += messages_found ? 1 : 0;
            buf_messages.erase(buf_messages.begin() + 0);
        }

        buf_markers.push_back(stream_char_to_bits);
        buf_messages.push_back(stream_char_to_bits);
    }
}
