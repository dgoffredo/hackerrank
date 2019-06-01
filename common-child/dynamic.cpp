
#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <vector>

int lengthOfLongestSubsequence(const std::string& left, 
                               const std::string& right)
{
    assert(left.size() == right.size());

    const int L = left.size();

    class Grid {
        int              sideLength;
        std::vector<int> data;

      public:
        explicit Grid(int sideLength)
        : sideLength(sideLength)
        , data(sideLength * sideLength)
        {}

        int& operator()(int i, int j) {
            return data[i * sideLength + j];
        }
    };

    Grid grid(L + 1);

    for (int diag = 0; diag < 2*L - 1; ++diag) {
        const int diagLength = diag < L ? diag + 1 : 2*L - diag - 1;
        const int iStart = std::min(diag, L - 1);
        const int jStart = diag < L ? 0 : diag - L + 1;

        for (int offset = 0; offset < diagLength; ++offset) {
            const int i = L - (iStart - offset) - 1;
            const int j = L - (jStart + offset) - 1;

            const int maxOnSides = std::max(grid(i + 1, j), grid(i, j + 1));
            if (left[i] == right[j]) {
                grid(i, j) = std::max(maxOnSides, grid(i + 1, j + 1) + 1);
            }
            else {
                grid(i, j) = maxOnSides;
            }
        }
    }

    return grid(0, 0);
}

int main()
{
    std::string left, right;
    std::cin >> left >> right;

    std::cout << lengthOfLongestSubsequence(left, right);
}