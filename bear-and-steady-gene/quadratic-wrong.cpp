#include <algorithm>
#include <cassert>
#include <initializer_list>
#include <iostream>
#include <string>
#include <vector>

typedef int Int;  // might want 'short' later to save on space

Int smallestBalancingEditLength(const std::string& gene)
{
    const Int L = gene.size();
    assert(L);

    class Grid {
        const Int        L;
        std::vector<Int> data;

      public:
        explicit Grid(Int inputLength, Int defaultValue)
        : L(inputLength)
        , data((L + 1) * (L + 1), defaultValue)
        {}

        Int& operator()(Int i, Int j) {
            return data[L * i + j];
        }
    };

    // Default value is 3/4 of L because that's the worst case.
    Grid grid(L, L / 4 * 3);

    // nucleotide -> index -> prefix count
    // e.g.
    //     {'G': [0, 0, 0, 1, 1, 2, 2, 2],
    //      'A': [1, 2, 3, 3, 4, 4, 5, 6]}
    // for input
    //     "AAAGAGAA"
    std::unordered_map<char, std::vector<Int>> prefixCounts;

    // First, fill 'prefixCounts' so that looking up the number of each
    // nucleotide is cheap.
    for (const char nucleotide : {'A', 'C', 'G', 'T'}) {
        prefixCounts.emplace(nucleotide, std::vector<Int>(L));
    }

    // The nucleotide that starts the sequence begins with count 1, while the
    // others begin with count 0 (which is the default values in the vectors).
    prefixCounts[gene[0]][0] = 1;

    for (int i = 1; i < L; ++i) {
        const char nucleotide = gene[i];

        for (auto& keyValue : prefixCounts) {
            const char   thisNucleotide = keyValue.first;
            vector<Int>&         counts = keyValue.second;

            counts[i] = counts[i - 1] + int(thisNucleotide == nucleotide);
        }
    }

    // Now our data structures are set up. The condition we'll be using as we
    // traverse the substrings of 'gene' depends upon the excess/deficiency
    // (if any) of the nucleotides. Whatever substring it turns out is the
    // smallest for our purposes, it must contain whatever "extra" nucleotides
    // we saw initially. So, the condition will look like:
    //
    //    Number of g's is >= (expected total g's) - (actual total g's).
    //    Number of a's is >= (expected total a's) - (actual total a's).
    //    etc.
    //
    // If the condition is _not_ met, then the value for the current grid entry
    // is just the min of its two neighbors.  If the condition _is_ met, then
    // the value for the current grid entry is the min of any of its two
    // neighbors _or_ its own length (the length of the substring corresponding
    // to that grid entry).
    for (int i = L - 1; i >= 0; --i) {
        for (int j = i + 1; j < L; ++j) {
            const int length = j - i;  // >= 1
            bool      isCandidate = true;
            for (const auto& keyValue : prefixCounts) {
                const std::vector<Int>& counts = keyValue->second;
                const int               total  = counts.back();
                
                const int howMany = i ? counts[j] - counts[i - 1] : counts[j];
                const int quota   = total - L / 4;

                if (howMany < quota) {
                    isCandidate = false;
                    break;
                }
            }

            // Now we can determine for the grid value at (i, j).
            const Int minNeighbors = std::min(grid(i + 1, j), grid(i, j - 1));
            if (isCandidate) {
                grid(i, j) = std::min(minNeighbors, length);
            }
            else {
                grid(i, j) = minNeighbors;
            }

            // TODO: This is too complicated, and wrong.
        }
    }
}

Int main()
{
    Int geneLength;
    std::string gene;

    std::cin >> geneLength >> gene;
    assert(geneLength == gene.size());

    std::cout << smallestBalancingEditLength(gene) << '\n';
}