#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

typedef int Int;  // might want 'short' later to save on space

Int smallestBalancingEditLength(const std::string& gene)
{
    const Int L = gene.size();
    assert(L);

    const char nucleotides[] = {'A', 'C', 'G', 'T'};

    // nucleotide -> index -> prefix count
    // e.g.
    //     {'G': [0, 0, 0, 0, 1, 1, 2, 2, 2],
    //      'A': [0, 1, 2, 3, 3, 4, 4, 5, 6],
    //      'T': [0, 0, 0, 0, 0, 0, 0, 0, 0],
    //      'C': [0, 0, 0, 0, 0, 0, 0, 0, 0]}
    // for input
    //     "AAAGAGAA"
    std::unordered_map<char, std::vector<Int>> prefixCounts;

    // First, fill 'prefixCounts' so that looking up the number of each
    // nucleotide is cheap.
    for (const char nucleotide : nucleotides) {
        prefixCounts.emplace(nucleotide, std::vector<Int>(L + 1));
    }

    for (int i = 0; i < L; ++i) {
        const char nucleotide = gene[i];

        for (auto& keyValue : prefixCounts) {
            const char        thisNucleotide = keyValue.first;
            std::vector<Int>& counts         = keyValue.second;

            counts[i + 1] = counts[i] + int(thisNucleotide == nucleotide);
        }
    }

    // If we're already balanced, then the answer is zero.
    if (std::all_of(prefixCounts.begin(),
                    prefixCounts.end(),
                    [L](const auto& keyValue) {
                        const std::vector<Int>& counts = keyValue.second;
                        return counts.back() == L / 4;
                    }))
    {
        return 0;
    }

    // Now we're ready to ROCK.
    Int minLength = L / 4 * 3;

    for (int i = 0; i < L; ++i) {
        for (int j = i + 1; j <= L; ++j) {
            if (j - i >= minLength) {
                continue;
            }

            const bool isCandidate = std::all_of(
                prefixCounts.begin(),
                prefixCounts.end(),
                [&](const auto& keyValue) {
                    const std::vector<Int>& counts = keyValue.second;

                    const int count = counts[j] - counts[i];
                    const int quota = counts.back() - L / 4;

                    return count >= quota;
                });

            if (isCandidate) {
                minLength = j - i;
            }
        }
    }

    return minLength;
}

Int main()
{
    Int geneLength;
    std::string gene;

    std::cin >> geneLength >> gene;
    assert(geneLength == gene.size());

    std::cout << smallestBalancingEditLength(gene) << '\n';
}