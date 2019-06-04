
#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

std::vector<int> rankify(const std::vector<int>& scores)
{
    std::vector<int> ranks;
    ranks.reserve(scores.size());

    ranks.push_back(1);
    for (int i = 1; i < scores.size(); ++i) {
        const int rank = scores[i] == scores[i - 1] ?
                         ranks[i - 1] :
                         ranks[i - 1] + 1;
        ranks.push_back(rank);
    }

    return ranks;
}

std::vector<int> aliceRankings(const std::vector<int>& scores,
                               const std::vector<int>& aliceScores)
{
    const std::vector<int> ranks = rankify(scores);

    std::vector<int> aliceRanks;
    aliceRanks.reserve(aliceScores.size());

    const auto end  = scores.rend();
    auto       iter = scores.rbegin();

    for (const int aliceScore : aliceScores)
    {
        int rank;  // to be determined

        iter = std::lower_bound(iter, end, aliceScore);
        const int i = std::distance(iter, end) - 1;

        if (iter == end) {
            rank = 1;
        }
        else if (aliceScore == *iter) {
            rank = ranks[i];
        }
        else {
            rank = ranks[i] + 1;
        }

        aliceRanks.push_back(rank);
    }

    return aliceRanks;
}

int main()
{
    int numScores;
    std::cin >> numScores;

    std::vector<int> scores;
    std::copy_n(std::istream_iterator<int>(std::cin),
                numScores,
                std::back_inserter(scores));

    int numAliceGames;
    std::cin >> numAliceGames;

    std::vector<int> aliceScores;
    std::copy_n(std::istream_iterator<int>(std::cin),
                numAliceGames,
                std::back_inserter(aliceScores));

    const std::vector<int> answer = aliceRankings(scores, aliceScores);
    std::copy(answer.begin(),
              answer.end(),
              std::ostream_iterator<int>(std::cout, "\n"));
}