// C++14

// TODO: This code is on hold. I'm not going to continue writing it, since I've
//       discovered that the algorithm is not good enough anyway.

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

// I'll first list in this file the definitions of all classes and the
// declarations of all functions. Then below will be their implementations.

struct Destination {
    std::size_t  stateIndex;
    std::int64_t score;
};

class Edges {
    // The outward edges of a state machine node. I use a class so that the
    // underlying data structure can be changed.

    std::vector<std::pair<char, Destination>> edges;  // sorted on the char

  public:
    void add(char letter, Destination toWhere);
        // Create or update the record for 'letter' to contain 'toWhere'.

    Destination lookup(char letter) const;
        // Return the 'Destination' record at the specified 'letter' if such a
        // record exists. Otherwise, return a value initialized 'Destination'
        // (all zeroes), which indicates a transition to the begin state
        // with no score emitted.
};

using MachineStates = std::vector<std::vector<Edges>>;

MachineStates combineMachines(const MachineStates& left,
                              const MachineStates& right);
    // Return the states of a machine that matches (possibly overlapping)
    // appearances of the genes matched by the specified 'left' and 'right'.

class Machine {
    MachineStates states;
    std::size_t   currentStateIndex;

  public:
    explicit Machine(MachineStates&& states);
        // The behavior is undefined if 'states' is empty.

    std::int64_t consume(char letter);
        // Advance this state machine given the specified input 'letter' and
        // return the (possibly zero) score emitted by the transition.

    std::int64_t consume(const std::string& strand);
        // Return the sum of the results of calling 'consume' on each letter in
        // the specified 'strand'.

    void reset();
        // Put this state machine into its initial state.
};

struct Gene {
    std::string  gene;
    std::int64_t score;
};

MachineStates compileStates(const Gene& gene);
    // Return the states of a machine that matches (possibly overlapping)
    // appearances of the specified 'scoredGene'.

std::vector<Machine> compilePrefixMachines(const vector<Gene>& genes);
    // Return a sequence of 'Machine', where a 'Machine' at index 'i' in the
    // output matches all machines whose index in the specified 'genes' is less
    // than 'i'.

// I overload the term "strand" here. In one sense it means 'struct Strand',
// and in another it means the actual sequence of letters contained within the
// 'struct Strand'. The type is always enough to tell them apart.

struct Strand {
    std::string strand;
    std::size_t firstGeneIndex;
    std::size_t lastGeneIndex;  // yes, actually the last, not one-past-last
};

std::int64_t scoreStrand(const Strand& strand,
                         const std::vector<Machine>& prefixMachines);
    // Return the total score of the specified 'strand' using the specified
    // 'prefixMachines', where a 'Machine' at index 'i' in 'prefixMachines'
    // matches the first 'i' genes (thus the 'Machine' at index zero matches
    // no gene).

struct Answer {
    std::int64_t minScore;
    std::int64_t maxScore;
};

Answer minAndMaxScores(const std::vector<Strand>&  strands,
                       const std::vector<Machine>& prefixMachines);

struct Input {
    std::vector<Gene>   genes;
    std::vector<Strand> strands;
};

Input read(std::istream& input);

// And now for all of the definitions.
// SCREW IT AUUUGGHHHH!

int main()
{
    const Input                input    = read(std::cin);
    const std::vector<Gene>&   genes    = input.genes;
    const std::vector<Strand>& strands  = input.strands;
    const std::vector<Machine> machines = compilePrefixMachines(genes);
    const Answer               answer   = minAndMaxScores(strands, machines);

    std::cout << answer.minScore << ' ' << answer.maxScore;
}