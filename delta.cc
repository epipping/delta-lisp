#include <algorithm> // max
#include <cassert>
#include <cmath> // floor
#include <cstdlib> // system
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

// TODO: check that initial input passes
// TODO: parallelise test_complements

std::vector<std::string> file_contents;
size_t number_of_lines;

std::string test_program;

void
read_file(char const *filename)
{
  std::ifstream s(filename);
  assert(s.is_open());

  std::string line;
  for (number_of_lines=0; !s.eof(); ++number_of_lines) {
    getline(s, line);
    file_contents.push_back(line);
  }
}

void
write_file(char const *filename, std::vector<std::string> subset)
{
  std::ofstream s(filename);
  assert(s.is_open());

  for (size_t i=0; i < subset.size() - 1; ++i)
    s << subset[i] << std::endl;

  s << subset[subset.size() - 1];
}

bool
run_program(char const *progname, std::vector<std::string> subset)
{
  std::string command(progname);
  command += " ";
  command += "output";
  write_file("output", subset);
  return system(command.c_str());
}

size_t
compute_break(size_t length, size_t part, size_t parts)
{
  return floor((length / parts) * part);
}

void
zero_subset(size_t left, size_t right,
	    std::vector<std::string> const subset,
	    std::vector<std::string> &complement)
{
  complement = subset; // copy
  complement.erase(complement.begin() + left, complement.begin() + right);
}

bool
test_complements(size_t parts,
		 std::vector<std::string> const subset,
		 std::vector<std::string> &reduction)
{
  std::vector<std::string> complement;
  size_t left;
  size_t right = 0;
  for (size_t j=1; j <=parts; ++j) {
    left = right;
    right = compute_break(subset.size(), j, parts);
    zero_subset(left, right, subset, complement);
    if (run_program(test_program.c_str(), complement) == 0) {
      assert(0 == rename("output", "output-minimal"));
      reduction = complement;
      return true;
    }
  }
  return false;
}

void
ddmin(size_t parts,
      std::vector<std::string> &subset)
{
  std::vector<std::string> potential_reduction;
  while (true) {
    if (test_complements(parts, subset, potential_reduction)) {
      subset = potential_reduction;
      std::cout << "Reduced to " << subset.size() << " lines" << std::endl;
      parts = std::max<size_t>(2, parts - 1);
    } else if (parts < subset.size())
      parts = std::min(2 * parts, subset.size());
    else
      break;
  }
}

int
main(int argc, char *argv[])
{
  assert (argc == 3);
  read_file(argv[1]);
  test_program = argv[2];

  std::vector<std::string> potential_reduction = file_contents;
  ddmin(2, potential_reduction);

  return 0;
}
