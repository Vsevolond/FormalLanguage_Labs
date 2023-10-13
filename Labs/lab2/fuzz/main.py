import sys

from functions import *


def main():
    # = CHECK ARGUMENTS = #
    if len(sys.argv) > 1:
        input_file = sys.argv[1]
    else:
        input_file = input("Enter the filename: ")

    output_file = sys.argv[2] if len(sys.argv) > 2 else "../files/output.txt"
    json_file = sys.argv[3] if len(sys.argv) > 3 else "../files/result.json"
    result_file = sys.argv[4] if len(sys.argv) > 4 else "../files/RESULT.txt"
    # = = = = = = = = = = #

    # = CONSTANTS = #
    alphabet_size = 0
    stellar_height = 0
    maximum_number_of_letters = 0
    # = = = = = = = #

    # = READING INPUT FILE = #
    with open(input_file, 'r', encoding='utf-8') as file:
        for line in file.readlines():
            key, value = line.replace(" ", "").strip().split("=")
            match key:
                case "alphabet_size":
                    alphabet_size = int(value)
                case "stellar_height":
                    stellar_height = int(value)
                case "maximum_number_of_letters":
                    maximum_number_of_letters = int(value)
                case _:
                    print(f"Unknown parameter: {key}")
    # = = = = = = = = = = = = #

    # = WRITING TO OUTPUT FILE = #
    with open(output_file, "w", encoding='utf-8') as file:
        for _ in range(10):
            file.write(str(get_random_regex(
                alph_size=alphabet_size,
                st_height=stellar_height,
                max_letters=maximum_number_of_letters)) + '\n')
    # = = = = = = = = = = = = = = #

    # = CHECK EXPRESSIONS = #
    print_results(json_file, result_file)
    # = = = = = = = = = = = #


if __name__ == "__main__":
    main()
