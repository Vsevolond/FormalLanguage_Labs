import sys
import subprocess
from functions import *


def main():
    # = CHECK ARGUMENTS = #
    input_file = f"../{sys.argv[1]}" if len(sys.argv) > 1 else "../files/input.txt"
    output_file = f"../{sys.argv[2]}" if len(sys.argv) > 2 else "../files/output.txt"
    json_file = f"../{sys.argv[3]}" if len(sys.argv) > 3 else "../files/result.json"
    result_file = f"../{sys.argv[4]}" if len(sys.argv) > 4 else "../files/RESULT.txt"
    # = = = = = = = = = = #

    # = CONSTANTS = #
    swift_file = "../core/main.swift"
    alphabet_size = 0
    stellar_height = 0
    maximum_number_of_letters = 0
    number_of_exprs = 50
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
        for _ in range(number_of_exprs):
            file.write(str(get_random_regex(
                alph_size=alphabet_size,
                st_height=stellar_height,
                max_letters=maximum_number_of_letters)) + '\n')
    # = = = = = = = = = = = = = = #

    # = RUN SWIFT CODE = #
    completed_process = subprocess.run(["swift", swift_file])
    # = = = = = = = = = #

    # = CHECK EXPRESSIONS = #
    if completed_process.returncode == 0:
        print_results(json_file, result_file)
        print("GOOD RUNNING CODE")
    else:
        print("FAIL RUNNING SWIFT-FILE")
    # = = = = = = = = = = = #


if __name__ == "__main__":
    main()
