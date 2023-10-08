import sys

from functions import *


def main():
    if len(sys.argv[1:]) == 1:
        input_file = sys.argv[1]
        output_file = "output.txt"
    elif len(sys.argv[1:]) == 2:
        input_file = sys.argv[1]
        output_file = sys.argv[2]
    else:
        input_file = input("Enter the filename: ")
        output_file = "output.txt"

    alphabet_size = 0
    stellar_height = 0
    maximum_number_of_letters = 0

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

    with open(output_file, "w", encoding='utf-8') as file:
        for _ in range(10):
            file.write(str(get_random_regex(
                alph_size=alphabet_size,
                st_height=stellar_height,
                max_letters=maximum_number_of_letters)) + '\n')


if __name__ == "__main__":
    main()
