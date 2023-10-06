from functions import *


def main():
    filename = "input.txt"  # input("Enter the filename: ")
    alphabet_size = 0
    stellar_height = 0
    maximum_number_of_letters = 0
    with open(filename, 'r', encoding='utf-8') as file:
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

    print(get_random_regex(3, 3, 5))


if __name__ == "__main__":
    main()
