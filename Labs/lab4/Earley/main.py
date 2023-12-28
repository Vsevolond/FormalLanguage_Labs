import sys
from functions import *


def main():
    input_file = f"../{sys.argv[1]}" if len(sys.argv) > 1 else "../grammar.txt"
    grammar = Grammar.load_grammar(input_file)

    print(grammar)


if __name__ == "__main__":
    main()
