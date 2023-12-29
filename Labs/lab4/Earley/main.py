import sys
from functions import *


def main():
    input_file = f"../{sys.argv[1]}" if len(sys.argv) > 1 else "../grammar.txt"
    grammar = Grammar.load_grammar(input_file)

    word = input()

    parse = run_parse(word, grammar)
    if parse is None:
        print("NO")
    else:
        parse.pretty_print()


if __name__ == "__main__":
    main()
