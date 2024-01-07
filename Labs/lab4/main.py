import sys
from src.Earley.functions import *
from nltk.parse import ChartParser


def main():
    input_file = f"../{sys.argv[1]}" if len(sys.argv) > 1 else "../grammar.txt"
    grammar = read_grammar_from_file(input_file)
    parser = ChartParser(grammar)

    # Предложение для анализа
    sentence = "2 + 3 * 4"

    if is_in_grammar(parser, grammar, sentence):
        chart = parser.chart_parse(sentence.split())
        for tree in chart.parses(grammar.start()):
            tree.pretty_print()
    else:
        print("NO")


if __name__ == "__main__":
    main()
