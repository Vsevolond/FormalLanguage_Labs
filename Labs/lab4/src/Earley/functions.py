from nltk import CFG


def read_grammar_from_file(file_path):
    with open(file_path, 'r') as file:
        grammar_str = file.read()
        grammar_str = add_quotes_to_terminals(grammar_str)
        return CFG.fromstring(grammar_str)


def add_quotes_to_terminals(grammar_str: str) -> str:
    lines = grammar_str.split('\n')
    transformed_lines = []

    for line in lines:
        # Разделение строки на левую и правую части по символу "->"
        parts = line.split('->')

        if len(parts) == 2:
            left_side = parts[0].strip()
            right_side = parts[1].strip()

            # Разделение правой части по пробелам
            symbols = right_side.split()

            # Добавление кавычек к терминалам
            for i, symbol in enumerate(symbols):
                if symbol in {'|', '(', ')'} or (len(symbol) > 1 and symbol[0] == symbol[-1] == "'"):
                    continue
                if '(' in symbol or ')' in symbol:
                    continue
                if not symbol.isalpha() or symbol.isnumeric() or symbol.islower():
                    symbols[i] = f"'{symbol}'"

            # Сборка преобразованной строки
            transformed_line = f"{left_side} -> {' '.join(symbols)}"
            transformed_lines.append(transformed_line)

    # Объединение преобразованных строк
    result = '\n'.join(transformed_lines)
    return result


def is_in_grammar(parser, grammar, expression):
    # Парсинг выражения
    try:
        chart = parser.chart_parse(expression.split())
        # Проверка успешного разбора
        for _ in chart.parses(grammar.start()):
            return True
    except ValueError:
        pass

    return False
