from nltk import CFG
from nltk.parse import EarleyChartParser


def read_grammar_from_file(file_path: str) -> CFG:
    """
    Читает грамматику из файла и возвращает объект CFG.

    Parameters:
    - file_path (str): Путь к файлу с грамматикой.

    Returns:
    - nltk.CFG: Объект CFG, представляющий грамматику.
    """
    with open(file_path, 'r') as file:
        grammar_str = file.read()
        grammar_str = add_quotes_to_terminals(grammar_str)
        return CFG.fromstring(grammar_str)


def replace_parentheses(expression: str) -> str:
    """
    Заменяет каждую открывающую и закрывающую скобку в выражении на строку
    с одинарными кавычками. Все остальные символы сохраняются без изменений.

    Parameters:
    - expression (str): Исходное выражение.

    Returns:
    - str: Строка с замененными скобками.
    """
    result = ""
    for char in expression:
        if char == '(':
            result += "'('"
        elif char == ')':
            result += "')'"
        else:
            result += char
    return result


def add_quotes_to_terminals(grammar_str: str) -> str:
    """
    Добавляет одинарные кавычки к терминалам в строке грамматики.

    Parameters:
    - grammar_str (str): Строка грамматики.

    Returns:
    - str: Строка грамматики с добавленными одинарными кавычками к терминалам.
    """
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
                    symbols[i] = replace_parentheses(symbol)
                    continue
                if not symbol.isalpha() or symbol.isnumeric() or symbol.islower():
                    symbols[i] = f"'{symbol}'"

            # Сборка преобразованной строки
            transformed_line = f"{left_side} -> {' '.join(symbols)}"
            transformed_lines.append(transformed_line)

    # Объединение преобразованных строк
    result = '\n'.join(transformed_lines)
    return result


def is_in_grammar(parser: EarleyChartParser, grammar: CFG, expression: list[str]) -> bool:
    """
    Проверяет, может ли заданное выражение быть разобрано с использованием
    заданного парсера и грамматики.

    Parameters:
    - parser (ChartParser): Объект парсера.
    - grammar (CFG): Объект грамматики.
    - expression (str): Выражение для разбора.

    Returns:
    - bool: True, если выражение может быть разобрано, иначе False.
    """
    try:
        chart = parser.chart_parse(expression)
        for _ in chart.parses(grammar.start()):
            return True
    except ValueError:
        pass

    return False
