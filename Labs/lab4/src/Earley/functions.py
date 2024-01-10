import re

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


def prepare_expression(input_string):
    """
    Удаляет пробелы из строки и преобразует ее в список символов.

    Parameters:
    - input_string (str): Исходная строка.

    Returns:
    - list: Список символов.
    """
    return list(input_string.replace(" ", ""))


def reverse_inside_brackets(text: str) -> str:
    """
    Инвертирует текст внутри круглых скобок, сохраняя общую структуру текста.

    Parameters:
    - text (str): Входной текст, содержащий выражения внутри круглых скобок.

    Returns:
    - str: Текст с инвертированным содержимым внутри скобок.
    """
    def reverse_text_inside(match: re.Match) -> str:
        """
        Вспомогательная функция для инверсии содержимого внутри пары скобок.

        Parameters:
        - match (re.match): Объект совпадения, представляющий пару скобок.

        Returns:
        - str: Инвертированное содержимое внутри скобок.
        """
        inside_text = match.group(1)
        reversed_inside = ' '.join(reversed(inside_text.strip('()').split()))
        return f"({reversed_inside})"

    def split_with_brackets(text: str) -> list[str]:
        """
        Вспомогательная функция для разделения текста на основе выражений внутри скобок.

        Parameters:
        - text (str): Входной текст, содержащий выражения внутри скобок.

        Returns:
        - list: Список выражений внутри скобок.
        """
        expressions = re.split(r"('\([^']+\)')", text)

        # Удаляем пустые строки из результата
        expressions = [expr.strip() for expr in expressions if expr]

        return expressions

    # Проверяем наличие скобок в исходной строке
    if '(' in text and ')' in text:
        reversed_text = re.sub(r'\(([^()]*)\)', reverse_text_inside, text)
        reversed_text = split_with_brackets(reversed_text)[::-1]
        reverse = []
        for i in reversed_text:
            if '(' not in i and ')' not in i:
                reverse.append(' '.join(reversed(i.split())))
            else:
                reverse.append(i)
        return ' '.join(reverse)
    else:
        # Если скобок нет, просто инвертируем всю строку
        return ' '.join(reversed(text.split()))


def reverse_right_expression(grammar_str: str) -> str:
    """
    Инвертирует выражения справа от символа "->" в строке грамматики, сохраняя структуру строки.

    Parameters:
    - grammar_str (str): Входная строка с правилами грамматики.

    Returns:
    - str: Строка с инвертированными выражениями справа от символа "->".
    """
    lines = grammar_str.split('\n')
    reversed_lines = []

    for line in lines:
        parts = line.split('->')

        if len(parts) == 2:
            left_side = parts[0].strip()
            right_side = parts[1].strip()

            # Переворачиваем выражения после "->"
            reversed_right_side = reverse_inside_brackets(right_side)

            # Собираем строку с перевернутыми выражениями
            reversed_line = f"{left_side} -> {reversed_right_side}"
            reversed_lines.append(reversed_line)
        else:
            reversed_lines.append(line)

    # Объединяем строки
    result = '\n'.join(reversed_lines)
    return result
