import random
import json
import re
from collections import deque
from enum import Enum


class Operation(Enum):
    OR = "|"
    AND = "&"
    STAR = "*"
    SHARP = "#"

    @classmethod
    def get_random(cls, exclude=None):
        if exclude is None:
            exclude = []

        available_operations = [op for op in cls if op not in exclude]
        if available_operations:
            return random.choice(available_operations)
        else:
            raise ValueError("No available operations to choose from")

    @classmethod
    def get_binary_operands(cls, by=""):
        if by == "name":
            binary_operands = [cls.AND.name, cls.OR.name, cls.SHARP.name]
        elif by == "value":
            binary_operands = [cls.AND.value, cls.OR.value, cls.SHARP.value]
        else:
            binary_operands = [cls.AND, cls.OR, cls.SHARP]

        return binary_operands

    @classmethod
    def get_unary_operands(cls, by=""):
        if by == "name":
            unary_operands = [cls.STAR.name]
        elif by == "value":
            unary_operands = [cls.STAR.value]
        else:
            unary_operands = [cls.STAR]

        return unary_operands


class TreeNode:
    def __init__(self, value='', left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def add_child(self, value=''):
        new_child = TreeNode(value)
        if self.left is None:
            self.left = new_child
            return self.left
        else:
            self.right = new_child
            return self.right

    def check_child(self, pos=0):
        # 0 = left, 1 = right
        if pos == 0:
            if self.left:
                return self.left.value
            return False
        if pos == 1:
            if self.right:
                return self.right.value
            return False

    def __str__(self):
        return self.infix_expression()

    def display_tree(self, depth=0):
        indent = '  ' * depth
        result = f'{indent}{self.value}\n'
        if self.left:
            result += self.left.display_tree(depth + 1)
        if self.right:
            result += self.right.display_tree(depth + 1)
        return result

    def infix_expression(self):
        if self.value in Operation.get_binary_operands(by="value"):
            left_expr = self.left.infix_expression()
            right_expr = self.right.infix_expression()
            return f"({left_expr}{self.value}{right_expr})"
        elif self.value in Operation.get_unary_operands(by="value"):
            left_expr = self.left.infix_expression()
            return f"({left_expr}{self.value})"
        else:
            return str(self.value)


class Transition:
    def __init__(self, from_state: int, to_state: int, by_symbol: str):
        self.from_state = from_state
        self.to_state = to_state
        self.by_symbol = by_symbol

    def edit_from(self, new_from):
        self.from_state = new_from

    def edit_to(self, new_to):
        self.to_state = new_to

    def edit_by(self, new_by):
        self.by_symbol = new_by

    def __str__(self):
        return f"from: {self.from_state}, to: {self.to_state}, by: {self.by_symbol}"


class FSM:
    def __init__(self, initial_state: set, states: set, final_states: set,
                 transitions: [Transition], terminals: [str]):
        self.initial_state = initial_state
        self.states = states
        self.final_states = final_states
        self.transitions = [Transition(**t) for t in transitions]
        self.terminals = terminals

    def __str__(self):
        result = "{\n"
        result += f"\tSTATES: {self.states}\n"
        result += f"\tFINAL_STATES: {self.final_states}\n"
        result += f"\tTRANSITIONS: {[str(t) for t in self.transitions]}\n"
        result += f"\tTERMINALS: {self.terminals}\n"
        result += "}"
        return result


class Expression:
    def __init__(self, input: str, output: str, fsm: dict):
        self.input = input
        self.output = output
        self.fsm = FSM(**fsm)

    def __str__(self):
        result = "{\n"
        result += f"INPUT: {self.input}\n"
        result += f"OUTPUT: {self.output}\n"
        result += f"FSM:\n{self.fsm}\n"
        result += "}"
        return result


def get_exprs(filename: str):
    exprs = []
    for regex in parse_json(filename):
        exprs.append(Expression(**regex))
    return exprs


def get_alphabet(size=1) -> list:
    alphabet = [chr(ord('a') + i) for i in range(size)]
    return alphabet


def parse_json(filename):
    with open(filename, 'r') as file:
        result = json.load(file)
    return result


def get_random_regex(alph_size=3, st_height=1, max_letters=3) -> TreeNode:
    alphabet = get_alphabet(alph_size)
    true_max_letters = random.randint(1, max_letters)

    def build_random_expression(letters_cnt, stars=0):
        if letters_cnt == 0:
            return "No expression"
        elif letters_cnt == 1:
            decision = random.randint(1, 100)
            if stars < st_height and decision < 50:
                operation = Operation.get_random(exclude=Operation.get_binary_operands())
            else:
                return TreeNode(random.choice(alphabet))
        else:
            operation = Operation.get_random(exclude=[] if stars <= st_height else [Operation.STAR])

        right_leaves = letters_cnt // 2
        left_leaves = letters_cnt - right_leaves

        if operation == Operation.STAR and stars <= st_height:
            stars += 1
        else:
            stars = 0

        left_tree = build_random_expression(left_leaves, stars)

        if operation in Operation.get_binary_operands():
            right_tree = build_random_expression(right_leaves, stars)
        else:
            right_tree = None

        return TreeNode(operation.value, left_tree, right_tree)

    regex = build_random_expression(true_max_letters)
    return regex


def get_possible_transitions(state: int, transtions: list[Transition]) -> list[Transition]:
    possible_transitions = []
    for trans in transtions:
        if state == trans.from_state:
            possible_transitions.append(trans)

    return possible_transitions


def shuffle_word(word: str) -> str:
    word = list(word)
    random.shuffle(word)
    return ''.join(word)


def sample_word(word: str) -> str:
    word = list(word)
    len_word = random.randint(1, len(word))
    return ''.join(random.sample(word, len_word))


def random_actions(word: str, alphabet: [str]) -> str:
    repeat = random.choice([1, 1, 1, 1, 1,
                            2, 3, 4, 5, 6])
    for _ in range(repeat):
        chance = random.randint(1, 100)
        if chance < 10:
            word += chr(random.randint(ord('a'), ord('z')))
        if 5 < chance < 15:
            word = word[::-1]
        if 10 < chance < 20:
            word += random.choice(alphabet)
        if 20 < chance < 50:
            if len(word) > 2:
                word = word[:-1]
        if 45 < chance < 55:
            word = sample_word(word)
        if 58 < chance < 71:
            word = shuffle_word(word)

    return word


def generate_random_word(fsm: FSM, max_length: int) -> str:
    current_state = 0
    word = ""
    true_max_length = random.randint(1, max_length)

    for _ in range(true_max_length):
        trans = get_possible_transitions(current_state, fsm.transitions)
        if len(trans) == 0:
            break
        chosen_trans = random.choice(trans)
        word += chosen_trans.by_symbol
        current_state = chosen_trans.to_state

    word = random_actions(word, fsm.terminals)

    return word


def check_regex(regex: str, word: str) -> bool:
    return True if re.fullmatch(regex, word) else False


def check_fsm(fsm: FSM, word: str) -> bool:
    if not word:
        return fsm.initial_state in fsm.final_states
    queue = deque()
    visited = set()
    current_state = fsm.initial_state

    queue.append(current_state)

    while queue:
        current_state = queue.popleft()
        visited.add(current_state)

        if word:
            if word[0] not in fsm.terminals:
                return False
            else:
                for transition in fsm.transitions:
                    if transition.from_state == current_state and transition.by_symbol == word[0]:
                        queue.append(transition.to_state)
                word = word[1:]

    if current_state in fsm.final_states and not word:
        return True

    return False


def print_results(filename: str, max_len_word: int = 15):
    check_result = True

    for num, expr in enumerate(get_exprs(filename)):
        print(f"{num} expression:")
        print(expr)
        word = generate_random_word(expr.fsm, max_len_word)
        print("\tGenerated word: ", word)
        inc_regex = check_regex(expr.output, word)
        inc_fsm = check_fsm(expr.fsm, word)
        print(f"\tIncluded in regex: {inc_regex}")
        print(f"\tIncluded in fsm: {inc_fsm}")
        if inc_regex != inc_fsm:
            check_result = False
        print(f"\tRESULT: {inc_regex == inc_fsm}\n")

    print(f"Regex is equivalent to FSM: {check_result}")
