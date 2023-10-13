import random
import json
import re
from collections import deque
from enum import Enum


class Operation(Enum):
    """
        Enumeration class representing operations used in constructing expression trees.

        Operations:
            - OR: Represents the logical OR operation (|)
            - AND: Represents the logical AND operation (&)
            - STAR: Represents the Kleene star operation (*)
            - SHARP: Represents the shuffle operation (#)
    """
    OR = "|"
    AND = "&"
    STAR = "*"
    SHARP = "#"

    @classmethod
    def get_random(cls, exclude=None):
        """
            Get a random operation from the available operations.

            Args:
                exclude (list, optional): A list of operations to exclude. Defaults to None.

            Returns:
                Operation: A randomly chosen operation.
        """
        if exclude is None:
            exclude = []

        available_operations = [op for op in cls if op not in exclude]
        if available_operations:
            return random.choice(available_operations)
        else:
            raise ValueError("No available operations to choose from")

    @classmethod
    def get_binary_operands(cls, by=""):
        """
            Returns a list of binary operands in either name or value format.

            Args:
                by (str, optional): The format in which operands are returned. Options: "name" or "value".
                    Defaults to an empty string, which returns Operation objects.

            Returns:
                list: List of binary operands.

        """
        if by == "name":
            binary_operands = [cls.AND.name, cls.OR.name, cls.SHARP.name]
        elif by == "value":
            binary_operands = [cls.AND.value, cls.OR.value, cls.SHARP.value]
        else:
            binary_operands = [cls.AND, cls.OR, cls.SHARP]

        return binary_operands

    @classmethod
    def get_unary_operands(cls, by=""):
        """
            Returns a list of unary operands in either name or value format.

            Args:
                by (str, optional): The format in which operands are returned. Options: "name" or "value".
                    Defaults to an empty string, which returns Operation objects.

            Returns:
                list: List of unary operands.
        """
        if by == "name":
            unary_operands = [cls.STAR.name]
        elif by == "value":
            unary_operands = [cls.STAR.value]
        else:
            unary_operands = [cls.STAR]

        return unary_operands


class TreeNode:
    """
            Class representing a node in an expression tree.

            Args:
                value (str, optional): The value of the node. Defaults to an empty string.
                left (TreeNode, optional): The left child node. Defaults to None.
                right (TreeNode, optional): The right child node. Defaults to None.

            Methods:
                add_child(self, value=''): Adds a child node to the current node.
                check_child(self, pos=0): Checks the child node at the specified position (0 for left, 1 for right).
                __str__(self): Returns the infix expression of the tree.
                display_tree(self, depth=0): Displays the tree structure with indentation.
                infix_expression(self): Returns the infix expression of the subtree rooted at this node.
        """
    def __init__(self, value='', left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def add_child(self, value=''):
        """
                Adds a child node to the current node.

                Args:
                    value (str, optional): The value of the child node. Defaults to an empty string.

                Returns:
                    TreeNode: The added child node.
                """
        new_child = TreeNode(value)
        if self.left is None:
            self.left = new_child
            return self.left
        else:
            self.right = new_child
            return self.right

    def check_child(self, pos=0):
        """
                Checks the child node at the specified position (0 for left, 1 for right).

                Args:
                    pos (int, optional): The position to check (0 for left, 1 for right). Defaults to 0.

                Returns:
                    str: The value of the child node if it exists, or False if there is no child at the specified position.
        """
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
        """
                Displays the tree structure with indentation.

                Args:
                    depth (int, optional): The depth of the current node in the tree. Defaults to 0.

                Returns:
                    str: The tree structure as a string with indentation.
        """
        indent = '  ' * depth
        result = f'{indent}{self.value}\n'
        if self.left:
            result += self.left.display_tree(depth + 1)
        if self.right:
            result += self.right.display_tree(depth + 1)
        return result

    def infix_expression(self):
        """
                Returns the infix expression of the subtree rooted at this node.

                Returns:
                    str: Infix expression of the subtree.
        """
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
    """
        Class representing a state transition in a finite state machine (FSM).

        Args:
            from_state (int): The starting state of the transition.
            to_state (int): The ending state of the transition.
            by_symbol (str): The symbol used to perform the transition.

        Methods:
            edit_from(self, new_from): Update the starting state.
            edit_to(self, new_to): Update the ending state.
            edit_by(self, new_by): Update the transition symbol.
            __str__(self): Return a string representation of the transition.
        """
    def __init__(self, from_state: int, to_state: int, by_symbol: str):
        self.from_state = from_state
        self.to_state = to_state
        self.by_symbol = by_symbol

    def edit_from(self, new_from):
        """
                Update the starting state of the transition.

                Args:
                    new_from (int): The new starting state.
        """
        self.from_state = new_from

    def edit_to(self, new_to):
        """
                Update the ending state of the transition.

                Args:
                    new_to (int): The new ending state.
        """
        self.to_state = new_to

    def edit_by(self, new_by):
        """
                Update the transition symbol.

                Args:
                    new_by (str): The new transition symbol.
        """
        self.by_symbol = new_by

    def __str__(self):
        return f"from: {self.from_state}, to: {self.to_state}, by: {self.by_symbol}"


class FSM:
    """
        Class representing a Finite State Machine (FSM).

        Args:
            initial_state (set): The set of initial states in the FSM.
            states (set): The set of all states in the FSM.
            final_states (set): The set of final/accepting states in the FSM.
            transitions (list of transitions): A list of state transitions in the FSM.
            terminals (list of str): A list of terminal symbols used in the FSM.

        Methods:
            __str__(self): Return a string representation of the FSM.
        """
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
    """
        Class representing an expression along with a Finite State Machine (FSM) definition.

        Args:
            input (str): The input regular expression string.
            output (str): The output regular expression string.
            fsm: A dictionary containing the FSM attributes, which is used to represent the FSM.

        Methods:
            __str__(self): Return a string representation of the Expression.
    """
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

        if not word:
            return current_state in fsm.final_states
        else:
            if word[0] not in fsm.terminals:
                return False

            for transition in fsm.transitions:
                if transition.from_state == current_state and transition.by_symbol == word[0]:
                    queue.append(transition.to_state)
            word = word[1:]

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
