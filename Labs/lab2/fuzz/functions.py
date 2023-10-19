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

    def infix_expression(self, is_root=True) -> str:
        """
            Returns the infix expression of the subtree rooted at this node.

            Returns:
                str: Infix expression of the subtree.
        """
        if self.value in Operation.get_binary_operands(by="value"):
            left_expr = self.left.infix_expression(is_root=False)
            right_expr = self.right.infix_expression(is_root=False)
            if self.value == Operation.AND.value:
                return f"{left_expr}{right_expr}"
            elif is_root:
                return f"{left_expr}{self.value}{right_expr}"
            else:
                return f"({left_expr}{self.value}{right_expr})"
        elif self.value in Operation.get_unary_operands(by="value"):
            left_expr = self.left.infix_expression(is_root=False)
            if len(left_expr) == 1 or (left_expr[0] == '(' and left_expr[-1] == ')'):
                return f"{left_expr}{self.value}"
            return f"({left_expr}){self.value}"
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

    def __init__(self, input: str = None, output: str = None, fsm: dict = None, error: str = None):
        self.input = input
        self.output = output
        self.fsm = FSM(**fsm) if fsm is not None else None
        self.error = error

    def __str__(self):
        result = "{\n"
        result += f"INPUT: {self.input}\n"
        result += f"OUTPUT: {self.output}\n"
        result += f"FSM:\n{self.fsm}\n"
        result += "}"
        return result


def get_exprs(filename: str) -> list[Expression]:
    """
        Retrieve a list of Expression objects from a JSON file.

        Args:
            filename (str): The name of the JSON file containing expression definitions.

        Returns:
            list[Expression]: A list of Expression objects created from the JSON file.
    """
    exprs = []
    for regex in parse_json(filename):
        exprs.append(Expression(**regex))
    return exprs


def get_alphabet(size: int = 1) -> list[str]:
    """
        Generate an alphabet as a list of strings.

        Args:
            size (int, optional): The size of the alphabet to generate. Defaults to 1.

        Returns:
            list[str]: A list of alphabet characters.
    """
    alphabet = [chr(ord('a') + i) for i in range(size)]
    return alphabet


def parse_json(filename: str) -> list[dict]:
    """
        Parse a JSON file and return its contents as a list of dictionaries.

        Args:
            filename (str): The path to the JSON file to be parsed.

        Returns:
            list[dict]: A list of dictionaries representing the contents of the JSON file.
    """
    with open(filename, 'r') as file:
        result = json.load(file)
    return result


def get_random_regex(alph_size: int = 3,
                     st_height: int = 1,
                     max_letters: int = 3) -> TreeNode:
    """
        Generate a random regular expression represented as a TreeNode.

        This function generates a random regular expression using a binary tree structure
        where each node represents an operation or a character from the alphabet.

        Args:
            alph_size (int): The size of the alphabet used for character nodes (default is 3).
            st_height (int): The maximum height of the regular expression tree (default is 1).
            max_letters (int): The maximum number of alphabet characters in the regular expression (default is 3).

        Returns:
            TreeNode: A TreeNode representing the generated regular expression.
    """
    alphabet = get_alphabet(alph_size)
    true_max_letters = random.randint(1, max_letters)

    def build_random_expression(letters_cnt, stars=0):
        if letters_cnt == 0:
            return "No expression"
        elif letters_cnt == 1:
            decision = random.randint(1, 100)
            if stars < st_height and decision < 7:
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


def get_possible_transitions(state: int, transitions: list[Transition]) -> list[Transition]:
    """
        Get a list of possible transitions originating from a specific state in a finite state machine.

        Args:
            state (int): The source state for which transitions are to be retrieved.
            transitions (list[Transition]): A list of Transition objects representing transitions in the FSM.

        Returns:
            list[Transition]: A list of Transition objects originating from the given state.
    """
    possible_transitions = []
    for trans in transitions:
        if state == trans.from_state:
            possible_transitions.append(trans)

    return possible_transitions


def shuffle_word(word: str) -> str:
    """
        Shuffle the characters in a word randomly.

        Args:
            word (str): The word to shuffle.

        Returns:
            str: The shuffled word.
    """
    word = list(word)
    random.shuffle(word)
    return ''.join(word)


def sample_word(word: str) -> str:
    """
        Sample a word by randomly selecting a subset of its characters.

        Args:
            word (str): The input word.

        Returns:
            str: A new word created by randomly selecting a subset of characters from the input word.
    """
    word = list(word)
    len_word = random.randint(1, len(word))
    return ''.join(random.sample(word, len_word))


def random_actions(word: str, alphabet: [str]) -> str:
    """
        Perform random actions on a word to modify it.

        Args:
            word (str): The input word to be modified.
            alphabet (List[str]): A list of characters to choose from when making modifications.

        Returns:
            str: The word after applying random modifications.
    """
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
    """
        Generate a random word using a Finite State Machine (FSM).

        Args:
            fsm (FSM): The Finite State Machine to use for generating the word.
            max_length (int): The maximum length of the generated word.

        Returns:
            str: The randomly generated word.
    """
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
    """
        Check if a word matches a regular expression pattern.

        Args:
            regex (str): The regular expression pattern to check against.
            word (str): The word to be checked for matching the regular expression.

        Returns:
            bool: True if the word matches the regular expression, False otherwise.
    """
    return True if re.fullmatch(regex, word) else False


def check_fsm(fsm: FSM, word: str) -> bool:
    """
        Check if a given word is recognized by a Finite State Machine (FSM).

        Args:
            fsm (FSM): The Finite State Machine to check against.
            word (str): The word to be checked for acceptance by the FSM.

        Returns:
            bool: True if the FSM accepts the word, False if not.
    """
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


def print_results(filename: str, output_filename: str, max_len_word: int = 15):
    """
        Print the results of evaluating expressions from a file, comparing regex and FSM results,
        and save them to an output file.

        Args:
            filename (str): The name of the file containing expressions to evaluate.
            output_filename (str): The name of the file to which the results will be saved.
            max_len_word (int, optional): The maximum length of randomly generated words.
                                         Defaults to 15.

        Writes to Output File:
            - Evaluation results for each expression, including the expression number,
              a generated word, and the comparison of the word with both regex and FSM.
            - A summary indicating whether regex is equivalent to FSM for all expressions.

        Returns:
            None
    """
    results = []

    with open(output_filename, "w", encoding='utf-8') as file:
        for num, expr in enumerate(get_exprs(filename)):
            file.write(f"{num + 1} expression:\n")
            if expr.error is not None:
                file.write("\tError in calculating FSM\n\n")
            else:
                generated_word = generate_random_word(expr.fsm, max_len_word)
                file.write(f"\tGenerated word: {generated_word}\n")
                inc_regex = check_regex(expr.output, generated_word)
                inc_fsm = check_fsm(expr.fsm, generated_word)
                result = inc_regex == inc_fsm
                results.append(result)
                file.write(f"\tIncluded in regex: {inc_regex}\n")
                file.write(f"\tIncluded in fsm: {inc_fsm}\n")
                file.write(f"\tRESULT: {result}\n\n")

        check_result = all(results)
        file.write(f"Regex is equivalent to FSM: {check_result}")
