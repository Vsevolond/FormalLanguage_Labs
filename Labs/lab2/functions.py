import random
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


def get_alphabet(size=1) -> list:
    alphabet = [chr(ord('a') + i) for i in range(size)]
    return alphabet


def get_random_regex(alph_size=3, st_height=1, max_letters=3):
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
