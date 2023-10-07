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
    def get_binary_operands(cls):
        binary_operands = [cls.AND.value, cls.OR.value, cls.SHARP.value]
        return binary_operands

    @classmethod
    def get_unary_operands(cls):
        unary_operands = [cls.STAR.value]
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
        return f"{self.infix_expression()}\n{self.display_tree()}"

    def display_tree(self, depth=0):
        indent = '  ' * depth
        result = f'{indent}{self.value}\n'
        if self.left:
            result += self.left.display_tree(depth + 1)
        if self.right:
            result += self.right.display_tree(depth + 1)
        return result

    def infix_expression(self):
        if self.value in Operation.get_binary_operands():
            left_expr = self.left.infix_expression()
            right_expr = self.right.infix_expression()
            return f"({left_expr} {self.value} {right_expr})"
        elif self.value in Operation.get_unary_operands():
            left_expr = self.left.infix_expression()
            return f"({left_expr} {self.value})"
        else:
            return str(self.value)


def get_alphabet(size=1) -> list:
    alphabet = [chr(ord('a') + i) for i in range(size)]
    return alphabet


def get_random_regex(alph_size=2, st_height=1, max_letters=5):
    alphabet = get_alphabet(alph_size)
    true_max_letters = random.randint(0, max_letters)

    def build_random_expression(stars=0, letters=0):
        if letters == true_max_letters:
            return TreeNode(random.choice(alphabet))
        else:
            operation = Operation.get_random()

            if operation == Operation.STAR:
                if stars > st_height:
                    operation = Operation.get_random(exclude=[Operation.STAR])
                else:
                    stars += 1
                left_child = build_random_expression(stars, letters + 1)
            else:
                left_child = build_random_expression(letters=letters + 1)

            right_child = None

            if operation.value in Operation.get_binary_operands():
                right_child = build_random_expression(letters=letters + 1)

            operation_node = TreeNode(operation.value, left_child, right_child)
            return operation_node

    regex = build_random_expression(max_letters)
    return regex
