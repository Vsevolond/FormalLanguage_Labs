import random
from enum import Enum


class Operation(Enum):
    OR = "|"
    AND = "&"
    STAR = "*"
    SHARP = "#"

    @classmethod
    def get_random(cls):
        return random.choice(list(cls))


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
        return self.display_tree()

    def display_tree(self, depth=0):
        indent = '  ' * depth
        result = f'{indent}{self.value}\n'
        if self.left:
            result += self.left.display_tree(depth + 1)
        if self.right:
            result += self.right.display_tree(depth + 1)
        return result