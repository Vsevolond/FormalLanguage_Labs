from collections import defaultdict


class Rule(object):
	"""
	Represents a CFG rule.
	"""

	def __init__(self, lhs, rhs):
		"""
		Represents the rule 'lhs -> rhs', where lhs is a non-terminal and
		rhs is a list of non-terminals and terminals.
		"""
		self.lhs, self.rhs = lhs, rhs

	def __contains__(self, sym):
		return sym in self.rhs

	def __eq__(self, other):
		if type(other) is Rule:
			return self.lhs == other.lhs and self.rhs == other.rhs

		return False

	def __getitem__(self, i):
		return self.rhs[i]

	def __len__(self):
		return len(self.rhs)

	def __repr__(self):
		return self.__str__()

	def __str__(self):
		return self.lhs + ' -> ' + ' '.join(self.rhs)


class Grammar(object):
	"""
	Represents a CFG.
	"""

	def __init__(self):
		# The rules are represented as a dictionary from L.H.S to R.H.S.
		self.rules = defaultdict(list)

	def add(self, rule):
		"""
		Adds the given rule to the grammar.
		"""

		self.rules[rule.lhs].append(rule)

	@staticmethod
	def load_grammar(fpath):
		"""
		Loads the grammar from file
		"""

		grammar = Grammar()

		with open(fpath) as f:
			for line in f:
				line = line.strip()

				if len(line) == 0:
					continue

				entries = line.split('->')
				lhs = entries[0].strip()
				for rhs in entries[1].split('|'):
					grammar.add(Rule(lhs, rhs.strip().split()))

		return grammar

	def __repr__(self):
		return self.__str__()

	def __str__(self):
		s = [str(r) for r in self.rules['S']]

		for nt, rule_list in self.rules.items():
			if nt == 'S':
				continue

			s += [str(r) for r in rule_list]

		return '\n'.join(s)

	# Returns the rules for a given Non-terminal.
	def __getitem__(self, nt):
		return self.rules[nt]

	def is_terminal(self, sym):
		"""
		Checks is the given symbol is terminal.
		"""

		return len(self.rules[sym]) == 0

	def is_tag(self, sym):
		"""
		Checks whether the given symbol is a tag, i.e. a non-terminal with rules
		to solely terminals.
		"""

		if not self.is_terminal(sym):
			return all(self.is_terminal(s) for r in self.rules[sym] for s in r.rhs)

		return False
