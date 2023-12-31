import Foundation

// MARK: - FSMError

enum FSMError: Error {
    
    case notLR0Grammar
}

// MARK: - FSM

class FSM {
    
    private var states: Set<FSMState> = .init()
    private var initialState: FSMState
    private var transitions: [Int: [GrammarSymbol: Int]] = [:]
    
    private var controlTable: [Int: [GrammarSymbol: ControlTableValue]] = [:]
    private var grammar: Grammar
    
    init(from grammar: Grammar) throws {
        self.grammar = grammar
        initialState = .init(items: grammar.getRulesConvertedToLR0Items())
        states.insert(initialState)
        
        transitions[initialState.id] = [:]
        
        var stack: Stack<FSMState> = .init()
        stack.push(initialState)
        
        while stack.top() != nil {
            let state = stack.pop()

            for token in state.observingTokens.withRemoving(.end) {
                let newState = try state.goto(by: token, grammar: grammar)

                if let existState = states.first(where: { $0 == newState }) {
                    transitions[state.id]?.updateValue(existState.id, forKey: token)
                    FSMState.ID -= 1
                } else {
                    states.insert(newState)
                    transitions[state.id]?.updateValue(newState.id, forKey: token)
                    transitions[newState.id] = [:]
                    stack.push(newState)
                }
            }
        }
        
        try fillControlTable()
    }
    
    func printFSM() {
        for state in states.sorted(by: { $0.id < $1.id }) {
            state.printState()
        }
        
        print()
        for (from, byTo) in transitions.sorted(by: { $0.key < $1.key }) {
            for (by, to) in byTo.sorted(by: { $0.value < $1.value }) {
                print("\(from) --\(by.value)--> \(to)")
            }
            print()
        }
        
        for (state, dict) in controlTable.sorted(by: { $0.key < $1.key }) {
            print("\(state):")
            for (symbol, action) in dict {
                print("\(symbol.value) = \(action.value)")
            }
            print()
        }
    }
}

// MARK: - Fill control table

extension FSM {
    
    private func fillControlTable() throws {
        for (from, byTo) in transitions {
            controlTable[from] = [:]
            for (by, to) in byTo {
                
                if by.isTerm {
                    controlTable[from]?.updateValue(.shift(state: to), forKey: by)
                } else {
                    controlTable[from]?.updateValue(.some(state: to), forKey: by)
                }
            }
        }
        
        try states.filter { $0.isFinal }.forEach { state in
            guard
                let item = state.endedItem,
                let followSet = grammar.getFollowSet(of: item.grammarRule.left)
            else {
                fatalError("something wrong")
            }
            
            try followSet.forEach { term in
                if let values = controlTable[state.id], values[term] != nil {
                    throw FSMError.notLR0Grammar
                }
                controlTable[state.id]?.updateValue(.reduce(by: item.toGrammarRule()), forKey: term)
            }
        }
    }
}

// MARK: - Analyse word

extension FSM {
    
    func analyse(word: String) -> Bool {
        var tokens = Stack<GrammarSymbol>(items: word.map { $0.grammarSymbol }.withAppending(.end))
        var stack = Stack<GrammarSymbol>()
        var states = Stack<Int>()

        states.push(0)
        var currentState: Int {
            states.top() ?? -1
        }
        
        while stack.top() != grammar.startNonTerm {
            let token = tokens.pop()

            guard
                let values = controlTable[currentState],
                let controlValue = values[token]
            else {
                return false
            }
            
            switch controlValue {
                
            case .some(let state):
                states.push(state)
                
            case .shift(let state):
                stack.push(token)
                states.push(state)
                
            case .reduce(let rule):
                let right = stack.pop(count: rule.right.count)
                states.pop(count: right.count)
                
                guard right == rule.right else {
                    fatalError("something wrong")
                }
                
                tokens.push(token)
                tokens.push(rule.left)
                stack.push(rule.left)
            }
        }
        
        guard
            stack.pop() == tokens.pop(),
            stack.isEmpty,
            tokens.top() == .end,
            currentState == 0
        else {
            fatalError("something wrong")
        }
        
        return true
    }
}

