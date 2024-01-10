import Foundation

// MARK: - AnalyseResult

enum FSMResult: Error {
    
    case notLR0Grammar
    case notAccepted(position: Int, nonTerms: Set<GrammarSymbol>, follow: [GrammarSymbol: Set<GrammarSymbol>])
    case accepted
}

// MARK: - FSM

class FSM {
    
    private var states: Set<FSMState> = .init()
    private var initialState: FSMState
    private var transitions: [Int: [GrammarSymbol: Int]] = [:]
    
    private var controlTable: ControlTable = .init()
    private var grammar: Grammar
    
    init(from grammar: Grammar) throws {
        self.grammar = grammar
        initialState = .init(items: grammar.getRulesConvertedToLR0Items())
        states.insert(initialState)
        
        transitions[initialState.id] = [:]
        
        var stack: Stack<FSMState> = .init()
        stack.push(initialState)
        
        while !stack.isEmpty {
            let state = stack.pop()

            for token in state.observingTokens.withRemoving(.end) {
                let newState = try state.goto(by: token, grammar: grammar)

                if let existState = states.first(where: { $0 == newState }) {
                    transitions[state.id]?.updateValue(existState.id, forKey: token)
                } else {
                    states.insert(newState)
                    transitions[state.id]?.updateValue(newState.id, forKey: token)
                    transitions[newState.id] = [:]
                    stack.push(newState)
                }
            }
        }
        
        do {
            try fillControlTable()
        } catch {
            throw FSMResult.notLR0Grammar
        }
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
    }
}

// MARK: - Fill control table

extension FSM {
    
    private func fillControlTable() throws {
        for (from, byTo) in transitions {
            for (by, to) in byTo {
                
                let action: ControlTableAction = by.isTerm ? .shift(state: to) : .some(state: to)
                try controlTable.set(to: from, by: by, action: action)
            }
        }
        
        for state in states.filter({ $0.isFinal }) {
            guard
                let item = state.endedItem,
                let followSet = grammar.getFollowSet(of: item.grammarRule.left)
            else {
                fatalError("something wrong")
            }
            
            for term in followSet {
                try controlTable.set(to: state.id, by: term, action: .reduce(by: item.toGrammarRule()))
            }
        }
        
        try controlTable.set(to: initialState.id, by: grammar.startNonTerm, action: .accept)
    }
}

// MARK: - Analyse word

extension FSM {
    
    func analyse(word: String) throws {
        var tokens = word.lowercased().map { $0.grammarSymbol }.withAppending(.end)
        var stack: Stack<TokenState> = .init()
        
        stack.push(.init(state: initialState.id, token: .eps))
        var currentState = initialState.id
        
        var currentToken = tokens.removeFirst()
        var lastToken = currentToken
        
        while !stack.isEmpty {
            guard let action = try? controlTable.get(for: currentState, by: currentToken) else {
                throw FSMResult.notAccepted(
                    position: word.count - tokens.count,
                    nonTerms: states.first(where: { $0.id == currentState })!.nonTerms,
                    follow: grammar.followSet
                )
            }
            
            switch action {
                
            case .some(let state):
                stack.push(.init(state: state, token: currentToken))
                currentState = state
                currentToken = lastToken
                
            case .shift(let state):
                stack.push(.init(state: state, token: currentToken))
                currentState = state
                currentToken = tokens.removeFirst()

            case .reduce(let rule):
                let top = stack.pop(count: rule.right.count).reversed()
                let right = top.map { $0.token }
                
                guard right == rule.right, let state = stack.top()?.state else {
                    fatalError("something wrong")
                }
                
                currentState = state
                if currentToken.isTerm {
                    lastToken = currentToken
                }
                currentToken = rule.left
                
            case .accept:
                throw FSMResult.accepted
            }
        }
    }
}
