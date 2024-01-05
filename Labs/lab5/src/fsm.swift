import Foundation

// MARK: - FSM

class FSM {
    
    private var states: Set<FSMState> = .init()
    private var initialState: FSMState
    private var transitions: [Int: [GrammarSymbol: Int]] = [:]
    
    private var controlTable: ControlTable = .init()
    private var grammar: Grammar
    
    init(from grammar: Grammar) {
        self.grammar = grammar
        initialState = .init(items: grammar.getRulesConvertedToLR0Items())
        states.insert(initialState)
        
        transitions[initialState.id] = [:]
        
        var stack: Stack<FSMState> = .init()
        stack.push(initialState)
        
        while stack.top() != nil {
            let state = stack.pop()

            for token in state.observingTokens.withRemoving(.end) {
                let newState = state.goto(by: token, grammar: grammar)

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
        
        fillControlTable()
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
        
//        for (state, dict) in controlTable.sorted(by: { $0.key < $1.key }) {
//            print("\(state):")
//            for (symbol, actions) in dict {
//                print("\(symbol.value) = \(actions.reduce(into: "") { $0 += "\($1.value) "})")
//            }
//            print()
//        }
    }
}

// MARK: - Fill control table

extension FSM {
    
    private func fillControlTable() {
        for (from, byTo) in transitions {
            for (by, to) in byTo {
                
                if by.isTerm {
                    controlTable.add(to: from, by: by, action: .shift(state: to))
                } else {
                    controlTable.add(to: from, by: by, action: .some(state: to))
                }
            }
        }
        
        states.filter { $0.isFinal }.forEach { state in
            let items = state.endedItems
            items.forEach { item in
                guard let followSet = grammar.getFollowSet(of: item.grammarRule.left) else {
                    fatalError("there is no follow set for: \(item.grammarRule.left)")
                }
                
                followSet.forEach { term in
                    controlTable.add(to: state.id, by: term, action: .reduce(by: item.toGrammarRule()))
                }
            }
        }
    }
}

// MARK: - Analyse word

extension FSM {
    
    func analyse(word: String) -> Bool {
        
    }
}

