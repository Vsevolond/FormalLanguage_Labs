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
                    controlTable.add(to: from, by: by, action: .some(state: to, nonTerm: by))
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
        
        controlTable.add(to: initialState.id, by: grammar.startNonTerm, action: .accept)
    }
}

// MARK: - Analyse word

extension FSM {
    
    func analyse(word: String, numStateOfStack: Int?) -> (Bool, Int, [String]) {
        var snapshot: [String]? = nil
        var tokens: [(value: GrammarSymbol, index: Int)] = word
                                                         .lowercased()
                                                         .map { $0.grammarSymbol }
                                                         .withAppending(.end)
                                                         .enumerated()
                                                         .map { ($1, $0) }
        var stack = GSStack()
        
        stack.push(node: .init(state: initialState.id, token: .eps, point: -1))
        var token = tokens.removeFirst()
        
        while true {

            if token.index == numStateOfStack {
                snapshot = stack.snapshot
            }
            
            let nodes = stack.activeNodes
            guard !nodes.isEmpty else {
                return (false, token.index, snapshot ?? stack.snapshot)
            }

            let actionsByNode: [(GSSNode, [ControlTableAction])] = nodes.map {
                ($0, controlTable.get(for: $0.state, by: token.value).sorted(by: >))
            }
            
            var isAccepted = false
            for (node, actions) in actionsByNode {

                if let lastAction = actions.last {
                    actions.withRemovingLast().forEach { action in
                        isAccepted = isAccepted || perform(action: action, for: node, by: token, to: &stack)
                    }
                    isAccepted = isAccepted || perform(action: lastAction, isLast: true, for: node, by: token, to: &stack)
                    
                } else {
                    stack.popBranch(from: node)
                }
            }
            
            if isAccepted {
                return (true, token.index, snapshot ?? stack.snapshot)
                
            } else if let newToken = tokens.first {
                token = newToken
                tokens.removeFirst()
            }
        }
    }
    
    private func perform(
        action: ControlTableAction,
        isLast: Bool = false,
        for node: GSSNode,
        by token: (value: GrammarSymbol, index: Int),
        to stack: inout GSStack
    ) -> Bool {
        switch action {
            
        case .some(let state, let nonTerm):
            let newNode = stack.push(to: node, newNode: .init(state: state, token: nonTerm, point: token.index))
            
            var isAccepted = false
            
            if newNode.isActive {
                let actions = controlTable.get(for: state, by: token.value).sorted(by: >)
                
                if let lastAction = actions.last {
                    actions.withRemovingLast().forEach { nextAction in
                        isAccepted = isAccepted || perform(action: nextAction, for: newNode, by: token, to: &stack)
                    }
                    isAccepted = isAccepted || perform(action: lastAction, isLast: true, for: newNode, by: token, to: &stack)
                    
                } else {
                    stack.popBranch(from: newNode)
                }
            }
            
            return isAccepted
            
        case .shift(let state):
            let newNode = GSSNode(state: state, token: token.value, point: token.index)
            stack.push(to: node, newNode: newNode)
            return false
            
        case .reduce(let rule):
            let stopNodes = stack.pop(from: node, tokens: rule.right.reversed(), withRemovingTop: isLast)
            let actionsByNode: [(GSSNode, [ControlTableAction])] = stopNodes.map {
                ($0, controlTable.get(for: $0.state, by: rule.left).sorted(by: >))
            }
            
            var isAccepted = false
            for (stopNode, actions) in actionsByNode {
                
                if let lastAction = actions.last {
                    actions.withRemovingLast().forEach { nextAction in
                        isAccepted = isAccepted || perform(action: nextAction, for: stopNode, by: token, to: &stack)
                    }
                    isAccepted = isAccepted || perform(action: lastAction, isLast: true, for: stopNode, by: token, to: &stack)

                } else {
                    stack.popBranch(from: stopNode)
                }
            }
            
            return isAccepted
            
        case .accept:
            return true
        }
    }
}

