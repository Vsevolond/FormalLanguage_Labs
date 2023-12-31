import Foundation

// MARK: - State

struct FSMState {
    
    static var ID: Int = 0
    private var items: Set<LR0Item>
    let id: Int
    
    private var countOfEndedItems: Int {
        let count = items.map { $0.observingToken }.filter { $0 == .end }.count
        return count
    }
    
    var endedItem: LR0Item? {
        items.filter { $0.observingToken == .end }.first
    }
    
    var observingTokens: Set<GrammarSymbol> {
        let tokens = items.reduce(into: Set<GrammarSymbol>()) { $0.insert($1.observingToken) }
        return tokens
    }
    
    var isFinal: Bool {
        countOfEndedItems == 1
    }
    
    init(items: Set<LR0Item>) {
        self.id = FSMState.ID
        FSMState.ID += 1
        self.items = items
    }
    
    func goto(by token: GrammarSymbol, grammar: Grammar) throws -> FSMState {
        var newState = FSMState(items: Set(items.compactMap { $0.goto(by: token) }))
        newState.closure(by: grammar)
        
        guard newState.countOfEndedItems <= 1 else {
            throw FSMError.notLR0Grammar
        }
        return newState
    }
    
    mutating private func closure(by grammar: Grammar) {
        var nonTerms: Set<GrammarSymbol> = .init()
        var observingNonTerms = observingTokens.filter { $0.isNonTerm }

        while nonTerms != observingNonTerms {
            nonTerms = observingNonTerms
            nonTerms.forEach { nonTerm in
                let rules = grammar.getRulesByLeft(nonTerm: nonTerm)
                items.formUnion(rules.map { $0.convertedToLR0Item() })
            }

            observingNonTerms = observingTokens.filter { $0.isNonTerm }
        }
    }
    
    func printState() {
        print("\(id):")
        items.forEach { item in
            item.printItem()
        }
    }
}

extension FSMState: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(items)
    }
    
    static func ==(lhs: FSMState, rhs: FSMState) -> Bool {
        lhs.items == rhs.items
    }
}
