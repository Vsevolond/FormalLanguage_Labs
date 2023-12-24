import Foundation

// MARK: - Extensions

extension Dictionary where Value: SetAlgebra, Value.Element: Hashable {
    
    var countOfAllValues: Int {
        let count = values.compactMap { $0 as? Set<Value.Element> }.map { $0.count }.reduce(0, +)
        return count
    }
}

extension Array {
    
    func withAppending(_ element: Element) -> Array<Element> {
        var newArray = self
        newArray.append(element)
        return newArray
    }
    
    func withRemovingFirst() -> Array<Element> {
        var newArray = self
        newArray.removeFirst()
        return newArray
    }
    
    func withRemovingLast() -> Array<Element> {
        var newArray = self
        newArray.removeLast()
        return newArray
    }
}

extension Set {
    
    func withInserting(_ element: Element) -> Set<Element> {
        var newSet = self
        newSet.insert(element)
        return newSet
    }
    
    func withRemoving(_ element: Element) -> Set<Element> {
        var newSet = self
        newSet.remove(element)
        return newSet
    }
}

extension Character {

    var grammarSymbol: GrammarSymbol {
        switch self {
        case "e": return .eps
        case "$": return .end
        default:
            if self.isLetter, self.isUppercase {
                return .nonTerm(self)
            } else {
                return .term(self)
            }
        }
    }
}

extension String {

    func removingSpaces() -> String {
        var string = self
        string.removeAll(where: { $0 == " " })
        return string
    }
}

// MARK: - Stack

struct Stack<Element> {

    private var items: [Element] = []

    mutating func push(_ item: Element) {
        items.append(item)
    }

    @discardableResult
    mutating func pop() -> Element {
        return items.removeLast()
    }

    func top() -> Element? {
        return items.last
    }
}

// MARK: - FSMError

enum FSMError: Error {
    
    case notLR0Grammar
}

// MARK: - GrammarSymbol

enum GrammarSymbol: Hashable {
    
    case term(_ char: Character)
    case nonTerm(_ char: Character)
    case eps
    case end
    
    var isTerm: Bool {
        switch self {
            case .nonTerm(_): return false
            default: return true
        }
    }
    
    var isNonTerm: Bool { !isTerm }
    
    var value: Character {
        switch self {
        case .term(let char), .nonTerm(let char):
            return char
        case .eps:
            return "e"
        case .end:
            return "$"
        }
    }
}

// MARK: - GrammarRule

struct GrammarRule: Hashable {

    let left: GrammarSymbol
    let right: [GrammarSymbol]
    
    var terms: Set<GrammarSymbol> {
        Set(right.filter({ $0.isTerm }))
    }
    
    var nonTerms: Set<GrammarSymbol> {
        Set(right.filter({ $0.isNonTerm }))
    }
    
    init(left: GrammarSymbol, right: [GrammarSymbol]) {
        self.left = left
        self.right = right
    }
    
    func printRule() {
        print("\(left.value) -> \(right.reduce(into: "") { $0 += String($1.value) })")
    }
    
    func convertedToLR0Item() -> LR0Item {
        .init(grammarRule: self)
    }
}

// MARK: - Grammar

class Grammar {

    private var nonTerms: Set<GrammarSymbol>
    private var terms: Set<GrammarSymbol>
    private var rules: [GrammarRule]
    private var startNonTerm: GrammarSymbol
    
    private var firstSet: [GrammarSymbol : Set<GrammarSymbol>]
    private var followSet: [GrammarSymbol : Set<GrammarSymbol>]
    
    init(from stringRules: [String]) {
        rules = Grammar.parseGrammarRules(from: stringRules)
        guard let firstRule = rules.first else {
            fatalError("there are no rules of grammar")
        }
        startNonTerm = .nonTerm("§")
        rules.insert(.init(left: startNonTerm, right: [firstRule.left]), at: 0)

        nonTerms = rules.reduce(into: Set<GrammarSymbol>()) { $0.formUnion($1.nonTerms.withInserting($1.left)) }
        terms = rules.reduce(into: Set<GrammarSymbol>()) { $0.formUnion($1.terms) }
        
        firstSet = nonTerms.reduce(into: [GrammarSymbol : Set<GrammarSymbol>]()) { $0.updateValue(.init(), forKey: $1) }
        followSet = nonTerms.reduce(into: [GrammarSymbol : Set<GrammarSymbol>]()) { $0.updateValue(.init(), forKey: $1) }
        
        removeUselessSymbols()
        makeFirstSet()
        makeFollowSet()
    }
    
    func printGrammar() {
        rules.forEach { rule in
            rule.printRule()
        }
        
        print("FIRST:")
        firstSet.forEach { key, value in
            print("\(key.value): \(value.map { $0.value })")
        }
        
        print("FOLLOW:")
        followSet.forEach { key, value in
            print("\(key.value): \(value.map { $0.value })")
        }
    }
}

// MARK: - For removing useless symbols in grammar

extension Grammar {
    
    private func removeUselessSymbols() { // удаление бесполезных символов
        deleteRulesWithNotGenerativeNonTerms()
        deleteRulesWithNotReachableNonTerms()
    }
    
    private func deleteRulesWithNotGenerativeNonTerms() { // удаление правил, содержащих непопрождающие нетерминалы
        var generativeNonTerms: Set<GrammarSymbol> = .init()
        rules.forEach { rule in
            if rule.nonTerms.count == 0 {
                generativeNonTerms.insert(rule.left)
            }
        }

        var count = 0
        while count != generativeNonTerms.count {
            count = generativeNonTerms.count

            rules.forEach { rule in
                if generativeNonTerms.contains(rule.nonTerms) {
                    generativeNonTerms.insert(rule.left)
                }
            }
        }
        
        var i = 0
        while i < rules.count {
            if !generativeNonTerms.contains(rules[i].left) || !generativeNonTerms.isSuperset(of: rules[i].nonTerms) {
                rules.remove(at: i)
            } else {
                i += 1
            }
        }
        
        guard let newStart = rules.first?.left else {
            fatalError("there are no rules with generative non terms")
        }
        
        startNonTerm = newStart
    }
    
    private func deleteRulesWithNotReachableNonTerms() { // удаление правил, содержащих недостижимые нетерминалы
        var reachableNonTerms: Set<GrammarSymbol> = [startNonTerm]

        var count = 0
        while count != reachableNonTerms.count {
            count = reachableNonTerms.count
            
            rules.forEach { rule in
                if reachableNonTerms.contains(rule.left) {
                    reachableNonTerms.formUnion(rule.nonTerms)
                }
            }
        }
        
        var i = 0
        while i < rules.count {
            if !reachableNonTerms.contains(rules[i].left) || !reachableNonTerms.isSuperset(of: rules[i].nonTerms) {
                rules.remove(at: i)
            } else {
                i += 1
            }
        }
        
        guard let newStart = rules.first?.left else {
            fatalError("there are no rules with reachable non terms")
        }
        
        startNonTerm = newStart
    }
}

// MARK: - For parsing grammar rules from file

extension Grammar {
    
    private static func parseGrammarRules(from stringRules: [String]) -> [GrammarRule] {
        var rules: [GrammarRule] = []
        stringRules.forEach { stringRule in
            let splitted = stringRule.split(separator: Constants.grammarRuleSeparator)
            let left = splitted[0], right = splitted[1]
            guard
                left.count == 1,
                let leftSymbol = left.first?.grammarSymbol
            else {
                fatalError("left part of grammar's rule must contain one non terminal: \(stringRule)")
            }
            
            guard leftSymbol.isNonTerm else {
                fatalError("left part of grammar's rule must be non terminal: \(stringRule)")
            }
            
            let rights: [[GrammarSymbol]] = right.split(separator: "|").map { $0.map { $0.grammarSymbol } }
            rights.forEach { rightSymbols in
                rules.append(.init(left: leftSymbol, right: rightSymbols))
            }
        }
        
        return rules
    }
}

// MARK: - FIRST set

extension Grammar {
    
    private func makeFirstSet() {
        var count = -1
        
        while count != firstSet.countOfAllValues {
            count = firstSet.countOfAllValues
            rules.forEach { rule in
                let firstSetOfNonTerm = getFirstSet(of: rule.right)
                firstSet[rule.left]?.formUnion(firstSetOfNonTerm)
            }
        }
    }
    
    private func getFirstSet(of list: [GrammarSymbol]) -> Set<GrammarSymbol> {
        guard let firstSymbol = list.first else {
            var epsSet = Set<GrammarSymbol>()
            epsSet.insert(.eps)
            return epsSet
        }
        
        var first = Set<GrammarSymbol>()
        switch firstSymbol {

        case .term(_), .eps:
            first.insert(firstSymbol)
            return first

        case .end:
            fatalError("can't get first of $")
            
        case .nonTerm(_):
            guard let firstSetOfNonTerm = firstSet[firstSymbol] else {
                fatalError("there is no first set for non term: \(firstSymbol.value)")
            }
            
            first = firstSetOfNonTerm.withRemoving(.eps)
            if firstSetOfNonTerm.contains(.eps) {
                let firstSetOfSuffix = getFirstSet(of: list.withRemovingFirst())
                first.formUnion(firstSetOfSuffix)
            }
            
            return first
        }
    }
}

// MARK: - FOLLOW set

extension Grammar {
    
    private func makeFollowSet() {
        followSet[startNonTerm]?.insert(.end)
        var count = -1
        
        while count != followSet.countOfAllValues {
            count = followSet.countOfAllValues
            rules.forEach { rule in
                let right = rule.right

                for (index, symbol) in right.enumerated() {
                    if symbol.isNonTerm {
                        let suffix = Array(right.suffix(from: index + 1))
                        let firstSetOfSuffix = getFirstSet(of: suffix)

                        followSet[symbol]?.formUnion(firstSetOfSuffix.withRemoving(.eps))
                        if firstSetOfSuffix.contains(.eps), let followSetOfLeft = followSet[rule.left] {
                            followSet[symbol]?.formUnion(followSetOfLeft)
                        }
                    }
                }
            }
        }
    }
}

// MARK: - For FSM

extension Grammar {
    
    func getRulesByLeft(nonTerm: GrammarSymbol) -> Set<GrammarRule> {
        let rulesByLeft = rules.filter { $0.left == nonTerm }
        return Set(rulesByLeft)
    }
    
    func getRulesConvertedToLR0Items() -> Set<LR0Item> {
        let items: [LR0Item] = rules.map { $0.convertedToLR0Item() }
        return Set(items)
    }
    
    func getIndexOfRule(by item: LR0Item) -> Int? {
        rules.firstIndex(of: item.toGrammarRule())
    }
    
    func getFollowSet(of nonTerm: GrammarSymbol) -> Set<GrammarSymbol>? {
        followSet[nonTerm]
    }
}

// MARK: - LR0Item

struct LR0Item: Hashable {
    
    private var point: Int = 0
    var grammarRule: GrammarRule
    
    var observingToken: GrammarSymbol {
        grammarRule.right[point]
    }
    
    init(grammarRule: GrammarRule) {
        let left = grammarRule.left
        var right = grammarRule.right
        if let firstElement = right.first, firstElement == .eps, right.count == 1 { // A -> e = A -> .$
            right = []
        }
        self.grammarRule = GrammarRule(left: left, right: right.withAppending(.end))
    }
    
    init(point: Int, grammarRule: GrammarRule) {
        self.point = point
        self.grammarRule = grammarRule
    }
    
    func goto(by token: GrammarSymbol) -> LR0Item? {
        guard observingToken == token else {
            return nil
        }
        
        let situation = LR0Item(point: point + 1, grammarRule: grammarRule)
        return situation
    }
    
    func toGrammarRule() -> GrammarRule {
        var right = grammarRule.right.withRemovingLast()
        if right.isEmpty {
            right.append(.eps)
        }
        
        let rule = GrammarRule(left: grammarRule.left, right: right)
        return rule
    }
    
    func printItem() {
        var right = grammarRule.right.reduce(into: "") { $0 += String($1.value) }
        let index = right.index(right.startIndex, offsetBy: point)
        right.insert(".", at: index)
        let string = "\(grammarRule.left.value) -> \(right)"
        print(string)
    }
}

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

// MARK: - Control Table Value

enum ControlTableValue {
    
    case some(state: Int)
    case shift(state: Int)
    case reduce(indexOfRule: Int)
    
    var value: String {
        switch self {
        case .some(let state):
            return String(state)
        case .shift(let state):
            return "s(\(state))"
        case .reduce(let indexOfRule):
            return "r(\(indexOfRule))"
        }
    }
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
        
        fillControlTable()
    }
    
    private func fillControlTable() {
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
        
        states.filter { $0.isFinal }.forEach { state in
            guard 
                let item = state.endedItem,
                let index = grammar.getIndexOfRule(by: item),
                let followSet = grammar.getFollowSet(of: item.grammarRule.left)
            else {
                fatalError("something wrong")
            }
            
            followSet.forEach { term in
                controlTable[state.id]?.updateValue(.reduce(indexOfRule: index), forKey: term)
            }
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
        
        for (state, dict) in controlTable.sorted(by: { $0.key < $1.key }) {
            print("\(state):")
            for (symbol, action) in dict {
                print("\(symbol.value) = \(action.value)")
            }
            print()
        }
    }
}

// MARK: - Help Functions

func readFromFile(fileName: String) -> [String] {
    #if DEBUG
    let currentDirectory = ""
    #else
    let currentDirectory = FileManager.default.currentDirectoryPath
    #endif
    guard let text = try? String(contentsOfFile: currentDirectory + "/\(fileName)") else {
        return []
    }

    let lines = text.split(separator: "\n").map { String($0).removingSpaces() }
    return lines
}

// MARK: - Constants

enum Constants {

    #if DEBUG
    static let grammarFileName = "/Users/vsevolond/UNIVERSITY/FormalLanguage_Labs/Labs/lab4/grammar.txt"
    #else
    static let grammarFileName = "grammar.txt"
    #endif
    static let grammarRuleSeparator = "->"
}

// MARK: - MAIN

let file = readFromFile(fileName: Constants.grammarFileName)
let grammar = Grammar(from: file)
grammar.printGrammar()
print()
do {
    let fsm = try FSM(from: grammar)
    fsm.printFSM()
} catch {
    print(error)
}


