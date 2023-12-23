import Foundation

// MARK: - Extensions

extension Dictionary where Value: SetAlgebra, Value.Element: Hashable {
    
    var countOfAllValues: Int {
        let count = values.compactMap { $0 as? Set<Value.Element> }.map { $0.count }.reduce(0, +)
        return count
    }
}

extension Array {
    
    func withRemovingFirst() -> Array<Element> {
        var newArray = self
        newArray.removeFirst()
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

struct GrammarRule {

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
        
        firstSet.forEach { key, value in
            print("\(key.value): \(value.map { $0.value })")
        }
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
