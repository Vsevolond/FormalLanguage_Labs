import Foundation

// MARK: - Grammar

class Grammar {

    private var nonTerms: Set<GrammarSymbol>
    private var terms: Set<GrammarSymbol>
    private var rules: [GrammarRule]
    var startNonTerm: GrammarSymbol
    
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
        
        makeFirstSet()
        makeFollowSet()
    }
    
    func printGrammar() {
        rules.forEach { rule in
            print(rule.stringValue)
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
    
    func getFollowSet(of nonTerm: GrammarSymbol) -> Set<GrammarSymbol>? {
        followSet[nonTerm]
    }
}

// MARK: - Constants

private enum Constants {
    
    static let grammarRuleSeparator = "->"
}
