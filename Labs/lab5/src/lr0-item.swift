import Foundation

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

