import Foundation

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
    
    var stringValue: String {
        "\(left.value) -> \(right.reduce(into: "") { $0 += String($1.value) })"
    }
    
    init(left: GrammarSymbol, right: [GrammarSymbol]) {
        self.left = left
        self.right = right
    }
    
    func convertedToLR0Item() -> LR0Item {
        .init(grammarRule: self)
    }
}
