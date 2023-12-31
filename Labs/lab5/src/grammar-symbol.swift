import Foundation

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
            return "@"
        }
    }
}
