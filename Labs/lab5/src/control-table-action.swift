import Foundation

// MARK: - Control Table Value

enum ControlTableAction {
    
    case some(state: Int, nonTerm: GrammarSymbol)
    case shift(state: Int)
    case reduce(by: GrammarRule)
    case accept
    
    var priority: Int {
        switch self {
        case .some(_, _):
            return 0
        case .shift(_):
            return 1
        case .reduce(_):
            return 2
        case .accept:
            return 3
        }
    }
}

extension ControlTableAction: Comparable {
    static func < (lhs: ControlTableAction, rhs: ControlTableAction) -> Bool {
        return lhs.priority < rhs.priority
    }
}
