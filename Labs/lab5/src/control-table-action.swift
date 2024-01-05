import Foundation

// MARK: - Control Table Value

enum ControlTableAction {
    
    case some(state: Int)
    case shift(state: Int)
    case reduce(by: GrammarRule)
    
    var value: String {
        switch self {
        case .some(let state):
            return String(state)
        case .shift(let state):
            return "s(\(state))"
        case .reduce(let rule):
            return "r(\(rule.stringValue))"
        }
    }
    
    var priority: Int {
        switch self {
        case .some(_):
            return 0
        case .shift(_):
            return 1
        case .reduce(_):
            return 2
        }
    }
}

extension ControlTableAction: Comparable {
    static func < (lhs: ControlTableAction, rhs: ControlTableAction) -> Bool {
        return lhs.priority < rhs.priority
    }
}
