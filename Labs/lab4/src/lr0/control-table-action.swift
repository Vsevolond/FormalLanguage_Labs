import Foundation

// MARK: - Control Table Value

enum ControlTableAction {
    
    case some(state: Int)
    case shift(state: Int)
    case reduce(by: GrammarRule)
    case accept
    
    var value: String {
        switch self {
        case .some(let state):
            return String(state)
        case .shift(let state):
            return "s(\(state))"
        case .reduce(let rule):
            return "r(\(rule.stringValue))"
        case .accept:
            return "accept"
        }
    }
}
