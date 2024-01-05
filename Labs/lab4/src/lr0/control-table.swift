import Foundation

// MARK: - Control Table Error

enum ControlTableError: Error {
    
    case existAction
    case notExistAction
}

// MARK: - Control Table

struct ControlTable {
    
    private var rows: [Int: ControlTableRow] = [:]
    
    mutating func set(to state: Int, by symbol: GrammarSymbol, action: ControlTableAction) throws {
        if rows[state] == nil {
            rows[state] = .init()
        }
        
        try rows[state]?.set(to: symbol, action: action)
    }
    
    func get(for state: Int, by symbol: GrammarSymbol) throws -> ControlTableAction {
        guard let row = rows[state] else {
            throw ControlTableError.notExistAction
        }
        
        return try row.getAction(for: symbol)
    }
}
