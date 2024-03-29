import Foundation

// MARK: - Control Table

struct ControlTable {
    
    private var rows: [Int: ControlTableRow] = [:]
    
    mutating func add(to state: Int, by symbol: GrammarSymbol, action: ControlTableAction) {
        if rows[state] == nil {
            rows[state] = .init()
        }
        
        rows[state]?.add(to: symbol, action: action)
    }
    
    func get(for state: Int, by symbol: GrammarSymbol) -> [ControlTableAction] {
        guard let row = rows[state] else {
            fatalError("there is no row for state: \(state)")
        }
        
        return row.getValues(for: symbol)
    }
}
