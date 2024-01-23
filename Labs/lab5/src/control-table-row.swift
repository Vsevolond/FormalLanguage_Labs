import Foundation

// MARK: - Control Table Row

struct ControlTableRow {
    
    private var values: [GrammarSymbol: [ControlTableAction]] = [:]
    
    mutating func add(to symbol: GrammarSymbol, action: ControlTableAction) {
        if values[symbol] == nil {
            values.updateValue([action], forKey: symbol)
        } else {
            values[symbol]?.append(action)
        }
    }
    
    func getValues(for symbol: GrammarSymbol) -> [ControlTableAction] {
        values[symbol] ?? []
    }
}
