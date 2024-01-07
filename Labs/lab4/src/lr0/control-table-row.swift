import Foundation

// MARK: - Control Table Row

struct ControlTableRow {
    
    private var values: [GrammarSymbol: ControlTableAction] = [:]
    
    mutating func set(to symbol: GrammarSymbol, action: ControlTableAction) throws {
        guard values[symbol] == nil else {
            throw ControlTableError.existAction
        }
        
        values.updateValue(action, forKey: symbol)
    }
    
    func getAction(for symbol: GrammarSymbol) throws -> ControlTableAction {
        guard let value = values[symbol] else {
            throw ControlTableError.notExistAction
        }
        
        return value
    }
}
