import Foundation

// MARK: - Extensions

extension Dictionary where Value: SetAlgebra, Value.Element: Hashable {
    
    var countOfAllValues: Int {
        let count = values.compactMap { $0 as? Set<Value.Element> }.map { $0.count }.reduce(0, +)
        return count
    }
}

extension Array {
    
    func withAppending(_ element: Element) -> Array<Element> {
        var newArray = self
        newArray.append(element)
        return newArray
    }
    
    func withRemovingFirst() -> Array<Element> {
        var newArray = self
        newArray.removeFirst()
        return newArray
    }
    
    func withRemovingLast() -> Array<Element> {
        var newArray = self
        newArray.removeLast()
        return newArray
    }
}

extension Set {
    
    func withInserting(_ element: Element) -> Set<Element> {
        var newSet = self
        newSet.insert(element)
        return newSet
    }
    
    func withRemoving(_ element: Element) -> Set<Element> {
        var newSet = self
        newSet.remove(element)
        return newSet
    }
}

extension Character {

    var grammarSymbol: GrammarSymbol {
        switch self {
        case "e": return .eps
        case "@": return .end
        default:
            if self.isLetter, self.isUppercase {
                return .nonTerm(self)
            } else {
                return .term(self)
            }
        }
    }
}

extension String {

    func removingSpaces() -> String {
        return replacingOccurrences(of: " ", with: "")
    }
}
