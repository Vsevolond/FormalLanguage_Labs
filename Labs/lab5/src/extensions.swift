import Foundation

// MARK: - Extensions

extension Dictionary where Value: SetAlgebra, Value.Element: Hashable {
    
    var countOfAllValues: Int {
        let count = values.compactMap { $0 as? Set<Value.Element> }.map { $0.count }.reduce(0, +)
        return count
    }
}

extension Array {
    
    func withInserting(_ element: Element, at index: Int) -> Array<Element> {
        var newArray = self
        newArray.insert(element, at: index)
        return newArray
    }
    
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
    
    func pairForEach(_ handler: ((Element, Element)) -> Void) {
        guard count > 1 else { return }
        
        for i in 0..<count-1 {
            let pair = (self[i], self[i + 1])
            handler(pair)
        }
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
    
    mutating func removeAll(where satisfy: (Element) -> Bool) {
        self = filter { !satisfy($0) }
    }
    
    func combinePairs(with set: Set<Element>) -> [(Element, Element)] {
        var pairs: [(Element, Element)] = []
        forEach { elem1 in
            set.forEach { elem2 in
                pairs.append((elem1, elem2))
            }
        }
        
        return pairs
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
        var string = self
        string.removeAll(where: { $0 == " " })
        return string
    }
}
