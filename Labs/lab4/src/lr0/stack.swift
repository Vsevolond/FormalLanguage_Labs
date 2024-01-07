import Foundation

// MARK: - Stack

struct Stack<Element> {

    var items: [Element] = []
    
    var isEmpty: Bool {
        top() == nil
    }

    mutating func push(_ item: Element) {
        items.append(item)
    }

    @discardableResult
    mutating func pop() -> Element {
        return items.removeLast()
    }
    
    @discardableResult
    mutating func pop(count: Int) -> [Element] {
        guard count <= items.count else {
            fatalError("count is more than count of exist items")
        }

        var top = [Element]()
        (0..<count).forEach { _ in
            top.append(pop())
        }
        
        return top
    }

    func top() -> Element? {
        return items.last
    }
}

