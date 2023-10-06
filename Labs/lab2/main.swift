import Foundation

// MARK: - Extensions

extension Character {
    
    func formatted() -> Regex.Symbol {
        switch self {
        case "(": return .openBracket
        case ")": return .closeBracket
        case "*": return .iteration
        case "&": return .concat
        case "|": return .union
        case "#": return .shuffle
        case "e": return .epsilon
        case "0": return .emptySet
        case "a"..."z": return .letter(self)
        default: return .unexpected
        }
    }
}

// MARK: - Stack

struct Stack<Element> {
    
    private var items: [Element] = []
    
    mutating func push(_ item: Element) {
        items.append(item)
    }
    
    mutating func pop() -> Element {
        return items.removeLast()
    }
    
    func top() -> Element? {
        return items.last
    }
}

// MARK: - Regex

struct Regex {
    
    enum Operation: Equatable {
        
        case union
        case concat
        case iteration
        case shuffle
        
        var value: Character {
            switch self {
            case .union: return "|"
            case .concat: return "&"
            case .iteration: return "*"
            case .shuffle: return "#"
            }
        }
    }
    
    enum Terminal: Equatable {
        
        case epsilon
        case emptySet
        case letter(Character)
        
        var value: Character {
            switch self {
            case .epsilon: return "e"
            case .emptySet: return "0"
            case .letter(let char): return char
            }
        }
    }
    
    enum Symbol: Equatable {
        
        case openBracket
        case closeBracket
        case iteration
        case concat
        case union
        case shuffle
        case epsilon
        case emptySet
        case letter(Character)
        case unexpected
        
        var priority: Int {
            switch self {
            case .union: return 1
            case .shuffle: return 2
            case .concat: return 3
            case .iteration: return 4
            default: return 0
            }
        }
    }
    
    let string: String
    
    func formatted() -> [Symbol] {
        let array: [Character] = string.split(separator: "").map { Character(String($0)) }
        var result: [Symbol] = []
        
        for symbol in array {
            if symbol.formatted() == .unexpected {
                fatalError("Unexpected symbol: \(symbol)")
            }
            result.append(symbol.formatted())
        }
        
        return result
    }
}

// MARK: - Node

class Node {
    
    enum Value: Equatable {
        
        case terminal(Regex.Terminal)
        case operation(Regex.Operation)
    }
    
    var value: Value
    var parent: Node? = nil
    var left: Node? = nil
    var right: Node? = nil
    
    init(value: Value) {
        self.value = value
    }
    
    func setNode(node: Node) {
        switch node.value {
            
        case .terminal(let terminal):
            if (terminal == .epsilon || terminal == .emptySet) && value == .operation(.iteration) {
                value = .terminal(terminal)
                return
            }
            
        default:
            break
        }
        
        let newNode = node
        newNode.parent = self
        if left != nil {
            right = newNode
        } else {
            left = newNode
        }
    }
    
    func setChilds(left: Node, right: Node) {
        switch (left.value, right.value) {
            
        case (let value, .terminal(.emptySet)), (.terminal(.emptySet), let value):
            switch self.value {
                
            case .terminal(_):
                return
                
            case .operation(let operation):
                switch operation {
                    
                case .concat, .shuffle:
                    self.value = .terminal(.emptySet)
                    
                case .union:
                    self.value = value
                    
                case .iteration:
                    return
                }
            }
            
        case (let value, .terminal(.epsilon)), (.terminal(.epsilon), let value):
            switch self.value {
                
            case .terminal(_):
                return
                
            case .operation(let operation):
                switch operation {
                    
                case .concat, .shuffle:
                    self.value = value
                    
                case .union:
                    if value == .terminal(.epsilon) {
                        self.value = value
                    } else {
                        setNode(node: left)
                        setNode(node: right)
                    }
                    
                case .iteration:
                    return
                }
            }
            
        default:
            setNode(node: left)
            setNode(node: right)
        }
    }
    
    private func hasEmptyString() -> Bool {
        switch value {
            
        case .terminal(let terminal):
            return terminal == .epsilon
            
        case .operation(let operation):
            switch operation {
                
            case .union:
                guard let left, let right else {
                    return false
                }
                return left.hasEmptyString() || right.hasEmptyString()
                
            case .concat, .shuffle:
                guard let left, let right else {
                    return false
                }
                return left.hasEmptyString() && right.hasEmptyString()
                
            case .iteration:
                return true
            }
        }
    }
    
    func derivative(by letter: Character) -> Node {
        switch value {
            
        case .terminal(let terminal):
            if terminal.value == letter {
                return Node(value: .terminal(.epsilon))
            } else {
                return Node(value: .terminal(.emptySet))
            }
            
        case .operation(let operation):
            switch operation {
                
            case .union:
                let newNode = Node(value: .operation(.union))
                guard let left, let right else {
                    return newNode
                }
                let leftDerivative = left.derivative(by: letter)
                let rightDerivative = right.derivative(by: letter)
                
                newNode.setChilds(left: leftDerivative, right: rightDerivative)
                return newNode
                
            case .concat:
                let newNode = Node(value: .operation(.union))
                guard let left, let right else {
                    return newNode
                }
                let leftNode = Node(value: .operation(.concat))
                let rightNode = Node(value: .operation(.concat))
                
                leftNode.setChilds(left: left.derivative(by: letter), right: right)
                if left.hasEmptyString() {
                    rightNode.setChilds(
                        left: .init(value: .terminal(.epsilon)),
                        right: right.derivative(by: letter)
                    )
                } else {
                    rightNode.setChilds(
                        left: .init(value: .terminal(.emptySet)),
                        right: right.derivative(by: letter)
                    )
                }
                
                newNode.setChilds(left: leftNode, right: rightNode)
                return newNode
                
            case .iteration:
                let newNode = Node(value: .operation(.concat))
                guard let left else {
                    return newNode
                }
                
                newNode.setChilds(left: left.derivative(by: letter), right: left)
                return newNode
                
            case .shuffle:
                let newNode = Node(value: .operation(.union))
                guard let left, let right else {
                    return newNode
                }
                
                let leftNode = Node(value: .operation(.shuffle))
                let rightNode = Node(value: .operation(.shuffle))
                leftNode.setChilds(left: left.derivative(by: letter), right: right)
                rightNode.setChilds(left: left, right: right.derivative(by: letter))
                
                newNode.setChilds(left: leftNode, right: rightNode)
                return newNode
            }
        }
    }
    
    func toRegex() -> String {
        switch value {
        case .terminal(let terminal):
            return String(terminal.value)
        case .operation(let operation):
            switch operation {
            case .union, .concat, .shuffle:
                guard let left, let right else {
                    return ""
                }
                let leftRegex = left.toRegex()
                let rightRegex = right.toRegex()
                return "(\(leftRegex)\(operation.value)\(rightRegex))"
            case .iteration:
                guard let left else {
                    return ""
                }
                let regex = left.toRegex()
                return "(\(regex)\(operation.value))"
            }
        }
    }
}

// MARK: - Tree

class Tree {
    var root: Node?
    var alphabet = Set<Character>()
    
    init(root: Node) {
        self.root = root
    }
    
    init(regex: Regex) {
        let formatted = regex.formatted()
        setup(with: formatted)
    }
    
    private func setup(with array: [Regex.Symbol]) {
        var symbolsStack = Stack<Regex.Symbol>()
        var nodesStack = Stack<Node>()
        
        let perform: () -> Void = {
            let poped = symbolsStack.pop()
            switch poped {
                
            case .iteration:
                let node = nodesStack.pop()
                let newNode = Node(value: .operation(.iteration))
                
                newNode.setNode(node: node)
                nodesStack.push(newNode)
                
            default:
                let rightNode = nodesStack.pop()
                let leftNode = nodesStack.pop()
                
                var newNode: Node
                if poped == .concat {
                    newNode = Node(value: .operation(.concat))
                } else if poped == .union {
                    newNode = Node(value: .operation(.union))
                } else {
                    newNode = Node(value: .operation(.shuffle))
                }
                
                newNode.setChilds(left: leftNode, right: rightNode)
                nodesStack.push(newNode)
            }
        }
        
        for sym in array {
            switch sym {
                
            case .openBracket:
                symbolsStack.push(sym)
                
            case .closeBracket:
                while symbolsStack.top() != nil && symbolsStack.top()! != .openBracket {
                    perform()
                }
                let _ = symbolsStack.pop()
                
            case .letter(let char):
                let newNode = Node(value: .terminal(.letter(char)))
                nodesStack.push(newNode)
                alphabet.insert(char)
                
            case .unexpected:
                continue
                
            default:
                while symbolsStack.top() != nil && symbolsStack.top()!.priority >= sym.priority {
                    perform()
                }
                symbolsStack.push(sym)
            }
        }
        
        while symbolsStack.top() != nil {
            perform()
        }
        
        root = nodesStack.pop()
        guard nodesStack.top() == nil else {
            fatalError("Stack is not empty")
        }
    }
    
    func toRegex() -> String {
        guard let root else {
            return "no tree"
        }
        return root.toRegex()
    }
    
    func derivative(by letter: Character) -> Tree {
        guard let root else {
            fatalError("no tree")
        }
        let newRoot = root.derivative(by: letter)
        return Tree(root: newRoot)
    }
}

// MARK: - Automata

class FSM {
    
}

// MARK: - MAIN

let regex = Regex(string: "((((a|b)*)&a)&(a|b))")
let tree = Tree(regex: regex)
print(tree.toRegex())
let derivative = tree.derivative(by: "a")
print(derivative.toRegex())
