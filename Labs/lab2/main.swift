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

extension Array where Element == Transition {
    func union() -> Element? {
        guard let first else {
            return nil
        }
        let tree = Tree(root: first.by)
        for transition in self {
            tree.addNode(node: transition.by)
        }
        let transition = Transition(from: first.from, to: first.to, by: tree.root)
        return transition
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
    
    enum Operation: Equatable, Hashable {
        
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
    
    enum Terminal: Equatable, Hashable, Comparable {
        
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
    
    var symbols: [Symbol] = []
    var terminals = Set<Terminal>()
    
    init(string: String) {
        setup(from: string)
    }
    
    mutating private func setup(from string: String) {
        let array: [Character] = string.split(separator: "").map { Character(String($0)) }
        
        for symbol in array {
            switch symbol.formatted() {
            case .letter(let char):
                terminals.insert(.letter(char))
            case .unexpected:
                fatalError("Unexpected symbol: \(symbol)")
            default:
                break
            }
            symbols.append(symbol.formatted())
        }
    }
}

// MARK: - Node

class Node: Equatable, Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(value)
        hasher.combine(parent)
        hasher.combine(left)
        hasher.combine(right)
    }
    
    static func == (lhs: Node, rhs: Node) -> Bool {
        return lhs.value == rhs.value && lhs.left == rhs.left && lhs.right == rhs.right
    }
    
    enum Value: Equatable, Hashable {
        
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
    
    private func setup(as node: Node) {
        let newNode = node
        newNode.left?.parent = self
        newNode.right?.parent = self
        self.left = newNode.left
        self.right = newNode.right
        self.value = newNode.value
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
            self.right = newNode
        } else {
            self.left = newNode
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
                    if left.value == value {
                        setup(as: left)
                    } else {
                        setup(as: right)
                    }
                    
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
                    if left.value == value {
                        setup(as: left)
                    } else {
                        setup(as: right)
                    }
                    
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
            switch self.value {
                
            case .terminal(_):
                return
                
            case .operation(let operation):
                switch operation {
                    
                case .concat, .shuffle:
                    setNode(node: left)
                    setNode(node: right)
                    
                case .union:
                    if left == right || (right == left.left || right == left.right) && left.value == .operation(.union) {
                        setup(as: left)
                    } else if (left == right.left || left == right.right) && right.value == .operation(.union) {
                        setup(as: right)
                    } else {
                        setNode(node: left)
                        setNode(node: right)
                    }
                    
                case .iteration:
                    return
                }
            }
        }
    }
    
    func hasEmptyString() -> Bool {
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
    
    func derivative(by letter: Regex.Terminal) -> Node {
        switch value {
            
        case .terminal(let terminal):
            if terminal == letter {
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
                
                newNode.setChilds(left: left.derivative(by: letter), right: self)
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
    var root: Node
    var terminals = Set<Regex.Terminal>()
    
    init(root: Node) {
        self.root = root
    }
    
    init(regex: Regex) {
        let symbols = regex.symbols
        self.terminals = regex.terminals
        self.root = Self.setup(with: symbols)
    }
    
    static private func setup(with array: [Regex.Symbol]) -> Node {
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
        
        let root = nodesStack.pop()
        guard nodesStack.top() == nil else {
            fatalError("Stack is not empty")
        }
        return root
    }
    
    func addNode(node: Node) {
        let newNode = Node(value: .operation(.union))
        newNode.setChilds(left: root, right: node)
        root = newNode
    }
    
    func toRegex() -> String {
        return root.toRegex()
    }
    
    func derivative(by terminal: Regex.Terminal) -> Tree {
        let newRoot = root.derivative(by: terminal)
        return Tree(root: newRoot)
    }
    
    func hasEmptyWord() -> Bool {
        return root.hasEmptyString()
    }
}

// MARK: - FSM State

struct State: Equatable {
    let id: Int
    let regex: Node
    
    static func ==(lhs: State, rhs: State) -> Bool {
        return lhs.regex == rhs.regex
    }
}

// MARK: - FSM Transition

struct Transition: Equatable {
    let from: State
    let to: State
    let by: Node
    
    func toRegex() -> String {
        return by.toRegex()
            .replacingOccurrences(of: "&", with: "")
            .replacingOccurrences(of: "(", with: "")
            .replacingOccurrences(of: ")", with: "")
    }
}

// MARK: - Automata

struct FSM {
    var states = [State]()
    var terminals: Set<Regex.Terminal>
    var transitions = [Transition]()
    var initialState: State
    var finalStates = [State]()
    
    init(tree: Tree) {
        self.terminals = tree.terminals
        self.initialState = .init(id: 0, regex: tree.root)
        self.states.append(initialState)
        if tree.root.hasEmptyString() {
            finalStates.append(initialState)
        }
        setup(by: tree, from: initialState)
    }
    
    mutating private func setup(by tree: Tree, from state: State) {
        for terminal in terminals.sorted() {
            let derivative = tree.derivative(by: terminal)
            guard derivative.root.value != .terminal(.emptySet) else {
                continue
            }
            let newState = State(id: states.count, regex: derivative.root)
            var newTransition: Transition
            if let existState = states.first(where: { $0 == newState }) {
                newTransition = .init(from: state, to: existState, by: Node(value: .terminal(terminal)))
            } else {
                states.append(newState)
                if derivative.hasEmptyWord() {
                    finalStates.append(newState)
                }
                newTransition = .init(from: state, to: newState, by: Node(value: .terminal(terminal)))
                setup(by: derivative, from: newState)
            }
            transitions.append(newTransition)
        }
    }
    
    func debug() {
        for state in states.sorted(by: { $0.id < $1.id }) {
            if state == initialState {
                let isFinal = finalStates.contains(state)
                print("q\(state.id) = \(state.regex.toRegex()) - initial\(isFinal ? ", final" : "")")
            } else {
                let isFinal = finalStates.contains(state)
                print("q\(state.id) = \(state.regex.toRegex())\(isFinal ? " - final" : "")")
            }
        }
        for transition in transitions.sorted(by: { $0.from.id < $1.from.id || ($0.from.id == $1.from.id && $0.to.id < $1.to.id) }) {
            let from = transition.from.id
            let to = transition.to.id
            let by = transition.toRegex()
            print("q\(from) -- \(by) --> q\(to)")
        }
    }
    
    mutating func toRegex() -> String {
        makeNewInitialState()
        makeNewFinalState()
        eliminateStates()
        
        guard let union = transitions.union() else {
            fatalError("no transitions")
        }
        return union.by.toRegex()
    }
    
    mutating private func eliminateStates() {
        while states.count != 0 {
            let state = states.removeFirst()
            
            let iterationTransition = transitions.filter { $0.from == state && $0.from == $0.to }.union()
            let toStateTransitions = transitions.filter { $0.to == state && $0.from != state }
            let fromStateTransitions = transitions.filter { $0.from == state && $0.to != state }
            
            
            for toStateTransition in toStateTransitions {
                for fromStateTransition in fromStateTransitions {
                    let newNode = Node(value: .operation(.concat))
                    if let iterationTransition {
                        let leftNode = Node(value: .operation(.concat))
                        let iterationNode = Node(value: .operation(.iteration))
                        iterationNode.setNode(node: iterationTransition.by)
                        leftNode.setChilds(left: toStateTransition.by, right: iterationNode)
                        newNode.setChilds(left: leftNode, right: fromStateTransition.by)
                    } else {
                        newNode.setChilds(left: toStateTransition.by, right: fromStateTransition.by)
                    }
                    let newTransition = Transition(from: toStateTransition.from, to: fromStateTransition.to, by: newNode)
                    transitions.append(newTransition)
                }
            }
            
            transitions.removeAll { $0.from.id == state.id || $0.to.id == state.id }
        }
    }
    
    mutating private func makeNewInitialState() {
        let newInitialState = State(id: -1, regex: Node(value: .terminal(.emptySet)))
        let newTransition = Transition(from: newInitialState, to: initialState, by: Node(value: .terminal(.epsilon)))
        transitions.append(newTransition)
        initialState = newInitialState
    }
    
    mutating private func makeNewFinalState() {
        guard finalStates.count > 0 else {
            fatalError("no final states")
        }
        let newFinalState = State(id: states.count, regex: Node(value: .terminal(.emptySet)))
        for finalState in finalStates {
            let newTransition = Transition(from: finalState, to: newFinalState, by: Node(value: .terminal(.epsilon)))
            transitions.append(newTransition)
        }
        finalStates = [newFinalState]
    }
}

func readFromFile() -> [String] {
    let fileName = "output.txt"
    guard let text = try? String(contentsOfFile: fileName) else {
        return []
    }
    let split = text.split(separator: "\n")
    var res: [String] = []
    
    for line in split {
        let splitLine = line.components(separatedBy: CharacterSet(charactersIn: " ")).filter { !$0.isEmpty }
        res.append(contentsOf: splitLine)
    }
    
    return res
}

struct TransitionJSONModel: Encodable {
    let from: Int
    let to: Int
    let by: String
    
    private enum CodingKeys: String, CodingKey {
        case from = "from_state"
        case to = "to_state"
        case by = "by_symbol"
    }
    
    init(from transition: Transition) {
        from = transition.from.id
        to = transition.to.id
        by = transition.by.toRegex()
    }
}

struct FSMJSONModel: Encodable {
    let initialState: Int
    let states: [Int]
    let finalStates: [Int]
    let transitions: [TransitionJSONModel]
    
    private enum CodingKeys: String, CodingKey {
        case initialState = "initial_state"
        case states
        case finalStates = "final_states"
        case transitions
    }
    
    init(from fsm: FSM) {
        initialState = fsm.initialState.id
        states = fsm.states.map { $0.id }
        finalStates = fsm.finalStates.map { $0.id }
        transitions = fsm.transitions.map { .init(from: $0) }
    }
}

struct Response: Encodable {
    let input: String
    let output: String
    let fsm: FSMJSONModel
}

// MARK: - MAIN

// x* = xx*|ε = xx∗|x∗ ?????
var responses: [Response] = []
let regs = readFromFile()
for reg in regs {
    let regex = Regex(string: reg)
    let tree = Tree(regex: regex)
    let fsm = FSM(tree: tree)
    var outputFSM = fsm
    let output = outputFSM.toRegex()
    let response = Response(input: reg, output: output, fsm: .init(from: fsm))
    responses.append(response)
}

let encoder = JSONEncoder()
encoder.outputFormatting = .prettyPrinted
let data = try encoder.encode(responses)
guard let string = String(data: data, encoding: .utf8) else {
    fatalError("error in encoding data")
}

let directoryPath = FileManager.default.currentDirectoryPath
let resultPath = directoryPath.appending("/result.json")
FileManager.default.createFile(atPath: resultPath, contents: nil)
try string.write(toFile: resultPath, atomically: true, encoding: .utf8)



