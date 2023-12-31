import Foundation

// MARK: - Extensions

extension Character {

    func formatted() -> Regex.Symbol {
        switch self {
        case "(": return .openBracket
        case ")": return .closeBracket
        case "*": return .operation(.iteration)
        case "|": return .operation(.union)
        case "#": return .operation(.shuffle)
        case "a"..."z" where self != "e": return .terminal(.letter(self))
        default: return .unexpected
        }
    }
}

extension Array where Element == Transition {

    func union() -> Transition? {
        guard let first else {
            return nil
        }
        let tree = Tree(root: first.by)
        for transition in self {
            if transition != first {
                tree.addNode(node: transition.by)
            }
        }
        let transition = Transition(from: first.from, to: first.to, by: tree.root)
        return transition
    }

    func compactByFrom() -> [Transition] {
        var dict: [Int: [Transition]] = [:]
        for transition in self {
            if dict[transition.from.id] == nil {
                dict[transition.from.id] = [transition]
            } else {
                dict[transition.from.id]?.append(transition)
            }
        }
        var result: [Transition] = []
        for (_, transitions) in dict {
            guard let union = transitions.union() else {
                continue
            }
            result.append(union)
        }
        return result
    }

    func compactByTo() -> [Transition] {
        var dict: [Int: [Transition]] = [:]
        for transition in self {
            if dict[transition.to.id] == nil {
                dict[transition.to.id] = [transition]
            } else {
                dict[transition.to.id]?.append(transition)
            }
        }
        var result: [Transition] = []
        for (_, transitions) in dict {
            guard let union = transitions.union() else {
                continue
            }
            result.append(union)
        }
        return result
    }
}

extension Node: Equatable {

    static func == (lhs: Node, rhs: Node) -> Bool {
        return lhs.value == rhs.value && lhs.left == rhs.left && lhs.right == rhs.right
    }
}

extension State: Equatable {

    static func ==(lhs: State, rhs: State) -> Bool {
        return lhs.regex == rhs.regex
    }
}

// MARK: - Errors

enum CustomError: Error {

    case unexpectedSymbol(Character)
    case cannotParseRegex
    case noTransitionsInFSM(FSM)
    case noFinalStatesInFSM(FSM)
    case timeoutWhileMakingFSM
}

// MARK: - Stack

struct Stack<Element> {

    private var items: [Element] = []

    mutating func push(_ item: Element) {
        items.append(item)
    }

    @discardableResult
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
        case maybe

        var value: String {
            switch self {
            case .union: return "|"
            case .concat: return ""
            case .iteration: return "*"
            case .shuffle: return "#"
            case .maybe: return "?"
            }
        }

        var priority: Int {
            switch self {
            case .union: return 1
            case .shuffle: return 2
            case .concat: return 3
            case .iteration, .maybe: return 4
            }
        }
    }

    enum Terminal: Hashable, Comparable {

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
        case terminal(Terminal)
        case operation(Operation)
        case unexpected
    }

    var symbols: [Symbol] = []
    var terminals = Set<Terminal>()

    init(string: String) throws {
        try setup(from: string)
    }

    private mutating func checkLast() {
        if let last = symbols.last {
            switch last {
            case .terminal(_), .closeBracket, .operation(.iteration):
                symbols.append(.operation(.concat))

            default: return
            }
        }
    }

    private mutating func setup(from string: String) throws {
        let array: [Character] = Array(string)

        for symbol in array {
            let formatted = symbol.formatted()

            switch formatted {
            case .openBracket:
                checkLast()

            case .terminal(let terminal):
                checkLast()
                terminals.insert(terminal)

            case .unexpected:
                throw CustomError.unexpectedSymbol(symbol)

            default: break
            }
            symbols.append(formatted)
        }
    }
}

// MARK: - Node

class Node {

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

    func setNode(node: Node) {
        if case .terminal(let terminal) = node.value,
                or(terminal == .epsilon, terminal == .emptySet),
                or(self.value == .operation(.iteration), self.value == .operation(.maybe))
        { // e* -> e, 0* -> 0, e? -> e, 0? -> 0
            self.value = .terminal(terminal)

        } else if node.value == self.value,
                  or(self.value == .operation(.iteration), self.value == .operation(.maybe))
        { // (r*)* -> r*, (r?)? -> r?
            setup(as: node)

        } else if node.value != self.value,
                  or(self.value == .operation(.iteration), self.value == .operation(.maybe)),
                  or(node.value == .operation(.iteration), node.value == .operation(.maybe))
        { // (r*)? -> r*, (r?)* -> r*
            guard let leftNode = node.left else {
                return
            }
            self.value = .operation(.iteration)
            addNode(node: leftNode)

        } else if self.value == .operation(.maybe),
                  node.value == .operation(.concat),
                  or(
                      and(node.right?.value == .operation(.iteration), node.left == node.right?.left),
                      and(node.left?.value == .operation(.iteration), node.left?.left == node.right)
                  )
        { // xx*|e -> x*, x*x|e -> x*
            if let iterationNode = node.right, iterationNode.value == .operation(.iteration) {
                setup(as: iterationNode)
            } else if let iterationNode = node.left, iterationNode.value == .operation(.iteration) {
                setup(as: iterationNode)
            }

        } else {
            addNode(node: node)
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

                case .concat, .shuffle: // 0r -> 0, r0 -> 0, 0#r -> 0, r#0 -> 0
                    self.value = .terminal(.emptySet)

                case .union:
                    if left.value == value { // r|0 -> r
                        setup(as: left)

                    } else { // 0|r -> r
                        setup(as: right)
                    }

                case .iteration, .maybe:
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
                    if left.value == value { // re -> r, r#e -> r
                        setup(as: left)

                    } else { // er -> r, e#r -> r
                        setup(as: right)
                    }

                case .union:
                    let newNode = Node(value: .operation(.maybe))
                    if left.value == .terminal(.epsilon) {
                        newNode.setNode(node: right)
                    } else {
                        newNode.setNode(node: left)
                    }
                    setup(as: newNode)

                case .iteration, .maybe:
                    return
                }
            }

        default:
            switch self.value {

            case .terminal(_):
                return

            case .operation(let operation):
                switch operation {

                case .shuffle:
                    setNode(node: left)
                    setNode(node: right)
                    
                case .concat:
                    if and(
                        left.value == .operation(.iteration),
                        right.value == .operation(.iteration),
                        left.left == right.left
                    ) { // r*r* -> r*
                        setup(as: left)

                    } else {
                        setNode(node: left)
                        setNode(node: right)
                    }

                case .union:
                    if left == right { // r|r -> r
                        setup(as: left)

                    } else if and(
                        left.value == .operation(.union),
                        right.value == .operation(.union),
                        left.left == right.right,
                        left.right == right.left
                    ) { // (r1|r2)|(r2|r1) -> (r1|r2), (r2|r1)|(r1|r2) -> (r1|r2)
                        setup(as: left)

                    } else if and(
                        right.value == .operation(.union),
                        or(left == right.left, left == right.right)
                    ) { // r1|(r1|r2) -> r1|r2, r1|(r2|r1) -> r2|r1
                        setup(as: right)

                    } else if and(
                        left.value == .operation(.union),
                        or(right == left.left, right == left.right)
                    ) { // (r1|r2)|r1 -> (r1|r2), (r2|r1)|r1 -> (r2|r1)
                        setup(as: left)
                        
                    } else if case .terminal(let leftTerminal) = left.value,
                              case .terminal(let rightTerminal) = right.value,
                              leftTerminal.value > rightTerminal.value
                    { // b|a -> a|b
                        addNode(node: right)
                        addNode(node: left)

                    } else {
                        setNode(node: left)
                        setNode(node: right)
                    }

                case .iteration, .maybe:
                    return
                }
            }
        }
    }

    func hasEmptyWord() -> Bool {
        switch value {

        case .terminal(let terminal):
            return terminal == .epsilon

        case .operation(let operation):
            switch operation {

            case .union:
                guard let left, let right else {
                    return false
                }
                return left.hasEmptyWord() || right.hasEmptyWord()

            case .concat, .shuffle:
                guard let left, let right else {
                    return false
                }
                return left.hasEmptyWord() && right.hasEmptyWord()

            case .iteration, .maybe:
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
                if left.hasEmptyWord() {
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

            case .maybe:
                guard let left else {
                    return self
                }

                let node = left.derivative(by: letter)
                return node

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
        var isLessPriority = false
        if let parent,
            case .operation(let parentOperation) = parent.value,
            case .operation(let operation) = value
        {
            isLessPriority = operation.priority < parentOperation.priority
        }
        switch value {

        case .terminal(let terminal):
            return String(terminal.value)

        case .operation(let operation):
            switch operation {

            case .shuffle, .union, .concat:
                guard let left, let right else {
                    return ""
                }
                let leftRegex = left.toRegex()
                let rightRegex = right.toRegex()
                return isLessPriority ? "(\(leftRegex)\(operation.value)\(rightRegex))" : "\(leftRegex)\(operation.value)\(rightRegex)"

            case .iteration, .maybe:
                guard let left else {
                    return ""
                }
                let regex = left.toRegex()
                return "\(regex)\(operation.value)"
            }
        }
    }

    private func setup(as node: Node) {
        let newNode = node
        self.parent = newNode.parent
        self.left = newNode.left
        self.right = newNode.right
        self.value = newNode.value
    }

    private func addNode(node: Node) {
        let newNode = node
        newNode.parent = self
        if left != nil {
            self.right = newNode
        } else {
            self.left = newNode
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

    init(regex: Regex) throws {
        let symbols = regex.symbols
        self.terminals = regex.terminals
        self.root = try Self.setup(with: symbols)
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
        return root.hasEmptyWord()
    }

    static private func setup(with array: [Regex.Symbol]) throws -> Node {
        var symbolsStack = Stack<Regex.Symbol>()
        var nodesStack = Stack<Node>()

        let perform: () -> Void = {
            let poped = symbolsStack.pop()
            switch poped {
 
            case .operation(.iteration):
                let node = nodesStack.pop()
                let newNode = Node(value: .operation(.iteration))

                newNode.setNode(node: node)
                nodesStack.push(newNode)

            default:
                let rightNode = nodesStack.pop()
                let leftNode = nodesStack.pop()

                var newNode: Node
                if poped == .operation(.concat) {
                    newNode = Node(value: .operation(.concat))
                } else if poped == .operation(.union) {
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

            case .closeBracket:
                while let top = symbolsStack.top(), top != .openBracket {
                    perform()
                }
                symbolsStack.pop()

            case .terminal(.letter(let char)):
                let newNode = Node(value: .terminal(.letter(char)))
                nodesStack.push(newNode)

            case .operation(let operation):
                while case .operation(let existOperation) = symbolsStack.top(),
                        existOperation.priority >= operation.priority
                {
                    perform()
                }
                symbolsStack.push(sym)

            default:
                symbolsStack.push(sym)
            }
        }

        while symbolsStack.top() != nil {
            perform()
        }

        let root = nodesStack.pop()
        guard nodesStack.top() == nil else {
            throw CustomError.cannotParseRegex
        }
        return root
    }
}

// MARK: - FSM State

struct State {
    let id: Int
    let regex: Node
}

// MARK: - FSM Transition

struct Transition: Equatable {
    let from: State
    let to: State
    let by: Node
}

// MARK: - Automata

class FSM {
    var states = [State]()
    var terminals: [Regex.Terminal]
    var transitions = [Transition]()
    var initialState: State
    var finalStates = [State]()

    init(tree: Tree) throws {
        self.terminals = tree.terminals.sorted()
        self.initialState = .init(id: 0, regex: tree.root)
        self.states.append(initialState)
        if tree.root.hasEmptyWord() {
            finalStates.append(initialState)
        }
        try setup(by: tree, from: initialState, startTime: CFAbsoluteTimeGetCurrent())
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
            let by = transition.by.toRegex()
            print("q\(from) -- \(by) --> q\(to)")
        }
    }

    func toRegex() throws -> String {
        makeNewInitialState()
        try makeNewFinalState()
        eliminateStates()

        guard let union = transitions.union() else {
            throw CustomError.noTransitionsInFSM(self)
        }
        return union.by.toRegex()
    }

    private func setup(by tree: Tree, from state: State, startTime: CFAbsoluteTime) throws {
        let currentTime = CFAbsoluteTimeGetCurrent()
        if currentTime - startTime > 10.0 {
            throw CustomError.timeoutWhileMakingFSM
        }
        for terminal in terminals {
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
                try setup(by: derivative, from: newState, startTime: startTime)
            }
            transitions.append(newTransition)
        }
    }

    private func eliminateStates() {
        while states.count != 0 {
            let state = states.removeFirst()

            let iterationTransition = transitions.filter { $0.from == state && $0.from == $0.to }.union()
            let toStateTransitions = transitions.filter { $0.to == state && $0.from != state }.compactByFrom()
            let fromStateTransitions = transitions.filter { $0.from == state && $0.to != state }.compactByTo()

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

    private func makeNewInitialState() {
        let newInitialState = State(id: -1, regex: Node(value: .terminal(.emptySet)))
        let newTransition = Transition(from: newInitialState, to: initialState, by: Node(value: .terminal(.epsilon)))
        transitions.append(newTransition)
        initialState = newInitialState
    }

    private func makeNewFinalState() throws {
        guard finalStates.count > 0 else {
            throw CustomError.noFinalStatesInFSM(self)
        }
        let newFinalState = State(id: states.count, regex: Node(value: .terminal(.emptySet)))
        for finalState in finalStates {
            let newTransition = Transition(from: finalState, to: newFinalState, by: Node(value: .terminal(.epsilon)))
            transitions.append(newTransition)
        }
        finalStates = [newFinalState]
    }
}

// MARK: - Transition JSON Model

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

// MARK: - FSM JSON Model

struct FSMJSONModel: Encodable {
    let initialState: Int
    let states: [Int]
    let finalStates: [Int]
    let transitions: [TransitionJSONModel]
    let terminals: [String]

    private enum CodingKeys: String, CodingKey {
        case initialState = "initial_state"
        case states
        case finalStates = "final_states"
        case transitions
        case terminals
    }

    init(from fsm: FSM) {
        initialState = fsm.initialState.id
        states = fsm.states.map { $0.id }
        finalStates = fsm.finalStates.map { $0.id }
        transitions = fsm.transitions.map { .init(from: $0) }
        terminals = fsm.terminals.map { String($0.value) }
    }
}

// MARK: - Response JSON Model

struct ResponseJSON: Encodable {
    let input: String
    let output: String?
    let fsm: FSMJSONModel?
    let error: String?

    init(input: String, output: String? = nil, fsm: FSMJSONModel? = nil, error: String? = nil) {
        self.input = input
        self.output = output
        self.fsm = fsm
        self.error = error
    }
}

// MARK: - Help Functions

func readFromFile() -> [String] {
    let fileName = "../files/output.txt"
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

func and(_ components: Bool...) -> Bool {
    for component in components {
        if component == false {
            return false
        }
    }
    return true
}

func or(_ components: Bool...) -> Bool {
    for component in components {
        if component == true {
            return true
        }
    }
    return false
}

func getResponse(of reg: String) -> ResponseJSON {
    do {
        let regex = try Regex(string: reg)
        let tree = try Tree(regex: regex)
        let fsm = try FSM(tree: tree)
        let fsmJson = FSMJSONModel(from: fsm)
        let output = try fsm.toRegex()
        let response = ResponseJSON(input: reg, output: output, fsm: fsmJson)
        return response

    } catch {
        let response: ResponseJSON
        switch error as? CustomError {

        case .unexpectedSymbol(let char):
            response = .init(input: reg, error: "Unexpected symbol: \(char)")

        case .cannotParseRegex:
            response = .init(input: reg, error: "Can't parse regex")

        case .noFinalStatesInFSM(let fsm):
            response = .init(input: reg, fsm: .init(from: fsm), error: "No final states in FSM")

        case .noTransitionsInFSM(let fsm):
            response = .init(input: reg, fsm: .init(from: fsm), error: "No transitions in FSM")

        case .timeoutWhileMakingFSM:
            response = .init(input: reg, error: "Time out while making fsm from regex")

        default:
            response = .init(input: reg, error: error.localizedDescription)
        }
        return response
    }
}

// MARK: - MAIN

if CommandLine.arguments.count > 1 {
    let reg = CommandLine.arguments[1]
    let response = getResponse(of: reg)
    if let error = response.error {
        print(error)
    } else if let output = response.output {
        print(output)
    } else {
        print("something wrong")
    }
} else {
    let regs = readFromFile()
    var responses: [Int: ResponseJSON] = [:]
    for index in 0..<regs.count {
        let reg = regs[index]
        let response = getResponse(of: reg)
        responses[index] = response
    }

    let encoder = JSONEncoder()
    encoder.outputFormatting = .prettyPrinted
    let data = try encoder.encode(responses.map { $0.value })
    guard let string = String(data: data, encoding: .utf8) else {
        fatalError("error in encoding data")
    }

    let directoryPath = FileManager.default.currentDirectoryPath
    let resultPath = directoryPath.appending("/../files/result.json")
    FileManager.default.createFile(atPath: resultPath, contents: nil)
    try string.write(toFile: resultPath, atomically: true, encoding: .utf8)
}



