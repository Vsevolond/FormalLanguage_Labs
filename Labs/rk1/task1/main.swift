import Foundation

extension Array {
    func appending(_ items: Element...) -> Array<Element> {
        var newArray = self
        for item in items {
            newArray.append(item)
        }
        return newArray
    }
}

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

enum Operation: CaseIterable {
    case plus
    case multiply
    
    var priority: Int {
        switch self {
        case .plus: return 0
        case .multiply: return 1
        }
    }
    
    static func random() -> Operation {
        let random = Int.random(in: 0...1)
        if random == 0 {
            return .plus
        } else {
            return .multiply
        }
    }
    
    var value: Character {
        switch self {
        case .plus:
            return "+"
        case .multiply:
            return "*"
        }
    }
}

enum Number: CaseIterable {
    case zero
    case one
    case two
    
    var value: Int {
        switch self {
        case .zero: return 0
        case .one: return 1
        case .two: return 2
        }
    }
    
    static func random() -> Number {
        let random = Int.random(in: 0...2)
        if random == 0 {
            return .zero
        } else if random == 1 {
            return .one
        } else {
            return .two
        }
    }
}

enum Item: Hashable {
    case number(Number)
    case operation(Operation)
    
    var value: String {
        switch self {
        case .number(let number):
            return String(number.value)
        case .operation(let operation):
            return String(operation.value)
        }
    }
}

enum ItemType {
    case number
    case operation
}

class Expression {
    
    var items: [Item]
    
    init(items: [Item]) {
        self.items = items
    }
    
    func calculate() -> Int {
        var operationStack = Stack<Operation>()
        var numberStack = Stack<Int>()
        
        let perform: () -> Void = {
            let poped = operationStack.pop()
            let number1 = numberStack.pop()
            let number2 = numberStack.pop()
            
            switch poped {
            case .plus:
                numberStack.push(number1 + number2)
            case .multiply:
                numberStack.push(number1 * number2)
            }
        }
        
        for item in items {
            switch item {
            case .number(let number):
                numberStack.push(number.value)
            case .operation(let operation):
                while let topOperation = operationStack.top(), operation.priority <= topOperation.priority {
                    perform()
                }
                operationStack.push(operation)
            }
        }
        
        while operationStack.top() != nil {
            perform()
        }
        
        let result = numberStack.pop()
        guard numberStack.top() == nil else {
            fatalError("stack is not empty")
        }
        return result
    }
    
    func string() -> String {
        let result = items.reduce(into: "") { partialResult, item in
            switch item {
            case .number(let number):
                partialResult += String(number.value)
            case .operation(let operation):
                switch operation {
                case .plus:
                    partialResult += " + "
                case .multiply:
                    partialResult += " * "
                }
            }
        }
        
        return result
    }
    
    func isAccept() -> Bool {
        return calculate() == 1
    }
}

func generateExpressions(from items: [Item], operands limit: Int) -> [Expression] {
    guard items.count < limit * 2 - 1 else {
        return []
    }
    
    var result = [Expression]()
    
    if items.count == 0 {
        for number in Number.allCases {
            
            let expression = Expression(items: [.number(number)])
            result.append(expression)
            result.append(contentsOf: generateExpressions(from: expression.items, operands: limit))
        }
    } else {
        for operation in Operation.allCases {
            for number in Number.allCases {
                
                let expression = Expression(items: items.appending(.operation(operation), .number(number)))
                result.append(expression)
                result.append(contentsOf: generateExpressions(from: expression.items, operands: limit))
            }
        }
    }
    
    return result
}

struct State {
    let id: String
    let isFinal: Bool
    let isInitial: Bool
    let transitions: [Item: String]
    
    init(id: String, isFinal: Bool = false, isInitial: Bool = false, transitions: [Item : String]) {
        self.id = id
        self.isFinal = isFinal
        self.isInitial = isInitial
        self.transitions = transitions
    }
    
    func next(by item: Item) -> String? {
        return transitions[item]
    }
}

class FSM {
    var states = [String: State]()
    var initialState: State?
    
    init(states: [State]) {
        for state in states {
            self.states[state.id] = state
            if state.isInitial {
                self.initialState = state
            }
        }
    }
    
    func accept(expression: Expression) -> Bool {
        guard let initialState else {
            fatalError("no initial state")
        }

        var currentState = initialState
        
        for item in expression.items {
            guard let id = currentState.next(by: item) else {
                return false
            }
            
            guard let nextState = states[id] else {
                fatalError("there is no state like: \(id)")
            }
            
            currentState = nextState
        }
        
        return currentState.isFinal
    }
    
    func printTransitions() {
        states.forEach { _, state in
            state.transitions.forEach { item, id in
                print("\"\(state.id)\" -> \"\(id)\" [label = \"\(item.value)\" ]")
            }
        }
    }
}

let states: [State] = [
    .init(id: "e", isInitial: true, transitions: [
        .number(.zero) : "0",
        .number(.one) : "1",
        .number(.two) : "2"
    ]),
    .init(id: "0", transitions: [
        .operation(.plus) : "0+",
        .operation(.multiply): "0*"
    ]),
    .init(id: "0+", transitions: [
        .number(.zero): "0",
        .number(.one): "1",
        .number(.two): "2"
    ]),
    .init(id: "0*", transitions: [
        .number(.zero): "0",
        .number(.one): "0",
        .number(.two): "0"
    ]),
    .init(id: "2", transitions: [
        .operation(.multiply): "2*"
    ]),
    .init(id: "2*", transitions: [
        .number(.one): "2",
        .number(.two): "2",
        .number(.zero): "0"
    ]),
    .init(id: "1", isFinal: true, transitions: [
        .operation(.plus): "1+",
        .operation(.multiply): "1*"
    ]),
    .init(id: "1*", transitions: [
        .number(.one): "1",
        .number(.zero): "0",
        .number(.two): "2"
    ]),
    .init(id: "1+", transitions: [
        .number(.zero): "1+0",
        .number(.one): "1+1",
        .number(.two): "1+2"
    ]),
    .init(id: "1+0", isFinal: true, transitions: [
        .operation(.plus): "1+0+",
        .operation(.multiply): "1+0*"
    ]),
    .init(id: "1+0+", transitions: [
        .number(.zero): "1+0",
        .number(.one): "1+1",
        .number(.two): "1+2"
    ]),
    .init(id: "1+0*", transitions: [
        .number(.zero): "1+0",
        .number(.one): "1+0",
        .number(.two): "1+0"
    ]),
    .init(id: "1+1", transitions: [
        .operation(.multiply): "1+1*"
    ]),
    .init(id: "1+1*", transitions: [
        .number(.one): "1+1",
        .number(.zero): "1+0",
        .number(.two): "1+2"
    ]),
    .init(id: "1+2", transitions: [
        .operation(.multiply): "1+2*"
    ]),
    .init(id: "1+2*", transitions: [
        .number(.zero): "1+0",
        .number(.one): "1+2",
        .number(.two): "1+2"
    ])
]

let fsm = FSM(states: states)
let expressions = generateExpressions(from: [], operands: 8)
fsm.printTransitions()

DispatchQueue.concurrentPerform(iterations: expressions.count) { index in
    let expression = expressions[index]
    let isAccept: Bool = expression.calculate() == 1
    let isAcceptByFSM: Bool = fsm.accept(expression: expression)
    
    if isAccept != isAcceptByFSM {
        print("Expression: \(expression.string())")
        print("Must be accepted: \(isAccept)")
        print("Accepted by FSM: \(isAcceptByFSM)")
        print("Result: FAIL")
        print()
    }
}
