import Foundation

extension Int {
    func fifth() -> [Int] {
        if self == 0 {
            return [0]
        }
        var result: [Int] = []
        var number = self
        while number != 0 {
            result.append(number % 5)
            number /= 5
        }
        return result
    }
}

enum Side: String, CaseIterable {
    case up = "N"
    case down = "S"
    case left = "W"
    case right = "E"
    
    func opposite() -> Side {
        switch self {
        case .up:
            return .down
        case .down:
            return .up
        case .left:
            return .right
        case .right:
            return .left
        }
    }
}

struct Transition {
    let from: Int
    let to: Int
    let by: Side
    
    func opposite() -> Transition {
        let oppositeSide = by.opposite()
        let transition = Transition(from: to, to: from, by: oppositeSide)
        return transition
    }
    
    func others() -> [Transition] {
        switch by {
        case .left, .right:
            return [.init(from: from, to: from, by: .up), .init(from: from, to: from, by: .down)]
        case .up, .down:
            return [.init(from: from, to: from, by: .left), .init(from: from, to: from, by: .right)]
        }
    }
}

class FSM {
    var states: [Int] = (1...21).map { $0 }
    var terminals: [Character] = ["N", "N", "S", "E", "W"]
    var transitions: [Transition] = [
        .init(from: 1, to: 2, by: .right),
        .init(from: 2, to: 3, by: .up),
        .init(from: 3, to: 4, by: .right),
        .init(from: 3, to: 5, by: .left),
        .init(from: 5, to: 6, by: .up),
        .init(from: 6, to: 7, by: .left),
        .init(from: 7, to: 8, by: .left),
        .init(from: 7, to: 10, by: .down),
        .init(from: 8, to: 9, by: .down),
        .init(from: 9, to: 10, by: .right),
        .init(from: 10, to: 11, by: .down),
        .init(from: 11, to: 12, by: .left),
        .init(from: 12, to: 13, by: .down),
        .init(from: 13, to: 14, by: .right),
        .init(from: 14, to: 15, by: .up),
        .init(from: 15, to: 16, by: .right),
        .init(from: 16, to: 17, by: .right),
        .init(from: 16, to: 19, by: .down),
        .init(from: 17, to: 18, by: .down),
        .init(from: 18, to: 19, by: .left),
        .init(from: 19, to: 20, by: .down),
        .init(from: 20, to: 21, by: .right)
    ]
    var finalState: Int = 15
    
    func synchronizeString() -> String {
        var transitionsDict: [Int: [String: Int]] = [:]
        for transition in transitions {
            let opposite = transition.opposite()
            transitions.append(opposite)
        }
        for state in states {
            let existTransitions = transitions.filter { $0.from == state }
            let byTerminals = existTransitions.map { $0.by }
            let missTerminals = Side.allCases.filter { !byTerminals.contains($0) }
            for terminal in missTerminals {
                transitions.append(.init(from: state, to: state, by: terminal))
            }
        }
        for transition in transitions {
            if transitionsDict[transition.from] != nil {
                transitionsDict[transition.from]?.updateValue(transition.to, forKey: transition.by.rawValue)
            } else {
                transitionsDict[transition.from] = [transition.by.rawValue : transition.to]
            }
        }
        var k = 0
        while true {
            let string: String = String(k.fifth().map { index in
                terminals[index]
            })
            let transitionsArray = states.map { from in
                getFiniteState(from: from, by: string, dict: &transitionsDict)
            }
            if Set(transitionsArray).count == 1 {
                return string
            } else {
                k += 1
            }
        }
    }
    
    func getFiniteState(from: Int, by: String, dict: inout [Int: [String: Int]]) -> Int {
        if let transitions = dict[from], let finiteState = transitions[by] {
            return finiteState
        }
        var pastString = by
        let newString = String(pastString.removeLast())
        guard
            let transitions = dict[from],
            let pastState = transitions[pastString],
            let transitionsPast = dict[pastState],
            let finiteState = transitionsPast[newString]
        else {
            fatalError("some error")
        }
        dict[from]?.updateValue(finiteState, forKey: by)
        return finiteState
    }
}

let fsm = FSM()
print(fsm.synchronizeString())
