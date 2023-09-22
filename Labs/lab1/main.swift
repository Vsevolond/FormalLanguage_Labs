import Foundation

// MARK: - Custom Types

enum OperationType: String {
    case sum = "arcmax"
    case mul = "arcsum"
    case more = "arcgg"
}

struct Operation {
    var operand1: String
    var operand2: String
    var action: OperationType
    
    func string() -> String {
        return "(\(action.rawValue) \(operand1) \(operand2))"
    }
}

struct Matrix {
    var m00: String = ""
    var m01: String = ""
    var m10: String = ""
    var m11: String = ""
    
    init() {}
    
    init(name: String) {
        m00 = name.appending("00")
        m01 = name.appending("01")
        m10 = name.appending("10")
        m11 = name.appending("11")
    }
    
    func asserts() -> [String] {
        return [
            "(assert (> \(m00) -1))",
            "(assert (>= \(m01) -1))",
            "(assert (>= \(m10) -1))",
            "(assert (>= \(m11) -1))"
        ]
    }
}

struct Vector {
    var v0: String = ""
    var v1: String = ""
    
    init() {}
    
    init(name: String) {
        v0 = name.appending("0")
        v1 = name.appending("1")
    }
    
    func asserts() -> [String] {
        return [
            "(assert (or (> \(v0) -1) (and (= 0 \(v0)) (= 0 \(v1)))))",
            "(assert (>= \(v1) -1))"
        ]
    }
}

struct Function {
    var name: String
    var matrix: Matrix
    var vector: Vector
    
    init(name: String) {
        self.name = name
        matrix = .init(name: name.appending("0"))
        vector = .init(name: name.appending("1"))
    }
    
    func declares() -> [String] {
        return [
            "(declare-fun \(matrix.m00) () Int)",
            "(declare-fun \(matrix.m01) () Int)",
            "(declare-fun \(matrix.m10) () Int)",
            "(declare-fun \(matrix.m11) () Int)",
            "(declare-fun \(vector.v0) () Int)",
            "(declare-fun \(vector.v1) () Int)"
        ]
    }
    
    func asserts() -> [String] {
        var asserts = [String]()
        asserts.append(contentsOf: matrix.asserts())
        asserts.append(contentsOf: vector.asserts())
        return asserts
    }
}

// MARK: - Help Functions

func readFromFile(path: String) -> [String] {
    guard let text = try? String(contentsOfFile: path) else {
        return []
    }
    let split = text.split(separator: "\n").map { String($0) }
    
    return split
}

func arcmulMatrixs(matrix1: Matrix, matrix2: Matrix) -> Matrix {
    var result = Matrix()
    result.m00 = Operation(
        operand1: Operation(operand1: matrix1.m00, operand2: matrix2.m00, action: .mul).string(),
        operand2: Operation(operand1: matrix1.m01, operand2: matrix2.m10, action: .mul).string(),
        action: .sum
    ).string()
    result.m01 = Operation(
        operand1: Operation(operand1: matrix1.m00, operand2: matrix2.m01, action: .mul).string(),
        operand2: Operation(operand1: matrix1.m01, operand2: matrix2.m11, action: .mul).string(),
        action: .sum
    ).string()
    result.m10 = Operation(
        operand1: Operation(operand1: matrix1.m10, operand2: matrix2.m00, action: .mul).string(),
        operand2: Operation(operand1: matrix1.m11, operand2: matrix2.m10, action: .mul).string(),
        action: .sum
    ).string()
    result.m11 = Operation(
        operand1: Operation(operand1: matrix1.m10, operand2: matrix2.m01, action: .mul).string(),
        operand2: Operation(operand1: matrix1.m11, operand2: matrix2.m11, action: .mul).string(),
        action: .sum
    ).string()
    
    return result
}

func arcmulMatrixVector(matrix: Matrix, vector: Vector) -> Vector {
    var result = Vector()
    result.v0 = Operation(
        operand1: Operation(operand1: matrix.m00, operand2: vector.v0, action: .mul).string(),
        operand2: Operation(operand1: matrix.m01, operand2: vector.v1, action: .mul).string(),
        action: .sum
    ).string()
    result.v1 = Operation(
        operand1: Operation(operand1: matrix.m10, operand2: vector.v0, action: .mul).string(),
        operand2: Operation(operand1: matrix.m11, operand2: vector.v1, action: .mul).string(),
        action: .sum
    ).string()
    
    return result
}

func arcsumVectors(vector1: Vector, vector2: Vector) -> Vector {
    var result = Vector()
    result.v0 = Operation(operand1: vector1.v0, operand2: vector2.v0, action: .sum).string()
    result.v1 = Operation(operand1: vector1.v1, operand2: vector2.v1, action: .sum).string()
    
    return result
}

func interpolation(of functions: [String]) -> Function {
    let names = functions.reversed().map { String($0) }
    var result = Function(name: names[0])
    
    for i in 1..<names.count {
        let function = Function(name: names[i])
        result = interpolation(function1: result, function2: function)
    }
    
    return result
}

func interpolation(function1: Function, function2: Function) -> Function {
    var result = Function(name: function1.name + function2.name)
    result.matrix = arcmulMatrixs(matrix1: function1.matrix, matrix2: function2.matrix)
    result.vector = arcsumVectors(
        vector1: arcmulMatrixVector(matrix: function1.matrix, vector: function2.vector),
        vector2: function1.vector
    )
    
    return result
}

// MARK: - MAIN

let start = """
(set-logic QF_NIA)

(define-fun arcmax ((a Int) (b Int)) Int
(ite (>= a b) a b))

(define-fun arcsum ((a Int) (b Int)) Int
(ite (and (> a -1) (> b -1)) (+ a b) (ite (<= a -1) b a)))

(define-fun arcgg ((a Int) (b Int)) Bool
(ite (or (> a b) (and (<= a -1) (<= b -1))) true false))

"""

let end = """
(check-sat)
(get-model)
(exit)
"""

var declares = [String]()
var asserts = [String]()

let directoryPath = FileManager.default.currentDirectoryPath
let inputPath = directoryPath.appending("/input.txt")

var functions = Set<String>()
let expressions = readFromFile(path: inputPath)
for expression in expressions {
    let split = expression.split(separator: "=").map { String($0) }
    let lhs = split[0].components(separatedBy: CharacterSet(charactersIn: "()")).filter { !$0.isEmpty && $0 != "x" }
    let rhs = split[1].components(separatedBy: CharacterSet(charactersIn: "()")).filter { !$0.isEmpty && $0 != "x" }
    
    for name in lhs {
        if !functions.contains(name) {
            functions.insert(name)
            let function = Function(name: name)
            declares.append(contentsOf: function.declares())
            asserts.append(contentsOf: function.asserts())
        }
    }
    for name in rhs {
        if !functions.contains(name) {
            functions.insert(name)
            let function = Function(name: name)
            declares.append(contentsOf: function.declares())
            asserts.append(contentsOf: function.asserts())
        }
    }
    
    let lhsInterpolation = interpolation(of: lhs)
    let rhsInterpolation = interpolation(of: rhs)
    
    let op1 = Operation(operand1: lhsInterpolation.matrix.m00, operand2: rhsInterpolation.matrix.m00, action: .more).string()
    let op2 = Operation(operand1: lhsInterpolation.matrix.m01, operand2: rhsInterpolation.matrix.m01, action: .more).string()
    let op3 = Operation(operand1: lhsInterpolation.matrix.m10, operand2: rhsInterpolation.matrix.m10, action: .more).string()
    let op4 = Operation(operand1: lhsInterpolation.matrix.m11, operand2: rhsInterpolation.matrix.m11, action: .more).string()
    let op5 = Operation(operand1: lhsInterpolation.vector.v0, operand2: rhsInterpolation.vector.v0, action: .more).string()
    let op6 = Operation(operand1: lhsInterpolation.vector.v1, operand2: rhsInterpolation.vector.v1, action: .more).string()
    
    asserts.append(contentsOf: [
        "(assert (and \(op1) \(op2) \(op3) \(op4)))",
        "(assert (and \(op5) \(op6)))"
    ])
}

var body = ""
declares.forEach { declare in
    body.append(declare + "\n")
}
asserts.forEach { assert in
    body.append(assert + "\n")
}

let smtPath = directoryPath.appending("/lab1.smt2")
FileManager.default.createFile(atPath: smtPath, contents: nil)
let content = start + "\n" + body + "\n" +  end
try content.write(toFile: smtPath, atomically: true, encoding: .utf8)
