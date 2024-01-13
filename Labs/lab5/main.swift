import Foundation

// MARK: - Help Functions

func readFromFile(fileName: String) -> [String] {
    #if DEBUG
    let currentDirectory = ""
    #else
    let currentDirectory = FileManager.default.currentDirectoryPath
    #endif
    guard let text = try? String(contentsOfFile: currentDirectory + "/\(fileName)") else {
        return []
    }

    let lines = text.split(separator: "\n").map { String($0).removingSpaces() }
    return lines
}

// MARK: - Constants

private enum Constants {

    #if DEBUG
    static let grammarFileName = "/Users/vsevolond/UNIVERSITY/FormalLanguage_Labs/Labs/lab5/grammar.txt"
    #else
    static let grammarFileName = "grammar.txt"
    #endif
}

// MARK: - MAIN

let grammarFile = readFromFile(fileName: Constants.grammarFileName)
let grammar = Grammar(from: grammarFile)
let fsm = FSM(from: grammar)
#if DEBUG
let word = readLine()!
let numStateOfStack = Int(readLine()!)!
#else
let word = CommandLine.arguments[1]
let numStateOfStack = Int(CommandLine.arguments[2])!
#endif

let (isBelong, index, snapshot) = fsm.analyse(word: word, numStateOfStack: numStateOfStack)
if isBelong {
    print("accepted")
    snapshot.forEach { string in
        print(string)
    }
    
} else {
    print("not accepted, error in position: \(index)")
    snapshot.forEach { string in
        print(string)
    }
}


