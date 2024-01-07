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
    static let grammarFileName = "/Users/vsevolond/UNIVERSITY/FormalLanguage_Labs/Labs/lab4/grammar.txt"
    static let wordFileName = "/Users/vsevolond/UNIVERSITY/FormalLanguage_Labs/Labs/lab4/word.txt"
    #else
    static let grammarFileName = "grammar.txt"
    static let wordFileName = "word.txt"
    #endif
}

// MARK: - MAIN

let grammarFile = readFromFile(fileName: Constants.grammarFileName)
let grammar = Grammar(from: grammarFile)
do {
    let fsm = try FSM(from: grammar)
    let word = readFromFile(fileName: Constants.wordFileName).reduce("", +)
    try fsm.analyse(word: word)
} catch {
    print(error)
}


