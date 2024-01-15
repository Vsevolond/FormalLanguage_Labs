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
    static let inputFileName = "/Users/vsevolond/UNIVERSITY/FormalLanguage_Labs/Labs/lab4/files/input.json"
    #else
    static let grammarFileName = "grammar.txt"
    static let wordFileName = "word.txt"
    static let inputFileName = FileManager.default.currentDirectoryPath.appending("/files/input.json")
    #endif
}

// MARK: - MAIN

let grammarRules = readFromFile(fileName: Constants.grammarFileName)
let word = readFromFile(fileName: Constants.wordFileName).reduce("", +)

let grammar = Grammar(from: grammarRules)
do {
    let fsm = try FSM(from: grammar)
    try fsm.analyse(word: word)
    
} catch {
    guard let result = error as? FSMResult else {
        fatalError("something wrong")
    }
    
    switch result {
        
    case .notLR0Grammar:
        print("Not LR(0) grammar")
        
    case .notAccepted(let i):
        let reversedGrammar = grammar.reversed()
        
        do {
            let fsm = try FSM(from: reversedGrammar)
            let reversedWord = String(word.reversed())
            try fsm.analyse(word: reversedWord)
            
        } catch {
            guard let result = error as? FSMResult else {
                fatalError("something wrong")
            }
            
            switch result {
                
            case .notLR0Grammar:
                print("Not accepted, error in position: \(i)")
                print("Reversed grammar is not LR(0) grammar")
                
            case .notAccepted(let j):
                if j >= word.count - i + 1 {
                    print("Not accepted, error in position: \(i)")
                } else {
                    let k = word.count - j + 1
                    print("Not accepted, errors in positions: \(i), \(k)")
                }
                
            case .accepted:
                print("Not accepted, error in position: \(i)")
                print("Reversed word doesn't have errors")
            }
        }
        
    case .accepted:
        print("Accepted")
    }
}


