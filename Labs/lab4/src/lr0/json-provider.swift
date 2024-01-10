import Foundation

// MARK: - JSON Provider

final class JSONProvider {
    
    private let result: FSMResult
    
    init(result: FSMResult) {
        self.result = result
    }
    
    func write(to fileName: String) throws {
        let response = JSONResponse(from: result)
        
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .withoutEscapingSlashes]
        
        let data = try encoder.encode(response)
        guard let string = String(data: data, encoding: .utf8) else {
            fatalError("error in encoding data")
        }
        
        FileManager.default.createFile(atPath: fileName, contents: nil)
        try string.write(toFile: fileName, atomically: true, encoding: .utf8)
    }
}
