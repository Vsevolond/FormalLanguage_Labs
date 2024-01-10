import Foundation

// MARK: - JSON Response

struct JSONResponse: Encodable {
    
    private let resultID: Int
    private var position: Int? = nil
    private var nonTerms: [String]? = nil
    private var follow: [String: [String]]? = nil
    
    init(from result: FSMResult) {
        switch result {
            
        case .notLR0Grammar:
            self.resultID = 0
            
        case .notAccepted(let position, let nonTerms, let follow):
            self.resultID = 1
            self.position = position
            self.nonTerms = nonTerms.map { String($0.value) }
            self.follow = follow.reduce(into: [String: [String]]()) {
                $0.updateValue($1.value.map { String($0.value) }, forKey: String($1.key.value))
            }
            
        case .accepted:
            self.resultID = 2
        }
    }
}
