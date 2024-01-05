import Foundation

// MARK: - GSSNode

struct GSSNode {
    
    private static var ID: Int = 0
    let id: Int
    
    let state: Int
    let token: GrammarSymbol
    let point: Int
    var isActive: Bool
    
    init(state: Int, token: GrammarSymbol, point: Int, isActive: Bool = true) {
        self.id = GSSNode.ID
        GSSNode.ID += 1

        self.state = state
        self.token = token
        self.point = point
        self.isActive = isActive
    }
}

extension GSSNode: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(state)
        hasher.combine(token)
        hasher.combine(point)
    }
    
    static func ==(lhs: GSSNode, rhs: GSSNode) -> Bool {
        return lhs.state == rhs.state && lhs.token == rhs.token && lhs.point == rhs.point
    }
}
