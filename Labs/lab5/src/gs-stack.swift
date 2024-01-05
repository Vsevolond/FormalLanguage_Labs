import Foundation

// MARK: - GSStack

class GSStack {
    
    private var nodesById: [Int: GSSNode] = [:]
    private var edges: Set<GSSEdge> = .init()
    
    var activeNodes: [GSSNode] {
        nodesById.values.filter { $0.isActive }
    }
    
    func push(node: GSSNode) {
        nodesById[node.id] = node
    }
    
    func push(to node: GSSNode, newNode: GSSNode) {
        nodesById[node.id]?.isActive = false
        
        if let existNode = nodesById.values.first(where: { $0 == node }) {
            edges.insert(.init(from: existNode.id, to: node.id))
        } else {
            nodesById[node.id] = node
            edges.insert(.init(from: node.id, to: node.id))
        }
    }
    
    func pop(from node: GSSNode, tokens: [GrammarSymbol], withRemovingTop: Bool) -> [GSSNode] {
        let paths = findPath(from: node, by: tokens)
        guard paths.count > 0 else {
            fatalError("there is no such tokens in stack")
        }
        
        
    }
    
    private func findPath(from node: GSSNode, by tokens: [GrammarSymbol]) -> [[GSSNode]] {
        guard let token = tokens.first else {
            return [[node]]
        }
        
        guard node.token == token else {
            return [[]]
        }
        
        let nextNodes = edges.filter { $0.from == node.id }.compactMap { nodesById[$0.to] }
        let paths: [[GSSNode]] = nextNodes
                                    .flatMap { findPath(from: $0, by: tokens.withRemovingFirst()) }
                                    .filter { $0.count == tokens.count }
                                    .map { $0.withInserting(node, at: 0) }
        return paths
    }
}
