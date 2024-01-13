import Foundation

// MARK: - GSStack

class GSStack {
    
    private var nodesById: [Int: GSSNode] = [:]
    private var edges: Set<GSSEdge> = .init()
    
    var snapshot: [String] {
        var stringEdges: [String] = [Constants.snapshotBegin]
        
        edges.forEach { edge in
            if let from = nodesById[edge.from], let to = nodesById[edge.to] {
                stringEdges.append("\(from.token.value)\(from.id) -> \(to.token.value)\(to.id)")
            }
        }
        
        stringEdges.append(Constants.snapshotEnd)
        return stringEdges
    }
    
    var activeNodes: [GSSNode] {
        nodesById.values.filter { $0.isActive }
    }
    
    func push(node: GSSNode) {
        nodesById[node.id] = node
    }
    
    @discardableResult
    func push(to node: GSSNode, newNode: GSSNode) -> GSSNode {
        nodesById[node.id]?.isActive = false
        
        if let existNode = nodesById.values.first(where: { $0 == newNode }) {
            edges.insert(.init(from: existNode.id, to: node.id))
            return existNode
        } else {
            nodesById[newNode.id] = newNode
            edges.insert(.init(from: newNode.id, to: node.id))
            return newNode
        }
    }
    
    func pop(from node: GSSNode, tokens: [GrammarSymbol], withRemovingTop: Bool) -> Set<GSSNode> {
        nodesById[node.id]?.isActive = false
        var path = findPath(from: node, by: tokens)
        
        guard !path.isEmpty else {
            fatalError("there is no such tokens in stack")
        }
        
        if withRemovingTop {
            
            path.pairForEach { currentNodes, nextNodes in
                let outcomingEdges: [GSSEdge] = currentNodes.filter { currentNode in
                    let incoming = edges.filter { $0.to == currentNode.id }
                    return incoming.count == 0
                }.combinePairs(with: nextNodes).map { .init(from: $0.id, to: $1.id) }
                
                edges = edges.filter { !outcomingEdges.contains($0) }
                currentNodes.forEach { currentNode in
                    popIfPossible(node: currentNode)
                }
            }
        }
        
        return path.removeLast()
    }
    
    func popBranch(from node: GSSNode) {
        nodesById[node.id]?.isActive = false
        
        let nextNodes = pop(node: node)
        nextNodes.forEach { node in
            popBranch(from: node)
        }
    }
    
    private func pop(node: GSSNode) -> [GSSNode] {
        let incoming = edges.filter { $0.to == node.id }
        guard incoming.count == 0 else {
            return []
        }
        
        let outcoming = edges.filter { $0.from == node.id }.compactMap { nodesById[$0.to] }
        edges.removeAll { $0.from == node.id }
        nodesById.removeValue(forKey: node.id)

        return outcoming
    }
    
    private func popIfPossible(node: GSSNode) {
        let incoming = edges.filter { $0.to == node.id }
        let outcoming = edges.filter { $0.from == node.id }
        
        guard incoming.count == 0, outcoming.count == 0 else {
            return
        }
        
        nodesById.removeValue(forKey: node.id)
    }
    
    private func findPath(from node: GSSNode, by tokens: [GrammarSymbol]) -> [Set<GSSNode>] {
        var nodesByIndex: [Int: Set<GSSNode>] = (0...tokens.count).reduce(into: [Int: Set<GSSNode>]()) { $0.updateValue(.init(), forKey: $1)}
        nodesByIndex[0]?.insert(node)
        
        for i in 1...tokens.count {
            guard let prevNodes = nodesByIndex[i - 1], !prevNodes.isEmpty else {
                return []
            }
            
            prevNodes.forEach { prevNode in
                let nextNodes: [GSSNode]
                if i == tokens.count {
                    nextNodes = edges.filter { $0.from == prevNode.id }.compactMap { nodesById[$0.to] }
                } else {
                    nextNodes = edges.filter { $0.from == prevNode.id }.compactMap { nodesById[$0.to] }.filter { $0.token == tokens[i] }
                }

                nodesByIndex[i]?.formUnion(nextNodes)
            }
        }
        
        return nodesByIndex.sorted(by: { $0.key < $1.key }).map { $1 }
    }
}

// MARK: - Constants

private enum Constants {
    
    static let snapshotBegin: String = "digraph {"
    static let snapshotEnd: String = "}"
}
