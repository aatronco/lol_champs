/**
 * community.js — wraps jLouvain for client-side community detection
 *
 * Exports (global):
 *   computeCommunities(nodes, links) → Map<nodeId, communityId>
 */

function computeCommunities(nodes, links) {
  if (!nodes || nodes.length === 0) return new Map();

  // jLouvain expects: node_id array + edge list [{source, target, weight?}]
  const nodeIds = nodes.map(n => n.id);

  const edges = links.map(l => ({
    source: typeof l.source === 'object' ? l.source.id : l.source,
    target: typeof l.target === 'object' ? l.target.id : l.target,
    weight: 1,
  }));

  try {
    const community = jLouvain().nodes(nodeIds).edges(edges);
    const result = community();  // returns {nodeId: communityId, ...}
    return new Map(Object.entries(result));
  } catch (e) {
    console.warn('jLouvain community detection failed:', e);
    return new Map();
  }
}
