/**
 * graph.js — D3.js force-directed graph with community visualization
 *
 * Entry point: called on DOMContentLoaded
 * Dependencies (globals): d3, computeCommunities, SidebarController
 */

const REGION_COLORS = {
  'Freljord':     '#3b82f6', 'Noxus':        '#ef4444',
  'Demacia':      '#f8fafc', 'Ionia':        '#22c55e',
  'Piltover':     '#f59e0b', 'Zaun':         '#84cc16',
  'Bilgewater':   '#0ea5e9', 'Shadow Isles': '#7c3aed',
  'Shurima':      '#d97706', 'Mt. Targon':   '#e879f9',
  'Void':         '#6366f1', 'Bandle City':  '#fb923c',
  'Ixtal':        '#10b981', 'Independent':  '#6b7280',
  'Unknown':      '#374151',
};

const EDGE_COLORS = {
  'Friendly':     '#22c55e',
  'Antagonistic': '#ef4444',
  'Romantic':     '#ec4899',
  'Related':      '#6b7280',
  'Other':        '#e5e7eb',
};

const COMMUNITY_COLORS = [
  '#60a5fa','#f87171','#34d399','#fbbf24','#a78bfa',
  '#f472b6','#38bdf8','#fb923c','#4ade80','#e879f9',
];

function nodeRadius(d) {
  return Math.min(20, Math.max(4, 4 + (d.degree || 0) * 0.8));
}

function communityColor(id) {
  return COMMUNITY_COLORS[id % COMMUNITY_COLORS.length];
}

function showError(msg) {
  const el = document.getElementById('error-message');
  el.textContent = msg;
  el.style.display = 'block';
}

async function init() {
  let nodes, links;

  try {
    [nodes, links] = await Promise.all([
      fetch('data/nodes.json').then(r => { if (!r.ok) throw new Error(); return r.json(); }),
      fetch('data/links.json').then(r => { if (!r.ok) throw new Error(); return r.json(); }),
    ]);
  } catch {
    showError('Failed to load data. Please check the repository.');
    return;
  }

  if (!nodes.length) { showError('No champion data found.'); return; }

  // Filter orphan links
  const nodeIds = new Set(nodes.map(n => n.id));
  const validLinks = links.filter(l => {
    const s = typeof l.source === 'object' ? l.source.id : l.source;
    const t = typeof l.target === 'object' ? l.target.id : l.target;
    const ok = nodeIds.has(s) && nodeIds.has(t);
    if (!ok) console.warn(`Orphan link filtered: ${s} → ${t}`);
    return ok;
  });

  if (!validLinks.length) {
    document.querySelector('#communities-section .section-label').after(
      Object.assign(document.createElement('p'), { textContent: 'No relationship data found.' })
    );
  }

  // Compute degree
  const degreeMap = new Map(nodes.map(n => [n.id, 0]));
  validLinks.forEach(l => {
    degreeMap.set(l.source, (degreeMap.get(l.source) || 0) + 1);
    degreeMap.set(l.target, (degreeMap.get(l.target) || 0) + 1);
  });
  nodes.forEach(n => n.degree = degreeMap.get(n.id) || 0);

  // Compute communities
  const communities = computeCommunities(nodes, validLinks);

  // D3 setup
  const container = document.getElementById('graph-container');
  const width = container.clientWidth;
  const height = container.clientHeight;

  const svg = d3.select('#graph')
    .attr('width', width)
    .attr('height', height);

  const g = svg.append('g');

  // Zoom
  svg.call(d3.zoom()
    .scaleExtent([0.1, 4])
    .on('zoom', e => g.attr('transform', e.transform))
  );

  // Force simulation
  const simulation = d3.forceSimulation(nodes)
    .force('link', d3.forceLink(validLinks).id(d => d.id).distance(60).strength(0.5))
    .force('charge', d3.forceManyBody().strength(-200))
    .force('collide', d3.forceCollide().radius(d => nodeRadius(d) + 4))
    .force('center', d3.forceCenter(width / 2, height / 2))
    .alphaDecay(0.03);

  // Edges
  const link = g.append('g').attr('class', 'links')
    .selectAll('line')
    .data(validLinks)
    .join('line')
    .attr('stroke', d => EDGE_COLORS[d.type] || '#e5e7eb')
    .attr('stroke-width', 1.5)
    .attr('stroke-opacity', 0.6);

  // Nodes
  const node = g.append('g').attr('class', 'nodes')
    .selectAll('circle')
    .data(nodes)
    .join('circle')
    .attr('r', nodeRadius)
    .attr('fill', d => REGION_COLORS[d.region] || '#374151')
    .attr('stroke', d => communityColor(communities.get(d.id) || 0))
    .attr('stroke-width', 3)
    .call(drag(simulation));

  // Tooltip
  const tooltip = document.getElementById('tooltip');
  const neighborMap = new Map(nodes.map(n => [n.id, []]));
  validLinks.forEach(l => {
    const s = typeof l.source === 'object' ? l.source.id : l.source;
    const t = typeof l.target === 'object' ? l.target.id : l.target;
    neighborMap.get(s)?.push(t);
    neighborMap.get(t)?.push(s);
  });

  node
    .on('mouseover', (event, d) => {
      const cid = communities.get(d.id);
      const neighbors = neighborMap.get(d.id) || [];
      tooltip.innerHTML = `
        <div class="tt-name">${d.id}</div>
        <div class="tt-row">Region: ${d.region}</div>
        <div class="tt-row">Community: ${cid !== undefined ? cid : '—'}</div>
        <div class="tt-neighbors">${neighbors.join(', ') || 'No connections'}</div>
      `;
      tooltip.style.display = 'block';
    })
    .on('mousemove', event => {
      tooltip.style.left = (event.clientX + 12) + 'px';
      tooltip.style.top  = (event.clientY - 10) + 'px';
    })
    .on('mouseout', () => { tooltip.style.display = 'none'; });

  // Simulation tick
  simulation.on('tick', () => {
    link
      .attr('x1', d => d.source.x).attr('y1', d => d.source.y)
      .attr('x2', d => d.target.x).attr('y2', d => d.target.y);
    node
      .attr('cx', d => d.x)
      .attr('cy', d => d.y);
  });

  // --- Sidebar ---
  let activeTypes = new Set(['Friendly','Antagonistic','Romantic','Related','Other']);
  let selectedCommunity = null;
  let searchQuery = '';

  SidebarController({
    communities,
    nodes,
    onFilterChange(types) {
      activeTypes = types;
      applyVisibility();
    },
    onCommunitySelect(cid) {
      selectedCommunity = cid;
      applyVisibility();
    },
    onSearch(query) {
      searchQuery = query;
      applyVisibility();
      if (query) centerOnMatches();
    },
  });

  function applyVisibility() {
    node.attr('opacity', d => {
      const matchesSearch = !searchQuery || d.id.toLowerCase().includes(searchQuery);
      const matchesCommunity = selectedCommunity === null || communities.get(d.id) === selectedCommunity;
      return (matchesSearch && matchesCommunity) ? 1 : 0.1;
    });

    link
      .style('display', d => activeTypes.has(d.type) ? null : 'none')
      .attr('stroke-opacity', d => {
        const s = typeof d.source === 'object' ? d.source.id : d.source;
        const t = typeof d.target === 'object' ? d.target.id : d.target;
        const sc = communities.get(s);
        const tc = communities.get(t);
        return sc !== undefined && sc === tc ? 0.6 : 0.2;
      })
      .attr('stroke-width', d => {
        const s = typeof d.source === 'object' ? d.source.id : d.source;
        const t = typeof d.target === 'object' ? d.target.id : d.target;
        const sc = communities.get(s);
        const tc = communities.get(t);
        return sc !== undefined && sc === tc ? 1.5 : 0.8;
      });
  }

  function centerOnMatches() {
    if (!searchQuery) return;
    const matches = nodes.filter(d => d.id.toLowerCase().includes(searchQuery) && d.x != null);
    if (!matches.length) return;
    const cx = matches.reduce((s, d) => s + d.x, 0) / matches.length;
    const cy = matches.reduce((s, d) => s + d.y, 0) / matches.length;
    // Preserve current zoom scale, only translate to center the matches
    const currentTransform = d3.zoomTransform(svg.node());
    svg.transition().duration(500).call(
      d3.zoom().scaleExtent([0.1, 4]).on('zoom', e => g.attr('transform', e.transform)).transform,
      d3.zoomIdentity.scale(currentTransform.k).translate(
        (width / 2 - cx * currentTransform.k) / currentTransform.k,
        (height / 2 - cy * currentTransform.k) / currentTransform.k
      )
    );
  }

  // Initial edge styling
  applyVisibility();
}

function drag(simulation) {
  return d3.drag()
    .on('start', (event, d) => {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x; d.fy = d.y;
    })
    .on('drag', (event, d) => {
      d.fx = event.x; d.fy = event.y;
    })
    .on('end', (event, d) => {
      if (!event.active) simulation.alphaTarget(0);
      // keep node fixed where user dropped it
    });
}

document.addEventListener('DOMContentLoaded', init);
