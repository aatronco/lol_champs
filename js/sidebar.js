/**
 * sidebar.js — manages sidebar UI: filters, search, community list, legend
 *
 * Exports (global):
 *   SidebarController(options) → controller object
 *
 * options: {
 *   communities: Map<nodeId, communityId>,
 *   nodes: array,
 *   onFilterChange: (activeTypes: Set<string>) => void,
 *   onCommunitySelect: (communityId: int | null) => void,
 *   onSearch: (query: string) => void,
 * }
 */

const RELATIONSHIP_TYPES = ['Friendly', 'Antagonistic', 'Romantic', 'Related', 'Other'];

const RELATIONSHIP_COLORS = {
  'Friendly':     '#22c55e',
  'Antagonistic': '#ef4444',
  'Romantic':     '#ec4899',
  'Related':      '#6b7280',
  'Other':        '#e5e7eb',
};

const REGION_COLORS = {
  'Freljord':     '#3b82f6',
  'Noxus':        '#ef4444',
  'Demacia':      '#f8fafc',
  'Ionia':        '#22c55e',
  'Piltover':     '#f59e0b',
  'Zaun':         '#84cc16',
  'Bilgewater':   '#0ea5e9',
  'Shadow Isles': '#7c3aed',
  'Shurima':      '#d97706',
  'Mt. Targon':   '#e879f9',
  'Void':         '#6366f1',
  'Bandle City':  '#fb923c',
  'Ixtal':        '#10b981',
  'Independent':  '#6b7280',
  'Unknown':      '#374151',
};

const COMMUNITY_COLORS = [
  '#60a5fa','#f87171','#34d399','#fbbf24','#a78bfa',
  '#f472b6','#38bdf8','#fb923c','#4ade80','#e879f9',
];

function communityColor(id) {
  return COMMUNITY_COLORS[id % COMMUNITY_COLORS.length];
}

function SidebarController({ communities, nodes, onFilterChange, onCommunitySelect, onSearch }) {
  const activeTypes = new Set(RELATIONSHIP_TYPES);
  let selectedCommunity = null;
  let searchTimer = null;

  // --- Filter chips ---
  const chipsContainer = document.getElementById('filter-chips');
  RELATIONSHIP_TYPES.forEach(type => {
    const chip = document.createElement('div');
    chip.className = 'chip';
    chip.textContent = type;
    chip.style.background = RELATIONSHIP_COLORS[type] + '22';
    chip.style.borderColor = RELATIONSHIP_COLORS[type];
    chip.style.color = RELATIONSHIP_COLORS[type];
    chip.dataset.type = type;
    chip.addEventListener('click', () => {
      if (activeTypes.has(type)) {
        activeTypes.delete(type);
        chip.classList.add('off');
      } else {
        activeTypes.add(type);
        chip.classList.remove('off');
      }
      onFilterChange(new Set(activeTypes));
    });
    chipsContainer.appendChild(chip);
  });

  // --- Community list ---
  const communityList = document.getElementById('community-list');

  if (!communities || communities.size === 0) {
    communityList.textContent = 'No communities detected.';
  } else {
    // Count nodes per community
    const counts = new Map();
    for (const [, cid] of communities) {
      counts.set(cid, (counts.get(cid) || 0) + 1);
    }
    // Sort by size descending
    const sorted = [...counts.entries()].sort((a, b) => b[1] - a[1]);

    sorted.forEach(([cid, count]) => {
      const entry = document.createElement('div');
      entry.className = 'community-entry';
      entry.innerHTML = `
        <div class="community-swatch" style="background:${communityColor(cid)}"></div>
        <div class="community-name">Community ${cid}</div>
        <div class="community-count">(${count} champions)</div>
      `;
      entry.addEventListener('click', () => {
        if (selectedCommunity === cid) {
          selectedCommunity = null;
          entry.classList.remove('active');
          onCommunitySelect(null);
        } else {
          document.querySelectorAll('.community-entry').forEach(e => e.classList.remove('active'));
          selectedCommunity = cid;
          entry.classList.add('active');
          onCommunitySelect(cid);
        }
      });
      communityList.appendChild(entry);
    });
  }

  // --- Region legend ---
  const legendContainer = document.getElementById('region-legend');
  const regions = [...new Set(nodes.map(n => n.region))].sort();
  regions.forEach(region => {
    const entry = document.createElement('div');
    entry.className = 'legend-entry';
    entry.innerHTML = `
      <div class="legend-dot" style="background:${REGION_COLORS[region] || '#374151'}"></div>
      <span>${region}</span>
    `;
    legendContainer.appendChild(entry);
  });

  // --- Search ---
  const searchInput = document.getElementById('search');
  searchInput.addEventListener('input', () => {
    clearTimeout(searchTimer);
    searchTimer = setTimeout(() => {
      onSearch(searchInput.value.trim().toLowerCase());
    }, 300);
  });

  return { REGION_COLORS, COMMUNITY_COLORS, communityColor };
}
