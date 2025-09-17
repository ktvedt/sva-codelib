// Store the current active filters (selected filter values and search text)
let activeFilters = {
  themes: new Set(),
  methods: new Set(),
  language: new Set(),
  data: new Set(),
  search: ""
};

const FILTER_FIELDS = ["themes", "methods", "language", "data"];

// Safeguard the contents of filtering fields
const asText = v => Array.isArray(v) ? v.join(" ") : (v == null ? "" : String(v));
const toArray = v => Array.isArray(v) ? v : (v == null ? [] : [v]);

// Check if a project matches the current filters and search
function matchesFilters(project) {
  const searchText = activeFilters.search.toLowerCase();
  const title = asText(project.title).toLowerCase();
  const description = asText(project.description).toLowerCase();
  const authors = toArray(project.authors).map(a => a.toLowerCase());

  // Check if search text matches any relevant field
  const searchMatch =
    title.includes(searchText) ||
    description.includes(searchText) ||
    authors.some(a => a.includes(searchText)) ||
    toArray(project.themes).some(t => String(t).toLowerCase().includes(searchText)) ||
    toArray(project.methods).some(m => String(m).toLowerCase().includes(searchText)) ||
    toArray(project.data).some(d => String(d).toLowerCase().includes(searchText)) ||
    asText(project.language).toLowerCase().includes(searchText);

  // Check if project matches all selected filters
  return searchMatch && FILTER_FIELDS.every(field => {
    const selected = activeFilters[field];
    if (!selected.size) return true;
    const values = toArray(project[field]);
    return [...selected].every(f => values.includes(f));
  });
}

// Render the list of projects that match the filters
function renderProjects(data) {
  const container = document.getElementById("projects");
  container.innerHTML = "";
  const filtered = data.filter(matchesFilters);

  // Update the counter
  const counter = document.getElementById("project-counter");
  if (counter) {
    counter.textContent = `${filtered.length} resultat${filtered.length === 1 ? '' : 'er'}`;
  }

  for (const project of filtered) {
    const card = document.createElement("div");
    card.className = "project-card";
    card.innerHTML = `
      <h3>${project.title}</h3>
      <p><strong>Forfatter:</strong> ${toArray(project.authors).join(", ")}</p>
      <p><strong>Programmeringsspråk:</strong> ${toArray(project.language)}</p>
      <p><strong>Data:</strong> ${toArray(project.data).join(", ")}</p>
      <p><strong>Metode:</strong> ${toArray(project.methods).join(", ")}</p>
      <p><strong>Tema:</strong> ${toArray(project.themes).join(", ")}</p>
      <p>${project.description || ""}</p>
      <p><a href="https://github.com/ktvedt/sva-codelib/tree/main/projects/${project.folder}" target="_blank">Finn filer</a></p>
    `;
    container.appendChild(card);
  }
}

// Render all filter groups (language, data, methods, themes)
function updateFilters(data) {
  const filtersContainer = document.getElementById("filters");
  filtersContainer.innerHTML = "";
  renderFilters(data, "language", "Språk");
  renderFilters(data, "data", "Data");
  renderFilters(data, "methods", "Metode");
  renderFilters(data, "themes", "Tema");
}

// Render a single filter group as a set of buttons
function renderFilters(data, field, label) {
  const filtersContainer = document.getElementById("filters");
  const group = document.createElement("div");
  group.className = "filter-group";
  group.innerHTML = `<h4>${label}</h4>`;

  // Only show filter options available after applying other filters (excluding this group)
  const filteredData = data.filter(project => {
    return FILTER_FIELDS.every(f => {
      const selected = activeFilters[f];
      if (!selected.size) return true;
      const values = toArray(project[f]);
      return [...selected].every(val => values.includes(val));
    });
  });

  // Count occurrences of each filter value in the filtered data
  const counts = {};
  for (const p of filteredData) {
    for (const v of toArray(p[field])) {
      if (v) counts[v] = (counts[v] || 0) + 1;
    }
  }

  // Sort filter values by descending count, then alphabetically for ties
  const allFilters = Object.keys(counts).sort((a, b) => 
    counts[b] - counts[a] || a.localeCompare(b)
  );
  const maxPerLine = 4;

  // Create a container for the filter buttons
  const btnContainer = document.createElement("div");
  btnContainer.className = "filter-btn-container";

  // Render only the first N filter buttons
  allFilters.slice(0, maxPerLine).forEach(value => {
    const btn = document.createElement("button");
    btn.className = "filter-btn";
    btn.textContent = value;
    if (activeFilters[field].has(value)) btn.classList.add("active");
    btn.onclick = () => {
      activeFilters[field].has(value) ? activeFilters[field].delete(value) : activeFilters[field].add(value);
      updateFilters(data);
      renderProjects(data);
    };
    btnContainer.appendChild(btn);
  });
  group.appendChild(btnContainer);

  // Only show "og X flere" if there are more available filters than shown and not currently selected
  const hiddenFilters = allFilters.slice(maxPerLine);
  const hiddenUnselected = hiddenFilters.filter(
    value => !activeFilters[field].has(value)
  );
  const more = document.createElement("div");
  more.className = "filter-text-more";
  if (hiddenUnselected.length > 0) {
    more.textContent = `og ${hiddenUnselected.length} flere filtre`;
  } else {
    more.innerHTML = "&nbsp;"; // Non-breaking space to reserve height
  }
  group.appendChild(more);

  filtersContainer.appendChild(group);
}

// Fetch project data and initialize the UI
fetch("projects.json")
  .then(res => res.json())
  .then(data => {
    renderProjects(data);
    updateFilters(data);
    document.getElementById("search-box").addEventListener("input", e => {
      activeFilters.search = e.target.value;
      renderProjects(data);
    });
  });