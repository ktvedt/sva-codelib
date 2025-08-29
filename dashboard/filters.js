// Store the current active filters (selected filter values and search text)
let activeFilters = {
  themes: new Set(),
  methods: new Set(),
  language: new Set(),
  data: new Set(),
  search: ""
};

// use these wrappers to safeguard the contents of filtering fields
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
  const filterFields = ["themes", "methods", "language", "data"];
  return searchMatch && filterFields.every(field => {
    const selected = activeFilters[field];
    const values = Array.isArray(project[field]) ? project[field] : [project[field]];
    // All selected filter values for this field must be present in the project
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
    counter.textContent = `${filtered.length} prosjekt${filtered.length === 1 ? '' : 'er'}`;
  }

  filtered.forEach(project => {
    const card = document.createElement("div");
    card.className = "project-card";
    card.innerHTML = `
      <h3>${project.title}</h3>
      <p><strong>Forfatter:</strong> ${(project.authors || []).join(", ")}</p>
      <p><strong>Programmeringsspråk:</strong> ${project.language}</p>
      <p><strong>Data:</strong> ${(project.data || []).join(", ")}</p>
      <p><strong>Metode:</strong> ${(project.methods || []).join(", ")}</p>
      <p><strong>Tema:</strong> ${(project.themes || []).join(", ")}</p>
      <p>${project.description || ""}</p>
      <p><a href="https://github.com/ktvedt/sva-codelib/tree/main/projects/${project.folder}" target="_blank">Finn filer</a></p>
    `;
    container.appendChild(card);
  });
}

// Render all filter groups (language, data, methods, themes)
function updateFilters(data) {
  document.getElementById("filters").innerHTML = "";
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
  const unique = new Set();

  // Only show filter options that are available after applying other filters
  const filterFields = ["themes", "methods", "language", "data"];
  const filteredData = data.filter(project => {
    return filterFields.every(f => {
      if (f === field) return true; // Don't filter on the current field
      const selected = activeFilters[f];
      const values = Array.isArray(project[f]) ? project[f] : [project[f]];
      return [...selected].every(val => values.includes(val));
    });
  });

  // Collect unique values for this filter group
  filteredData.forEach(p => {
    const values = Array.isArray(p[field]) ? p[field] : [p[field]];
    values.forEach(v => v && unique.add(v));
  });

  // Render up to 5 filter buttons for this group
  [...unique].sort().slice(0, 5).forEach(value => {
    const btn = document.createElement("button");
    btn.className = "filter-btn";
    btn.textContent = value;
    if (activeFilters[field].has(value)) btn.classList.add("active");
    btn.onclick = () => {
      // Toggle filter value on click
      activeFilters[field].has(value) ? activeFilters[field].delete(value) : activeFilters[field].add(value);
      updateFilters(data);
      renderProjects(data);
    };
    group.appendChild(btn);
  });

  filtersContainer.appendChild(group);
}

// Fetch project data and initialize the UI
fetch("projects.json")
  .then(res => res.json())
  .then(data => {
    renderProjects(data);
    updateFilters(data);
    // Update search filter on input
    document.getElementById("search-box").addEventListener("input", e => {
      activeFilters.search = e.target.value;
      renderProjects(data);
    });
  });