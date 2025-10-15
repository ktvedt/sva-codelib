// Store the current active filters (selected filter values and search text)
let activeFilters = {
  themes: new Set(),
  methods: new Set(),
  language: new Set(),
  data: new Set(),
  search: ""
};

const FILTER_FIELDS = ["themes", "methods", "language", "data"];

const filterSelectionOrder = {
  themes: [],
  methods: [],
  language: [],
  data: []
};

let currentFilterPopup = null;
let currentFilterPopupAnchor = null;
let outsideClickHandler = null;
let escapeKeyHandler = null;
let lastRenderedCount = null;

// Safeguard the contents of filtering fields
const asText = v => Array.isArray(v) ? v.join(" ") : (v == null ? "" : String(v));
const toArray = v => Array.isArray(v) ? v : (v == null ? [] : [v]);

function closeFilterPopup() {
  if (!currentFilterPopup) return;
  currentFilterPopup.remove();
  currentFilterPopup = null;
  currentFilterPopupAnchor = null;
  if (outsideClickHandler) {
    document.removeEventListener("click", outsideClickHandler, true);
    outsideClickHandler = null;
  }
  if (escapeKeyHandler) {
    document.removeEventListener("keydown", escapeKeyHandler);
    escapeKeyHandler = null;
  }
}

function toggleFilterPopup(anchor, field, label, filters, data) {
  if (currentFilterPopup && currentFilterPopupAnchor === anchor) {
    closeFilterPopup();
    return;
  }
  openFilterPopup(anchor, field, label, filters, data);
}

function openFilterPopup(anchor, field, label, filters, data) {
  closeFilterPopup();

  const popup = document.createElement("div");
  popup.className = "filter-popup";

  const title = document.createElement("div");
  title.className = "filter-popup-title";
  title.textContent = `${label}: flere filtre`;
  popup.appendChild(title);

  const btnContainer = document.createElement("div");
  btnContainer.className = "filter-popup-btns";

  filters.forEach(value => {
    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "filter-btn";
    btn.textContent = value;
    if (activeFilters[field].has(value)) btn.classList.add("active");
    btn.onclick = ev => {
      ev.preventDefault();
      ev.stopPropagation();
      if (activeFilters[field].has(value)) {
        activeFilters[field].delete(value);
        filterSelectionOrder[field] = filterSelectionOrder[field].filter(v => v !== value);
      } else {
        activeFilters[field].add(value);
        filterSelectionOrder[field] = filterSelectionOrder[field].filter(v => v !== value);
        filterSelectionOrder[field].push(value);
      }
      closeFilterPopup();
      updateFilters(data);
      renderProjects(data);
    };
    btnContainer.appendChild(btn);
  });

  popup.appendChild(btnContainer);
  document.body.appendChild(popup);
  popup.addEventListener("click", ev => ev.stopPropagation());

  // Position popup near anchor without shifting layout
  const rect = anchor.getBoundingClientRect();
  const popupRect = popup.getBoundingClientRect();
  let top = rect.bottom + 8;
  if (top + popupRect.height > window.innerHeight - 16) {
    top = Math.max(16, rect.top - popupRect.height - 8);
  }
  let left = rect.left;
  if (left + popupRect.width > window.innerWidth - 16) {
    left = Math.max(16, window.innerWidth - popupRect.width - 16);
  }

  popup.style.top = `${top}px`;
  popup.style.left = `${left}px`;

  currentFilterPopup = popup;
  currentFilterPopupAnchor = anchor;

  outsideClickHandler = event => {
    if (!currentFilterPopup) return;
    if (currentFilterPopup.contains(event.target)) return;
    if (event.target === currentFilterPopupAnchor) return;
    closeFilterPopup();
  };
  document.addEventListener("click", outsideClickHandler, true);

  escapeKeyHandler = event => {
    if (event.key === "Escape") {
      closeFilterPopup();
    }
  };
  document.addEventListener("keydown", escapeKeyHandler);
}

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
    if (lastRenderedCount !== filtered.length) {
      counter.classList.remove("counter-pulse");
      // Force reflow to restart animation on consecutive updates
      // eslint-disable-next-line no-unused-expressions
      counter.offsetWidth;
      counter.classList.add("counter-pulse");
      lastRenderedCount = filtered.length;
    }
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
  closeFilterPopup();
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
  const selectedOrdered = filterSelectionOrder[field]
    .filter(value => activeFilters[field].has(value));
  filterSelectionOrder[field] = selectedOrdered.slice();
  const selectedInCounts = selectedOrdered.filter(value => counts[value]);
  const selectedNotInCounts = selectedOrdered.filter(value => !counts[value]);
  const orderedSelected = [...selectedInCounts, ...selectedNotInCounts];
  const remainingFilters = allFilters.filter(value => !activeFilters[field].has(value));
  const maxPerLine = 4;
  const visibleLimit = Math.max(maxPerLine, orderedSelected.length);
  const visibleValues = [];
  const seen = new Set();

  for (const value of orderedSelected) {
    if (seen.has(value)) continue;
    visibleValues.push(value);
    seen.add(value);
  }
  for (const value of remainingFilters) {
    if (visibleValues.length >= visibleLimit) break;
    if (seen.has(value)) continue;
    visibleValues.push(value);
    seen.add(value);
  }

  // Create a container for the filter buttons
  const btnContainer = document.createElement("div");
  btnContainer.className = "filter-btn-container";

  // Render only the first N filter buttons
  visibleValues.forEach(value => {
    const btn = document.createElement("button");
    btn.className = "filter-btn";
    btn.textContent = value;
    if (activeFilters[field].has(value)) btn.classList.add("active");
    btn.onclick = () => {
      if (activeFilters[field].has(value)) {
        activeFilters[field].delete(value);
        filterSelectionOrder[field] = filterSelectionOrder[field].filter(v => v !== value);
      } else {
        activeFilters[field].add(value);
        filterSelectionOrder[field] = filterSelectionOrder[field].filter(v => v !== value);
        filterSelectionOrder[field].push(value);
      }
      updateFilters(data);
      renderProjects(data);
    };
    btnContainer.appendChild(btn);
  });
  group.appendChild(btnContainer);

  // Only show "og X flere" if there are more available filters than shown and not currently selected
  const hiddenFilters = remainingFilters.filter(value => !visibleValues.includes(value));
  let more;
  if (hiddenFilters.length) {
    more = document.createElement("button");
    more.type = "button";
    more.className = "filter-text-more";
    more.textContent = `og ${hiddenFilters.length} flere filtre`;
    more.onclick = ev => {
      ev.preventDefault();
      ev.stopPropagation();
      toggleFilterPopup(more, field, label, hiddenFilters, data);
    };
  } else {
    more = document.createElement("div");
    more.className = "filter-text-more placeholder";
    more.innerHTML = "&nbsp;";
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
