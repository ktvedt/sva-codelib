let activeFilters = {
    keywords: new Set(),
    themes: new Set(),
    methods: new Set(),
    search: ""
  };
  
  function matchesFilters(project) {
    const searchText = activeFilters.search.toLowerCase();

    const title = (project.title || "").toLowerCase();
    const description = (project.description || "").toLowerCase();
    const searchMatch =
        title.includes(searchText) ||
        description.includes(searchText) ||
        (project.keywords || []).some(k => k.toLowerCase().includes(searchText)) ||
        (project.themes || []).some(t => t.toLowerCase().includes(searchText)) ||
        (project.methods || []).some(m => m.toLowerCase().includes(searchText));
  
    const filterFields = ["keywords", "themes", "methods"];
    const allMatch = filterFields.every(field => {
      const selected = activeFilters[field];
      const values = project[field] || [];
      return [...selected].every(f => values.includes(f));
    });
  
    return searchMatch && allMatch;
  }
  
  function renderProjects(data) {
    const container = document.getElementById("projects");
    container.innerHTML = "";
  
    data.filter(matchesFilters).forEach(project => {
      const card = document.createElement("div");
      card.className = "project-card";
      card.innerHTML = `
        <h3>${project.title}</h3>
        <p><strong>Authors:</strong> ${(project.authors || []).join(", ")}</p>
        <p><strong>Language:</strong> ${project.language}</p>
        <p><strong>Data:</strong> ${(project.data || []).join(", ")}</p>
        <p><strong>Methods:</strong> ${(project.methods || []).join(", ")}</p>
        <p><strong>Themes:</strong> ${(project.themes || []).join(", ")}</p>
        <p>${project.description || ""}</p>
        <p><a href="https://github.com/ktvedt/sva-codelib/tree/main/projects/${project.folder}" target="_blank">View full project on GitHub</a></p>
`;
      container.appendChild(card);
    });
  }
  
  function renderFilters(data, field, containerId) {
    const container = document.getElementById(containerId);
    const unique = new Set();
    data.forEach(p => (p[field] || []).forEach(v => unique.add(v)));
  
    [...unique].sort().forEach(value => {
      const btn = document.createElement("button");
      btn.className = "filter-btn";
      btn.textContent = value;
      btn.onclick = () => {
        if (activeFilters[field].has(value)) {
          activeFilters[field].delete(value);
          btn.classList.remove("active");
        } else {
          activeFilters[field].add(value);
          btn.classList.add("active");
        }
        renderProjects(data);
      };
      container.appendChild(btn);
    });
  }
  
  fetch("projects.json")
    .then(res => res.json())
    .then(data => {
      renderProjects(data);
      renderFilters(data, "keywords", "filters");
      renderFilters(data, "themes", "filters");
      renderFilters(data, "methods", "filters");
  
      document.getElementById("search-box").addEventListener("input", (e) => {
        activeFilters.search = e.target.value;
        console.log("Search input triggered:", e.target.value); // debug
        renderProjects(data);
      });
    });