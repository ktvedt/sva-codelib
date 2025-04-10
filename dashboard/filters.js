fetch('projects.json')
  .then(res => res.json())
  .then(data => {
    const container = document.getElementById('projects');
    const filters = new Set();
    data.forEach(project => {
      const card = document.createElement('div');
      card.innerHTML = `<h3>${project.title}</h3><p>${project.language} - ${project.model}</p>`;
      container.appendChild(card);
      project.keywords.forEach(k => filters.add(k));
    });

    // Render filter buttons
    const filterContainer = document.getElementById('filters');
    filters.forEach(k => {
      const btn = document.createElement('button');
      btn.textContent = k;
      btn.onclick = () => {
        Array.from(container.children).forEach(card => {
          card.style.display = card.innerText.includes(k) ? 'block' : 'none';
        });
      };
      filterContainer.appendChild(btn);
    });
  });