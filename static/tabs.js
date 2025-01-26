function showTabWithSubtabs(tabIndex) {
  // Hide all tab-content elements
  let tabs = document.getElementsByClassName('tab-content');
  for (let t of tabs) {
    t.style.display = 'none';
  }

  // Show the chosen tab
  let chosenTab = document.getElementById('tab-content-' + tabIndex);
  if (chosenTab) {
    chosenTab.style.display = 'block';
  }
}

function showSubTab(subtabIndex) {
  // Find the currently visible tab
  let visibleTab = document.querySelector('.tab-content[style*="display: block"]');
  if (!visibleTab) {
    return;
  }

  // Within that tab, hide all subtab-content sections
  let subtabs = visibleTab.getElementsByClassName('subtab-content');
  for (let s of subtabs) {
    s.style.display = 'none';
  }

  // Show the requested subtab
  let tabIndex = visibleTab.id.replace('tab-content-', '');
  let subtabId = 'subtab-content-' + tabIndex + '-' + subtabIndex;
  let chosenSub = document.getElementById(subtabId);
  if (chosenSub) {
    chosenSub.style.display = 'block';
  }
}

function toggleDetails(sectionId) {
  let row = document.getElementById(sectionId);
  if (!row) {
    return;
  }

  // Toggle the "hidden" class
  if (row.classList.contains('hidden')) {
    row.classList.remove('hidden');
  } else {
    row.classList.add('hidden');
  }
}

function toggleArrow(clickedRow) {
  let arrowEl = clickedRow.querySelector('.arrow');
  if (!arrowEl) {
    return;
  }

  // Switch arrow from ▶ to ▼ (and vice versa)
  arrowEl.textContent = (arrowEl.textContent.trim() === '▶') ? '▼' : '▶';
}
