function toggleDetails(sectionId) {
  const section = document.getElementById(sectionId);
  const row = document.querySelector(`[onclick="toggleDetails('${sectionId}')"]`);
  
  if (section.classList.contains('hidden')) {
    section.classList.remove('hidden');
    row.classList.add('expanded'); // Add expanded class
  } else {
    section.classList.add('hidden');
    row.classList.remove('expanded'); // Remove expanded class
  }
}
