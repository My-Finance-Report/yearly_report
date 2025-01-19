function toggleDetails(sectionId) {
  const section = document.getElementById(sectionId);
  const row = document.querySelector(`[onclick="toggleDetails('${sectionId}')"]`);
  
  if (section.classList.contains('hidden')) {
    section.classList.remove('hidden');
  } else {
    section.classList.add('hidden');
  }
}
