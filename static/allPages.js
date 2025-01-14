
document.addEventListener("DOMContentLoaded", () => {
    const urlHash = window.location.hash; 
    if (urlHash) {
      const targetRow = document.querySelector(urlHash);
      if (targetRow) {
        targetRow.classList.add("table-row-highlight");
  
        // Optionally scroll to the row
        targetRow.scrollIntoView({ behavior: "smooth", block: "center" });
      }
    }
  });
  