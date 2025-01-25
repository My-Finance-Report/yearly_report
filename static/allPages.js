
document.addEventListener("DOMContentLoaded", () => {
    const urlHash = window.location.hash; 
    if (urlHash) {
      const targetRow = document.querySelector(urlHash);
      if (targetRow) {
        targetRow.classList.add("table-row-highlight");
  
        targetRow.scrollIntoView({ behavior: "smooth", block: "center" });
      }
    }
  });
  

function toggleArrow(row) {
  const arrow = row.querySelector("td span"); // Find the arrow inside the row
  if (!arrow) return;

  if (row.dataset.rotated === "true") {
    row.dataset.rotated = "false";
    arrow.classList.remove("rotate-90");
  } else {
    row.dataset.rotated = "true";
    arrow.classList.add("rotate-90");
  }
}


document.addEventListener("DOMContentLoaded", function () {
  const buttons = [
    { id: "configureChartsButton", path: "/new-configuration" },
    { id: "homeButton", path: "/dashboard" },
    { id: "manageAccountsButton", path: "/manage-accounts" },
    { id: "addTransactionsButton", path: "/upload" }
  ];

  const currentPath = window.location.pathname;

  buttons.forEach(({ id, path }) => {
    const button = document.getElementById(id);
    if (button) {
      if (currentPath === path) {
        button.setAttribute("disabled", "true"); 
      } else {
        button.removeAttribute("disabled"); 
      }
    }
  });
});

