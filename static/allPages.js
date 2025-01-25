
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


document.addEventListener("DOMContentLoaded", function () {
  const image = document.getElementById("tilting-image");

  image.addEventListener("mousemove", function (event) {
    const rect = image.getBoundingClientRect();
    const x = event.clientX - rect.left; // X position within the element
    const y = event.clientY - rect.top; // Y position within the element
    const centerX = rect.width / 2;
    const centerY = rect.height / 2;
    
    const rotateX = ((y - centerY) / centerY) * 10; // Max 10 degrees tilt
    const rotateY = ((centerX - x) / centerX) * 10; // Max 10 degrees tilt

    image.style.transform = `rotateX(${rotateX}deg) rotateY(${rotateY}deg)`;
    image.style.transition = "transform 0.1s ease-out";
  });

  image.addEventListener("mouseleave", function () {
    image.style.transform = "rotateX(0deg) rotateY(0deg)";
    image.style.transition = "transform 0.3s ease-out";
  });
});
