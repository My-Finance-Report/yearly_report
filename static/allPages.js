document.addEventListener("DOMContentLoaded", () => {
    const urlHash = window.location.hash;
    if (urlHash) {
        const targetRow = document.querySelector(urlHash);
        if (targetRow) {
            targetRow.setAttribute("data-highlight", "true"); // Use an attribute instead of a class
            targetRow.scrollIntoView({ behavior: "smooth", block: "center" });
        }
    }
});





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


document.addEventListener("DOMContentLoaded", function () {
  function toggleUpdateButton(inputElement) {
    const form = inputElement.closest("form");
    const updateButton = form.querySelector(".update-button");

    if (!updateButton) return;

    // Get all form elements that can trigger updates
    const textInput = form.querySelector("input[name='updatedSourceName']");
    const selectInput = form.querySelector("select[name='updatedSourceKind']");

    // Check if the text input value has changed
    const textChanged = textInput && textInput.value.trim() !== textInput.defaultValue.trim();

    // Check if the select dropdown value has changed
    const selectChanged = selectInput && selectInput.value !== selectInput.defaultValue;

    // Enable button if any value has changed
    if (textChanged || selectChanged) {
      updateButton.removeAttribute("disabled");
    } else {
      updateButton.setAttribute("disabled", "true");
    }
  }

  // Attach event listeners to all relevant input fields and dropdowns
  document.querySelectorAll("input[name='updatedSourceName'], select[name='updatedSourceKind']").forEach(element => {
    element.addEventListener("input", function () {
      toggleUpdateButton(this);
    });

    // Ensure dropdown updates trigger the check
    if (element.tagName === "SELECT") {
      element.addEventListener("change", function () {
        toggleUpdateButton(this);
      });
    }
  });
});

