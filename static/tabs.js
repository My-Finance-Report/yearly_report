function showTabWithSubtabs(tabIndex) {
  // (A) Re-enable all tab-button elements
  let allTabButtons = document.querySelectorAll(".tab-button");
  for (let btn of allTabButtons) {
    btn.removeAttribute("disabled");
  }

  // (B) Hide all tab-content elements
  let tabs = document.getElementsByClassName("tab-content");
  for (let t of tabs) {
    t.style.display = "none";
  }

  // (C) Show the chosen tab
  let chosenTab = document.getElementById("tab-content-" + tabIndex);
  if (chosenTab) {
    chosenTab.style.display = "block";
  }

  // (D) Disable the newly "active" button
  //     (Assumes each button has data-tab-index="<idx>" to match tabIndex)
  let activeButton = document.querySelector('.tab-button[data-tab-index="' + tabIndex + '"]');
  if (activeButton) {
    activeButton.setAttribute("disabled", "true");
  }

  // (E) Update the URL without reloading the page
  let selectedSourceId = activeButton.getAttribute("data-source-id");

  // Update URL with the selected sourceId
  let newUrl = new URL(window.location);
  if (selectedSourceId) {
    newUrl.searchParams.set("sourceId", selectedSourceId);
  } else {
    newUrl.searchParams.delete("sourceId");
  }
  window.history.pushState({}, "", newUrl);

  fetchAndDrawHistogram()

}

function showSubTab(subtabIndex) {
  // Find the currently visible tab
  let visibleTab = document.querySelector('.tab-content[style*="display: block"]');
  if (!visibleTab) {
    return;
  }

  // (A) Re-enable all subtab-button elements *within* the visible tab
  let subtabButtons = document.querySelectorAll(".subtab-button");
  for (let btn of subtabButtons) {
    btn.removeAttribute("disabled");
  }

  // (B) Hide all .subtab-content sections in the visible tab
  let subtabs = visibleTab.getElementsByClassName("subtab-content");
  for (let s of subtabs) {
    s.style.display = "none";
  }

  // (C) Show the requested subtab
  let tabIndex = visibleTab.id.replace("tab-content-", "");
  let subtabId = "subtab-content-" + tabIndex + "-" + subtabIndex;
  let chosenSub = document.getElementById(subtabId);
  if (chosenSub) {
    chosenSub.style.display = "block";
  }

  // (D) Disable the newly "active" subtab button
  //     (Assumes each subtab button has data-subtab-index="<subtabIndex>" in the visible tab)
  let activeSubBtn = document.querySelector('.subtab-button[data-subtab-index="' + subtabIndex + '"]');

  if (activeSubBtn) {
    activeSubBtn.setAttribute("disabled", "true");
  }
}

function toggleDetails(sectionId) {
  let row = document.getElementById(sectionId);
  if (!row) {
    return;
  }

  // Toggle the "hidden" class
  if (row.classList.contains("hidden")) {
    row.classList.remove("hidden");
  } else {
    row.classList.add("hidden");
  }
}

function toggleArrow(clickedRow) {
  let arrowEl = clickedRow.querySelector(".arrow");
  if (!arrowEl) {
    return;
  }

  // Switch arrow from ▶ to ▼ (and vice versa)
  arrowEl.textContent = (arrowEl.textContent.trim() === "▶") ? "▼" : "▶";
}



document.addEventListener("DOMContentLoaded", () => {
  let params = new URLSearchParams(window.location.search);
  let sourceId = params.get("sourceId");
  if (sourceId) {
    let matchingButton = document.querySelector(`.tab-button[data-source-id="${sourceId}"]`);
    if (matchingButton) {
      let tabIndex = matchingButton.getAttribute("data-tab-index");
      showTabWithSubtabs(tabIndex);
    }
  } else {
    console.log("setting to index 0")
    showTabWithSubtabs(0);
  }
});