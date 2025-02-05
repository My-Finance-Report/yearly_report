function showTabWithSubtabs(tabIndex) {


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

function showSubTab(subTabIndex){

  let subtabButtons = document.querySelectorAll(".subtab-button");
  for (let btn of subtabButtons) {
    btn.removeAttribute("disabled");
  }

  let activeSubBtn = document.querySelector('.subtab-button[data-subtab-index="' + subTabIndex + '"]');
  if (activeSubBtn) {
    activeSubBtn.setAttribute("disabled", "true");
  }

  let visibleTab = document.querySelector('.tab-content[style*="display: block"]');

  const tabIndex = visibleTab.dataset.tabIndex

  let subtabs = visibleTab.getElementsByClassName("subtab-content");
  for (let s of subtabs) {
    s.style.display = "none";
  }

  let subtabId = "subtab-content-" + tabIndex + "-" + subTabIndex;
  let chosenSub = document.getElementById(subtabId);
  if (chosenSub) {
    chosenSub.style.display = "block";
  }


}

function showSubTabBlah(subtabIndex) {
  console.log('show sub tab')
  console.log(subtabIndex)
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


    let selectedGroupId = activeSubBtn.getAttribute("data-group-id");
    let newUrl = new URL(window.location);
    if (selectedGroupId) {
      newUrl.searchParams.set("groupId", selectedGroupId);
    } else {
      newUrl.searchParams.delete("groupId");
    }
    window.history.pushState({}, "", newUrl);


}

function toggleDetails(sectionId) {
  console.log(sectionId)
  const section = document.getElementById(sectionId);
  const row = document.querySelector(`[onclick="toggleDetails('${sectionId}')"]`);
  if (section.classList.contains('hidden')) {
    console.log('removing hidden')
    section.classList.remove('hidden');
  }
  
  else {
    console.log('adding hidden')
    section.classList.add('hidden');
  }
}

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


document.addEventListener("DOMContentLoaded", () => {
  let params = new URLSearchParams(window.location.search);
  let sourceId = params.get("sourceId");
  let groupId = params.get("groupId");

  if (sourceId) {
    let matchingButton = document.querySelector(`.tab-button[data-source-id="${sourceId}"]`);
    if (matchingButton) {
      let tabIndex = matchingButton.getAttribute("data-tab-index");
      showTabWithSubtabs(tabIndex);
      showSubTab(0,tabIndex)
    }
  } else {
    showTabWithSubtabs(0);
  }

  if (groupId) {
    let matchingButton = document.querySelector(`.subtab-button[data-group-id="${groupId}"]`);
    if (matchingButton) {
      let tabIndex = matchingButton.getAttribute("data-subtab-index");
      showSubTab(tabIndex);
    }
  } else {
    showSubTab(0);
  }


});