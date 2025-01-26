function updateQueryParams(tabIndex, subTabIndex = null) {
    const url = new URL(window.location);
    url.searchParams.set("tab", tabIndex);
    if (subTabIndex !== null) {
        url.searchParams.set("subtab", subTabIndex);
    } else {
        url.searchParams.delete("subtab");
    }
    window.history.replaceState(null, "", url);
}

function showTabWithSubtabs(tabIndex) {
    document.querySelectorAll(".tab-content").forEach(tab => {
        tab.style.display = "none";
    });

    const selectedTab = document.getElementById(`tab-content-${tabIndex}`);
    if (selectedTab) {
        selectedTab.style.display = "block";
    }

    document.querySelectorAll(".tab-button").forEach(button => {
        button.removeAttribute("disabled");
        button.classList.remove("active-tab"); // Remove green styling
    });

    const clickedTabButton = document.querySelector(`[data-tab-id="tab-content-${tabIndex}"]`);
    if (clickedTabButton) {
        clickedTabButton.setAttribute("disabled", "true");
        clickedTabButton.classList.add("active-tab"); // Add green styling
    }

    // Update URL
    updateQueryParams(tabIndex);

    // Automatically show the first subtab of the selected tab
    const firstSubtabButton = document.querySelector(`#tab-content-${tabIndex} .subtab-button`);
    if (firstSubtabButton) {
        const firstSubtabIndex = firstSubtabButton.getAttribute("data-subtab-id").split("-").pop();
        showSubTab(tabIndex, firstSubtabIndex);
    }
}

function showSubTab(tabIndex, subTabIndex) {
    document.querySelectorAll(`#tab-content-${tabIndex} .subtab-content`).forEach(subtab => {
        subtab.style.display = "none";
    });

    const selectedSubTab = document.getElementById(`subtab-content-${tabIndex}-${subTabIndex}`);
    if (selectedSubTab) {
        selectedSubTab.style.display = "block";
    }

    document.querySelectorAll(`#tab-content-${tabIndex} .subtab-button`).forEach(button => {
        button.removeAttribute("disabled");
        button.classList.remove("active-subtab"); // Remove green styling
    });

    const clickedSubTabButton = document.querySelector(`[data-subtab-id="subtab-content-${tabIndex}-${subTabIndex}"]`);
    if (clickedSubTabButton) {
        clickedSubTabButton.setAttribute("disabled", "true");
        clickedSubTabButton.classList.add("active-subtab"); // Add green styling
    }

    // Update URL
    updateQueryParams(tabIndex, subTabIndex);
}

// Restore tab from URL on page load
document.addEventListener("DOMContentLoaded", function () {
    const urlParams = new URLSearchParams(window.location.search);
    const tabIndex = urlParams.has("tab") ? parseInt(urlParams.get("tab"), 10) : 0;
    const subTabIndex = urlParams.has("subtab") ? parseInt(urlParams.get("subtab"), 10) : null;

    showTabWithSubtabs(tabIndex);

    if (subTabIndex !== null) {
        showSubTab(tabIndex, subTabIndex);
    }
});
