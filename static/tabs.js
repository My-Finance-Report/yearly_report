
function showTabWithSubtabs(index) {
    var tabs = document.getElementsByClassName('tab-content');
    for (var i = 0; i < tabs.length; i++) {
      tabs[i].style.display = (i === index) ? 'block' : 'none';
    }
    // Default to showing the first subtab
    showSubTab(index, 0);
  }
  
  function showSubTab(tabIndex, subTabIndex) {
    var tabContent = document.getElementById(`tab-content-${tabIndex}`);
    var subtabs = tabContent.getElementsByClassName('subtab-content');
    for (var i = 0; i < subtabs.length; i++) {
      subtabs[i].style.display = (i === subTabIndex) ? 'block' : 'none';
    }
  }
  
 