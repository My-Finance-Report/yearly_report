
function showTabWithSubtabs(index) {
  var tabs = document.getElementsByClassName('tab');
  for (var i = 0; i < tabs.length; i++) {
    if (i === index) {
      tabs[i].classList.add('active');
    } else {
      tabs[i].classList.remove('active');
    }
  }

  var tabContents = document.getElementsByClassName('tab-content');
  for (var i = 0; i < tabContents.length; i++) {
    tabContents[i].style.display = (i === index) ? 'block' : 'none';
  }

  showSubTab(index, 0);
}

function showSubTab(tabIndex, subTabIndex) {
  var tabContent = document.getElementById(`tab-content-${tabIndex}`);
  var subtabs = tabContent.getElementsByClassName('tab');
  for (var i = 0; i < subtabs.length; i++) {
    if (i === subTabIndex) {
      subtabs[i].classList.add('active');
    } else {
      subtabs[i].classList.remove('active');
    }
  }

  var subTabContents = tabContent.getElementsByClassName('subtab-content');
  for (var i = 0; i < subTabContents.length; i++) {
    subTabContents[i].style.display = (i === subTabIndex) ? 'block' : 'none';
  }
}
