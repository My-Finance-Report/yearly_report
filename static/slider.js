
function updateRange() {
    let startSlider = document.getElementById('startLine');
    let endSlider   = document.getElementById('endLine');
    let s = parseInt(startSlider.value);
    let e = parseInt(endSlider.value);
  
    if (e < s) {
      e = s;
      endSlider.value = s;
    }
    highlightRange(s, e);
    document.getElementById('finalStartId').value = s;
    document.getElementById('finalEndId').value   = e;
  }
  
  function highlightRange(start, end) {
    const linesContainer = document.getElementById('linesContainer');
    const children = linesContainer.children;
    for (let i = 0; i < children.length; i++) {
      if (i >= start && i <= end) {
        children[i].style.backgroundColor = '#FFFF99'; // pale yellow
      } else {
        children[i].style.backgroundColor = '';
      }
    }
  }
  