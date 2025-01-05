function updatePreview() {
  const startKeyword = document.getElementById("startKeyword").value.trim();
  const endKeyword = document.getElementById("endKeyword").value.trim();
  const previewContainer = document.getElementById("previewContainer");
  const linesContainer = document.getElementById("linesContainer");
  const lines = Array.from(linesContainer.querySelectorAll("span"));

  // Clear the current preview
  previewContainer.innerHTML = "";

  let startIndex = 0;
  let endIndex = lines.length;

  // Find the start index if a start keyword is provided
  if (startKeyword !== "") {
    const startMatch = lines.findIndex((line) =>
      line.textContent.trim().toLowerCase().includes(startKeyword.toLowerCase())
    );
    if (startMatch !== -1) {
      startIndex = startMatch;
    }
  }

  // Find the end index if an end keyword is provided
  if (endKeyword !== "") {
    const endMatch = lines.findIndex((line, index) =>
      index > startIndex &&
      line.textContent.trim().toLowerCase().includes(endKeyword.toLowerCase())
    );
    if (endMatch !== -1) {
      endIndex = endMatch + 1; // Include the line with the end keyword
    }
  }

  // Extract and display lines within the range
  const selectedLines = lines.slice(startIndex, endIndex);
  if (selectedLines.length > 0) {
    selectedLines.forEach((line) => {
      const lineClone = line.cloneNode(true);
      previewContainer.appendChild(lineClone);
      previewContainer.appendChild(document.createElement("br"));
    });
  } else {
    previewContainer.textContent =
      "No matching lines found between the given keywords.";
  }
}
