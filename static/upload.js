document.addEventListener("DOMContentLoaded", () => {
  const dropzone = document.getElementById("dropzone");
  const fileInput = document.getElementById("pdfFileInput");
  const fileList = document.getElementById("fileList");
  const uploadButton = document.getElementById("uploadButton");

  if (!dropzone || !fileInput || !fileList || !uploadButton) {
    console.error("One or more elements missing in DOM.");
    return;
  }

  // Prevent default behavior to allow dropping
  dropzone.addEventListener("dragover", (event) => {
    event.preventDefault();
    dropzone.classList.add("bg-gray-200"); // Highlight dropzone
  });

  // Handle dropped files
  dropzone.addEventListener("drop", (event) => {
    event.preventDefault();
    dropzone.classList.remove("bg-gray-200");

    const files = event.dataTransfer.files;
    fileInput.files = files;
    updateFileList();
  });

  // Make file list updater globally available
  window.updateFileList = () => {
    fileList.innerHTML = "";

    if (!fileInput.files.length) {
      uploadButton.disabled = true;
      return;
    }

    for (const file of fileInput.files) {
      const li = document.createElement("li");
      li.textContent = `📄 ${file.name} (${(file.size / 1024).toFixed(2)} KB)`;
      fileList.appendChild(li);
    }

    uploadButton.disabled = false;
    uploadButton.textContent = `Upload ${fileInput.files.length} File(s)`;
  };

  // Make triggerFileInput globally available
  window.triggerFileInput = () => fileInput.click();

  // Attach event listener for input file selection
  fileInput.addEventListener("change", updateFileList);
});
