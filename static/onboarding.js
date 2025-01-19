
const markProcessing = (form) => {
  const finishButton = document.getElementById("finish-button");

  form.querySelector("input[type='file']").disabled = true;
  form.querySelector("input[type='submit']").value = "Uploading...";
  form.querySelector("input[type='submit']").disabled = true;

  setTimeout(() => {
    form.innerHTML = "<p class='text-green-600 font-medium'>Completed</p>";
    checkAllUploads();
  }, 2000); // Simulate upload delay
};

const checkAllUploads = () => {
  const allCompleted = document.querySelectorAll(".upload-section p.text-green-600").length === document.querySelectorAll(".upload-section").length;
  const finishButton = document.getElementById("finish-button");

  if (allCompleted) {
    finishButton.disabled = false;
    finishButton.classList.remove("opacity-50", "cursor-not-allowed");
  }
};

