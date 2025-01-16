document.addEventListener("DOMContentLoaded", function () {
  google.charts.load("current", { packages: ["sankey"] });
  google.charts.setOnLoadCallback(fetchAndDrawSankey);

  function fetchAndDrawSankey() {
    fetch("/api/sankey-data")
      .then((response) => response.json())
      .then((data) => drawSankey(data))
      .catch((error) => {
        console.error("Error fetching Sankey data:", error);
        displayNoDataMessage();
      });
  }

  function drawSankey(data) {
    if (data.length === 0) {
      displayNoDataMessage();
      return;
    }

    const chartData = new google.visualization.DataTable();
    chartData.addColumn("string", "From");
    chartData.addColumn("string", "To");
    chartData.addColumn("number", "Weight");
    chartData.addRows(data);

    const options = {
      width: 600,
      height: 400,
      sankey: {
        link: {
          colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f']
        }
      }
    };

    const chart = new google.visualization.Sankey(
      document.getElementById("sankeyChart")
    );
    chart.draw(chartData, options);
  }

  function displayNoDataMessage() {
    const sankeyContainer = document.getElementById("sankeyChart");
    sankeyContainer.innerHTML = "<p>No transactions yet</p>";
    sankeyContainer.style.textAlign = "center";
    sankeyContainer.style.fontSize = "18px";
    sankeyContainer.style.color = "#666";
  }
});
