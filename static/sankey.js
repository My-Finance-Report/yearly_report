document.addEventListener("DOMContentLoaded", function () {
    google.charts.load("current", { packages: ["sankey"] });
    google.charts.setOnLoadCallback(fetchAndDrawSankey);
  
    function fetchAndDrawSankey() {
      fetch("/api/sankey-data")
        .then((response) => response.json())
        .then((data) => drawSankey(data))
        .catch((error) => console.error("Error fetching Sankey data:", error));
    }
  
    function drawSankey(data) {
      const chartData = new google.visualization.DataTable();
      chartData.addColumn("string", "From");
      chartData.addColumn("string", "To");
      chartData.addColumn("number", "Weight");
      chartData.addRows(data);
  
      const options = { width: 800, height: 600 };
      const chart = new google.visualization.Sankey(
        document.getElementById("sankeyChart")
      );
      chart.draw(chartData, options);
    }
  });
  