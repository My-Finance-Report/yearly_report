document.addEventListener("DOMContentLoaded", function () {
  google.charts.load("current", { packages: ["sankey"] });
  google.charts.setOnLoadCallback(fetchAndDrawSankey);


  function fetchAndDrawSankey() {
      const apiEndpoint = window.location.pathname.includes('/demo-account')
          ? '/demo/api/sankey-data'
          : '/api/sankey-data';


    fetch(apiEndpoint)
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
      width: "100%",
      height: getChartHeight(),
      sankey: {
        iterations: 0,
        link: {
          colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f']
        },
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

function getChartHeight() {
  return window.innerWidth < 768 ? 300 : 500;  // Adjust height for mobile
}


window.addEventListener("resize", () => {
  options.height = getChartHeight();
  drawSankeyChart();  // Re-render chart function
});
