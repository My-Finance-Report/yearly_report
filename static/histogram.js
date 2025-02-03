google.charts.load('current', { packages: ['corechart'] });
google.charts.setOnLoadCallback(fetchAndDrawHistogram);

function fetchAndDrawHistogram() {

  const urlParams = new URLSearchParams(window.location.search);
  const sourceId = urlParams.get("sourceId");

  let apiEndpoint = window.location.pathname.includes('/demo-account')
        ? '/demo/api/histogram-data'
        : '/api/column-data';

  if (sourceId) {
    apiEndpoint += `?sourceId=${sourceId}`;
  }


  fetch(apiEndpoint)
    .then((response) => response.json())
    .then((data) => {
      if (!data || !data.rowHeaders || data.rowHeaders.length === 0) {
        displayNoDataMessage();
      } else {
        const combinedData = combineMatrixData(data);
        drawHistogram(combinedData);
      }
    })
    .catch((error) => {
      console.error('Error fetching histogram data:', error);
      displayNoDataMessage();
    });
}

function combineMatrixData(matrix) {
  const { columnHeaders, rowHeaders, dataRows } = matrix;
  const combinedRows = rowHeaders.map((rowHeader, index) => [rowHeader].concat(dataRows[index]));
  return [columnHeaders, ...combinedRows];
}

function drawHistogram(rows) {
  const data = google.visualization.arrayToDataTable(rows);

  const options = {
    width: "100%",
    height: getChartHeight(),
    legend: { 
      position: 'top',  // Moves legend below the chart
      maxLines: 4,  // Allows wrapping
      textStyle: {
        fontSize: 14,  // Increases font size
        color: "#333", // Darker text color for better readability
        bold: true,    // Makes text stand out
      },
      alignment: 'center' // Center the legend nicely
    },
    isStacked: true, 
    bar: { 
      groupWidth: '75%',
      spacing: 2 
    },
    hAxis: { 
      slantedText: true, 
      slantedTextAngle: 45,
      gridlines: { color: 'transparent' }
    },
    vAxis: { 
      minValue: 0 ,
      gridlines: { color: 'transparent' }
    },
    chartArea: {
      width: '70%', 
      height: '70%'
    },
    colors: ['#4285F4', '#34A853', '#FBBC05', '#EA4335', '#5E35B1', '#00ACC1'], 
    animation: {
      startup: true,
      duration: 500,
      easing: 'out'
    }
  };

  const chart = new google.visualization.ColumnChart(
    document.getElementById('histogram_chart')
  );
  chart.draw(data, options);
}

function displayNoDataMessage() {
  const chartContainer = document.getElementById('histogram_chart');
  chartContainer.innerHTML = '<p>No transactions yet</p>';
  chartContainer.style.textAlign = 'center';
  chartContainer.style.fontSize = '18px';
  chartContainer.style.color = '#666';
}


function getChartHeight() {
  return window.innerWidth < 768 ? 300 : 500;  // Adjust height for mobile
}


window.addEventListener("resize", () => {
  options.height = getChartHeight();
  drawSankeyChart();  
});

window.addEventListener("popstate", () => {
  fetchAndDrawHistogram();
});