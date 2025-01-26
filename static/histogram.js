google.charts.load('current', { packages: ['corechart'] });
google.charts.setOnLoadCallback(fetchAndDrawHistogram);

function fetchAndDrawHistogram() {
   const apiEndpoint = window.location.pathname.includes('/demo-account')
        ? '/demo/api/histogram-data'
        : '/api/column-data';


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
    height: "100%",
    legend: { position: 'top', maxLines: 3 },
    isStacked: false, 
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
