google.charts.load('current', { packages: ['corechart'] });
google.charts.setOnLoadCallback(fetchAndDrawHistogram);

function fetchAndDrawHistogram() {
  fetch('/api/histogram-data')
    .then((response) => response.json())
    .then((data) => {
      const combinedData = combineMatrixData(data);
      console.log('Combined Data:', combinedData);

      drawHistogram(combinedData);
    })
    .catch((error) => console.error('Error fetching histogram data:', error));
}

function combineMatrixData(matrix) {
  const { columnHeaders, rowHeaders, dataRows } = matrix;

  // Combine each row header with its corresponding data row
  const combinedRows = rowHeaders.map((rowHeader, index) => [rowHeader].concat(dataRows[index]));

  // Return the full heterogeneous array
  return [columnHeaders,...combinedRows];
}

function drawHistogram(rows) {
  // Expect rows to be a 2D array, including header row
  const data = google.visualization.arrayToDataTable(rows);

  const options = {
    width: 800,
    height: 600,
    legend: { position: 'top', maxLines: 3 },
    isStacked: false, // Changed to false for regular bar chart
    bar: { 
      groupWidth: '75%',
      spacing: 2 // Add spacing between bars in the same group
    },
    hAxis: { 
      title: 'Month',
      slantedText: true, // Make month labels slanted for better readability
      slantedTextAngle: 45
    },
    vAxis: { 
      title: 'Transactions',
      minValue: 0 // Ensure the axis starts at 0
    },
    chartArea: {
      width: '80%', // Give more space for the bars
      height: '70%'
    },
    colors: ['#4285F4', '#34A853', '#FBBC05', '#EA4335', '#5E35B1', '#00ACC1'], // Google-style colors
    animation: {
      startup: true,
      duration: 500,
      easing: 'out'
    }
  };

  const chart = new google.visualization.BarChart(
    document.getElementById('histogram_chart')
  );
  chart.draw(data, options);
}
