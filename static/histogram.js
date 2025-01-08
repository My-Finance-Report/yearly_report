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
    legend: { position: 'top', maxLines: 2 },
    isStacked: true, // For stacked columns/bars
    bar: { groupWidth: '75%' }, // Bar width adjustment
    hAxis: { title: 'Month' }, // Horizontal axis title
    vAxis: { title: 'Transactions' }, // Vertical axis title
  };

  const chart = new google.visualization.BarChart(
    document.getElementById('histogram_chart')
  );
  chart.draw(data, options);
}
