function makeCountryCompPlot(countryCompData, plotDiv) {
	Plotly.d3.csv(countryCompData, function(data)
	{ 
		function unpackCountry(data, country, header) {
			// extract the specific country
			temp = data.filter(function(row) {
				if (row["Country"] == country)
				{
					return row;
				}

			});
			// return the column of interest
			return temp.map(function(row) {
				return row[header]
			});
		}

		countryList = [];

		for (var i = 0; i < data.length; i++) {
			if (!countryList.includes(data[i]["Country"]) && data[i]["Country"] != "India")
			{
				countryList.push(data[i]["Country"]);
			}
		}

		// alphabetize, case-insensitive)
		countryList.sort((a, b) => a.localeCompare(b, undefined, {sensitivity: 'base'}));

		countryList.push("India");

		colors = ["#0D0887FF", "#2B0594FF", "#42049EFF", 
				  "#5601A4FF", "#6A00A8FF", "#7E03A8FF", 
				  "#900DA4FF", "#A11B9BFF", "#B12A90FF", 
				  "#BF3984FF", "#CC4678FF", "#D6556DFF", 
				  "#E16462FF", "#EA7457FF", "#F1844BFF", 
				  "#F89441FF", "#FCA636FF", "#138808"  ]

		myData = []

		// create traces and add them to the data array
		for (var i = 0; i < countryList.length; i++)
		{
			var caseTrace = {
				x: unpackCountry(data, countryList[i], 'Cases_day'),
				y: unpackCountry(data, countryList[i], 'loess_cases'),
				type: 'scatter',
				text: unpackCountry(data, countryList[i], 'Cases_text'),
				hoverinfo: 'text',
				name: countryList[i],
				showlegend: false,
				legendgroup: countryList[i],
				xaxis: 'x2',
        		yaxis: 'y2',
				visible: "legendonly",
				marker: {
					color: colors[i],
				},
			}

			var deathTrace = {
				x: unpackCountry(data, countryList[i], 'Deaths_day'),
				y: unpackCountry(data, countryList[i], 'loess_deaths'),
				type: 'scatter',
				text: unpackCountry(data, countryList[i], 'Deaths_text'),
				hoverinfo: 'text',
				name: countryList[i],
				legendgroup: countryList[i],
				xaxis: 'x1',
        		yaxis: 'y1',
				visible: "legendonly",
				marker: {
					color: colors[i],
				},
			}

			if (countryList[i] == "India")
			{
				caseTrace.visible = "yes";
				deathTrace.visible = "yes";
			}

			myData.push(caseTrace);
			myData.push(deathTrace);
		}

		var layout = {
			grid: {
				rows: 2,
				columns: 1,
			},
			hovermode: 'closest',
			// top graph, cases
			xaxis: {
				anchor: 'y2',
				title: xaxisResponsiveLabel('cases', 100),
			},
			yaxis2: { 
				title: 'Incident number of reported cases',
				domain: [.6,1],
			},

			// bottom graph, deaths
			xaxis2: {
				anchor: 'y1',

				title: xaxisResponsiveLabel('deaths', 3),
			},
			yaxis: { 
				title: 'Incident number of reported deaths',
				domain: [0,.4],
			},
			margin: marginSizing(),

			annotations: annotations(),
		};
	
		var config = {responsive: true};
	
		Plotly.newPlot(
			plotDiv, 
			myData,
			layout,
			config,
		);
	});
};

window.addEventListener('resize', countryCompResize);

function countryCompResize() {
	if (document.getElementById('national'))
	{
		var updatedLayout = {
			annotations: annotations(),
			xaxis: {
				anchor: 'y2',
				title: xaxisResponsiveLabel('cases', 100),
			},
			xaxis2: {
				anchor: 'y1',
				title: xaxisResponsiveLabel('deaths', 3),
			},
			margin: marginSizing(),
		}
		Plotly.relayout(countryComp, updatedLayout)
	}	
}

function marginSizing()
{
    if (window.matchMedia("(min-width: 815px)").matches){
        return {
			l: 50,
			r: 50,
			b: 50,
			t: 50,
			pad: 0,
		}
    }
	else {
		return {
			l: 50,
			r: 50,
			b: 50,
			t: 70,
			pad: 0,
		}
	}
}

function casesSubtitle()
{
    if (window.matchMedia("(min-width: 815px)").matches) {
        return 'COVID-19 cases in India compared to other countries'
    } 
    else if (window.matchMedia("(min-width: 600px)").matches){
        return 'COVID-19 cases in India<br>compared to other countries'
      }
	else {
		return 'Country COVID-19<br>cases compared'
	}
}

function deathsSubtitle()
{
    if (window.matchMedia("(min-width: 815px)").matches) {
        return 'COVID-19 deaths in India compared to other countries'
    } 
    else if (window.matchMedia("(min-width: 600px)").matches){
        return 'COVID-19 deaths in India<br>compared to other countries'
      }
	else {
		return 'Country COVID-19<br>deaths compared'
	}
}

function xaxisResponsiveLabel(type, num) {
	if (window.matchMedia("(min-width: 500px)").matches) {
        return 'Days since cumulative ' + type + ' passed ' + num;
    } 
    else {
        return 'Days since cumulative<br>' + type + ' passed ' + num;
    }
}

function annotations()
{
	return [
		{
			showarrow: false,
			// aligns bottom of text box with desired location
			yanchor: 'bottom',
			// sets y(0): bottom of plotting area
			// sets y(1): top of plotting area
			yref: 'paper',
			y: 1,
			xanchor: 'left',
			xref: 'paper',
			x: 0,
			align: 'left',

			text: casesSubtitle(),
			font: {
				size: 18,
			},
		},
		{
			showarrow: false,
			// aligns bottom of text box with desired location
			yanchor: 'bottom',
			// sets y(0): bottom of plotting area
			// sets y(1): top of plotting area
			yref: 'paper',
			y: .4,
			xanchor: 'left',
			xref: 'paper',
			x: 0,
			align: 'left',

			text: deathsSubtitle(),
			font: {
				size: 18,
			},
		},
	]
}