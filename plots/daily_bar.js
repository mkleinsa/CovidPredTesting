function makeDailyBarPlot(externalData, plotDiv, locale) {
    Plotly.d3.csv(externalData, function(data)
        {
            var trace1 = {
                x: unpack(data, locale, "date"),
                y: unpack(data, locale, "daily_cases"),
                name: 'New Cases',
                type: 'bar',
                marker: {
                    color: '#f2c82e',
                }
            };
        
            var trace2 = {
                x: unpack(data, locale, "date"),
                y: unpack(data, locale, "daily_deaths"),
                name: 'Fatalities',
                type: 'bar',
                marker: {
                    color: '#ed553b',
                }
            };
            
            var trace3 = {
                x: unpack(data, locale, "date"),
                y: unpack(data, locale, "daily_recovered"),
                name: 'Recoveries',
                type: 'bar',
                marker: {
                    color: '#138808',
                }
            };
        
            var data = [trace1, trace2, trace3];
        
            var layout = {
                title: title(),
                barmode: 'stack', 
                xaxis: 
                    {
                        tickangle: -45
                    },
                legend: {
                    "orientation": "h",
                    y: -.2,
                },
                yaxis:
                {
                    title: 
                    {
                        text: 'Daily counts'
                    }
                },
                margin: margin(),
            };
        
            var config = {responsive: true};
        
            Plotly.newPlot(
                plotDiv, 
                data, 
                layout,
                config
            );
        } 
    );
};