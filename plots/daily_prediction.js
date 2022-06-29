function makeDailyPredictionPlot(externalData, plotDiv) {
    
    var locale = "India";

    Plotly.d3.csv(externalData, function(data)
        {
            var trace1 = {
                x: unpack(data, locale, "date"),
                y: unpack(data, locale, "daily_cases"),
                name: 'SEIR daily new cases',
                type: 'bar',
                xaxis: 'x1',
        		yaxis: 'y1',
                marker: {
                    color: '#138808',
                }
            };

            var trace2 = {
                x: unpack(data, locale, "date"),
                y: unpack(data, locale, "daily_deaths"),
                name: 'SEIR daily new deaths',
                type: 'bar',
                xaxis: 'x2',
        		yaxis: 'y2',
                marker: {
                    color: '#FF9933',
                }
            };
        
            var myData = [trace1, trace2];

            var layout = {
                title: title(),
                grid: {
                    rows: 2,
                    columns: 1,
                },
                barmode: 'stack', 
                xaxis: 
                    {
                        anchor: 'y2',
                        tickangle: 0,
                        title: 'Date',
                    },
                yaxis2: { 
                    title: 'Projected number of daily new deaths',
                },
                legend: {
                    "orientation": "h",
                    y: -.2,
                },
                xaxis2: {
                  anchor: 'y1',
                  //title: 'Date',
                },
                yaxis:
                {
                    title: 
                    {
                        text: 'Projected number of daily new cases'
                    }
                },
                margin: margin(),
            };

            //var layout = {
            //    grid: {
            //        rows: 2,
            //        columns: 1,
            //    },
            //    hovermode: 'closest',
            //    // top graph, cases
            //    xaxis: {
            //        // anchor: 'y2',
            //        title: 'Date',
            //    },
            //    //yaxis2: { 
                //    title: 'Projected number of daily cases',
                //    //domain: [.6,1],
                //},
    
                // bottom graph, deaths
                //xaxis2: {
                //    //anchor: '',
    
                //    title: 'Date',
                //},
            //    yaxis: { 
            //        title: 'Projected number of daily deaths',
            //        //domain: [0,.4],
            //    },
                //margin: marginSizing(),
    
                //annotations: annotations(),
            //};
        
            var config = {responsive: true};
        
            Plotly.newPlot(
                plotDiv, 
                myData,
                layout,
                config,
            );
        
        });
};