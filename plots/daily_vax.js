function makeDailyVaxPlot(externalData, plotDiv, locale) {
    Plotly.d3.csv(externalData, function(data)
        { 
            var start = getStart(data, locale, "daily_doses");

            var trace = {
                x: unpack(data, locale, "date", start),
                y: unpack(data, locale, "daily_doses", start),
                // text: hoverText,
                type: 'bar',
                marker: {
                    color: '#138808',
                },
                // hoverinfo: 'text',
            };
        
            var layout = {
                title: title(),
                yaxis: {
                    title: {
                        text: 'Number of Vaccines'
                    },
                },
                showlegend: false,
                hovermode: 'closest',
                margin: margin(),
            };
        
            var data = [trace];
        
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
