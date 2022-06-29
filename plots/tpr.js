function makeTPRPlot(externalData, tprDiv, locale) {
    Plotly.d3.csv(externalData, function(data)
        {
            var trace = {
                x: unpack(data, locale, "date"),
                y: unpack(data, locale, "tpr"),
                text: unpack(data, locale, "tpr_text"),
                type: 'scatter',
                mode: 'lines+markers',
                marker: {
                    color: '#262626',
                    size: 4,
                    symbol: 3,
                },
                line: {
                    color: '#36a30b',
                    width: 5,
                },
                hoverinfo: 'text',
            };
        
        
            var layout = {
                title: title(),
                yaxis: {
                    title: {
                        text: 'Test positive rate'
                    },
                },
                hovermode:'closest',
                showlegend: false,
                margin: margin(),
            };
        
            var data = [trace]
        
            var config = {responsive: true};
        
            Plotly.newPlot(
                tprDiv, 
                data,
                layout,
                config,
            );
        } 
    );
};