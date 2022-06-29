function makeTVRPlot(externalData, tvrDiv, locale) {
    Plotly.d3.csv(externalData, function(data)
        { 
            var start = getStart(data, locale, "r")

            var trace = {
                x: unpack(data, locale, "date", start, "r"),
                y: unpack(data, locale, "r", start, "r"),
                text: unpack(data, locale, "r_text", start, "r"),
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
        
            var upper = {
                x: unpack(data, locale, "date", start, "r"),
                y: unpack(data, locale, "r_upper", start, "r"),
                type: 'scatter',
                line: {
                    color: '#36a30b',
                    width: 0,
                    opacity: 0.2,
                },  
                hoverinfo: 'skip', 
            }
        
            var lower = {
                x: unpack(data, locale, "date", start, "r"),
                y: unpack(data, locale, "r_lower", start, "r"),
                type: 'scatter',
                fill: 'tonexty',
                line: {
                    color: '#36a30b',
                    width: 0,
                    opacity: 0.2,
                },
                hoverinfo: 'skip',
            }
        
            var layout = {
                title: title(),
                yaxis: {
                    title: {
                        text: 'R(t)'
                    },
                },
                hovermode:'closest',
                shapes: [{
                    type: 'line',
                    xref: 'paper',
                    x0: 0,
                    y0: 1,
                    x1: 1,
                    y1: 1,
                    opacity: .5,
                    line: {
                      color: 'ff9933',
                    }
                }],
                showlegend: false,
                margin: margin(),
            };
        
            var data = [upper, lower, trace]
        
            var config = {responsive: true};
        
            Plotly.newPlot(
                tvrDiv, 
                data,
                layout,
                config,
            );
        } 
    );    
};