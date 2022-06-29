function makePercVaxPlot(externalData, plotDiv, locale) {
    Plotly.d3.csv(externalData, function(data)
        {
            var start = getStart(data, locale, "pct_one_dose")

            var oneDoseTrace = {
                x: unpack(data, locale, "date", start, "pct_one_dose"),
                y: unpack(data, locale, "pct_one_dose", start, "pct_one_dose"),
                text: unpack(data, locale, "pct_one_dose_text", start, "pct_one_dose"),
                type: 'scatter',
                mode: 'lines',
                line: {
                    color: '#ff9933',
                    width: 4,
                },
                hoverinfo: 'text',
            };
        
            var twoDoseTrace = {
                x: unpack(data, locale, "date", start, "pct_one_dose"),
                y: unpack(data, locale, "pct_two_doses", start, "pct_one_dose"),
                text: unpack(data, locale, "pct_two_doses_text", start, "pct_one_dose"),
                type: 'scatter',
                mode: 'lines',
                line: {
                    color: '#138808',
                    width: 4,
                },
                hoverinfo: 'text',
            };
        
        
            var layout = {
                title: title(),
                yaxis: {
                    title: {
                        text: 'Percent of population vaccinated'
                    },
                },
                hovermode: 'closest',
                showlegend: false,
                margin: margin(),
            };
        
            var data = [oneDoseTrace, twoDoseTrace]
        
            var config = {responsive: true};
        
            Plotly.newPlot(
                plotDiv, 
                data,
                layout,
                config,
            );
        } 
    );
};