function unpack(data, locale, columnName, startIndex, plotColumn) {
    // extract the specific locale
    temp = filterLocale(data, locale);

    if (startIndex != -1)
    {
        temp.splice(0, startIndex);
    }

    if (plotColumn !== null)
    {
        temp = temp.filter(function(row) {
            if (row[plotColumn] != 'NA')
            {
                return row;
            }
        });
    }

    // return the column of interest
    return temp.map(function(row) {
        return row[columnName]
    });
}

function getStart(data, locale, columnName)
{
    temp = filterLocale(data, locale);
    
    return temp.findIndex(e => e[columnName] != 'NA');
}

function filterLocale(data, locale)
{
    temp = data.filter(function(row) {
        if (row["place"] == locale)
        {
            return row;
        }
    });

    return temp;
}