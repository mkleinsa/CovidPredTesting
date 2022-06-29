// set menu id, build national data
window.addEventListener("load", function() {
    document.getElementById("navbar").firstElementChild.setAttribute("id", "national")
    
    document.getElementById("National").classList.add("active")
    document.getElementById("States").classList.add("inactive")
    document.getElementById("Metrics").classList.add("inactive")
    document.getElementById("References").classList.add("inactive")

    buildNationalSite();
})

// builds national page
function buildNationalSite()
{
    content = document.getElementById('content')

    // topmatter
    makeTopMatter()

    // snapshot
    f = document.createElement('iframe')
    snapshotLink = 'https://raw.githack.com/umich-cphds/cov-ind-19-data/master/source_data/package-data/processed/snapshot.html'
    f.setAttribute('src', snapshotLink)
    f.setAttribute('title', 'snapshot')
    f.setAttribute('id', 'snapshot')
    content.appendChild(f)

    // all plots
    buildPlotSite("India")

    // country comparison
    buildCountryComp();

    // country comparison
    buildPredictions();
}

// builds the country comparison graphs on national page
function buildCountryComp()
{
    plots = document.getElementById('plots')

    // country comp
    t = document.createElement('h2');
    t.innerHTML = 'Daily number of COVID-19 cases and deaths'
    plots.appendChild(t);

    s = document.createElement('p');
    s.innerHTML = 'The first figure represents COVID-19 case counts where the x-axis starts on the day when each country passed 100 cases. The second figure represents COVID-19 fatalities where the x-axis starts on the day when each country exceeded 3 fatalities. These axes allow comparison of counts at similar stages of the outbreak. You can click on countries in the legend to add or remove them and you can hover your cursor over the lines to see the exact numerical counts.'
    plots.appendChild(s);

    e = document.createElement('div');
    e.setAttribute('id', "countryComp");
    e.setAttribute('style', "height:800px;margin-bottom:50px;");
    plots.appendChild(e)

    var countryCompData = "https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/package-data/processed/case_death_country_comp_cases_and_deaths.csv";
    var countryCompDiv = document.getElementById("countryComp")
    makeCountryCompPlot(countryCompData, countryCompDiv)
}

// builds the national prediction plots on national page
function buildPredictions()
{
    plots = document.getElementById('plots')

    locale = "India";

    // predictions
    t = document.createElement('h2');
    t.innerHTML = 'SEIR daily new cases'
    plots.appendChild(t);

    s = document.createElement('p');
    s.innerHTML = 'This figure provides the projected number of COVID-19 new cases (orange) and deaths (green) in ' + locale + '. You can hover your cursor over the bar to see the exact numerical counts.'
    plots.appendChild(s);

    e = document.createElement('div');
    e.setAttribute('id', "nat_predictions");
    e.setAttribute('style', "height:800px;margin-bottom:50px;");
    plots.appendChild(e)

    var pred_data = "https://raw.githubusercontent.com/mkleinsa/CovidPredTesting/master/SEIR_predictions.csv"
    var dailyPredplotDiv = document.getElementById("nat_predictions");
    makeDailyPredictionPlot(pred_data, dailyPredplotDiv);

}

// adds dom elements to page to enable plot building
function buildPlotSite(locale)
{
    plots = document.createElement('div')
    plots.setAttribute('id', 'plots')
    document.getElementById('content').appendChild(plots)

    plots = ["daily_barplot", "tvr", "tpr", "daily_vax", "perc_vax"];

    for (var i = 0; i < plots.length; i++)
    {
        t = document.createElement('h2');
        t.setAttribute('id', plots[i] + ' title');
        document.getElementById("plots").appendChild(t);

        s = document.createElement('p');
        s.setAttribute('id', plots[i] + ' subtitle');
        document.getElementById("plots").appendChild(s);

        p = document.createElement('div');
        p.setAttribute('class', "standardPlot");
        p.setAttribute('id', plots[i]);
        document.getElementById("plots").appendChild(p)
    }

    buildPlots(locale);
}

// constructs plots and their titles
function buildPlots(locale) {
    var data = "https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/package-data/processed/new_everything.csv";

    document.getElementById('daily_barplot title').innerHTML = 
        'Daily number of new COVID-19 cases, fatalities and recoveries in ' + locale;
    document.getElementById('daily_barplot subtitle').innerHTML = 
            'This figure provides the number of COVID-19 new cases (yellow), fatalities (red), and recoveries (green) in ' + locale + '. You can hover your cursor over the bar to see the exact numerical counts.';
    var dailyBarplotDiv = document.getElementById("daily_barplot");
    makeDailyBarPlot(data, dailyBarplotDiv, locale);

    document.getElementById('tvr title').innerHTML = 
        'Time-varying R';
    var tvrDiv = document.getElementById("tvr");
    makeTVRPlot(data, tvrDiv, locale);

    document.getElementById('tpr title').innerHTML = 
        'Test positive rate';
    var tprDiv = document.getElementById("tpr");
    makeTPRPlot(data, tprDiv, locale);

    document.getElementById('daily_vax title').innerHTML = 
        'Daily number of COVID-19 vaccines in ' + locale;
    document.getElementById('daily_vax subtitle').innerHTML = 
        'This figure provides the daily number of COVID-19 vaccines (green) in ' + locale + ' since March 15, 2021. You can hover your cursor over the bar to see the exact numerical counts.'
    var vaxDiv = document.getElementById("daily_vax");
    makeDailyVaxPlot(data, vaxDiv, locale)

    document.getElementById('perc_vax title').innerHTML = 
        'Percent of population with one or two doses of COVID-19 vaccine in ' + locale;
    document.getElementById('perc_vax subtitle').innerHTML = 
        'This figure provides the percentage of the population with one dose of the COVID-19 vaccine (grey) or two doses of the COVID-19 vaccine (green) in ' + locale + ' since March 15, 2021. You can hover your cursor over the lines to see the exact numerical counts.'
    var precVaxDiv = document.getElementById("perc_vax");
    makePercVaxPlot(data, precVaxDiv, locale)

}

// builds references page
function buildReferences() {
    makeTopMatter()

    references = document.createElement('div')
    references.setAttribute('class', 'references')
    references.innerHTML = 
    `
        <h4>References</h4>
        <p>
            Estimating the wave 1 and wave 2 infection fatality rates from SARS-CoV-2 in India: <a href="https://bmcresnotes.biomedcentral.com/articles/10.1186/s13104-021-05652-2">Purkayastha et al. 2021</a></p>
        <p>
            On the resurgence of COVID-19 in India: <a href="https://www.medrxiv.org/content/10.1101/2021.06.23.21259405v1">Salvatore et al. 2021</a></p>
        <p>
            COVID-19 Pandemic in India: Through the Lens of Modeling: <a href="https://www.ghspjournal.org/content/9/2/220">Babu et al. 2021</a></p>
        <p>
            Estimating COVID-19 related mortality in India: <a href="https://www.preprints.org/manuscript/202105.0617/v1">Zimmermann et al. 2021</a></p>
        <p>
            India's COVID-19 crisis: a call for international action: <a href="https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)01121-1/fulltext?rss=yes">Kuppalli et al. 2021</a></p>
        <p>
            Incorporating false-negative tests into COVID-19 models: <a href="https://www.nature.com/articles/s41598-021-89127-1">Bhattacharyya et al. 2021</a></p>
        <p>
            Forecasting COVID-19 in India and the effect of lockdowns: <a href="https://hdsr.mitpress.mit.edu/pub/r1qq01kw/release/2">Ray et al. 2020</a></p>
        <p>
            National vs state-level COVID-19 trends: <a href="https://bmjopen.bmj.com/content/10/12/e041778">Salvatore et al. 2020</a></p>
        <p>
            Comparing 5 COVID-19 models in India: <a href="https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-021-06077-9">Purkayastha et al. 2020</a></p>
        <p>
            Extending SEIR model for imperfect testing: <a href="https://doi.org/10.1101/2020.09.24.20200238">Bhaduri et al. 2020</a></p>
        <p>
            Read the report: <a href="https://bit.ly/COV-IND-19_Report">COV-IND-19 Report</a> (direct download link, check downloads folder)</p>
        <p>
            Read our Medium trilogy: <a href="https://medium.com/@covind_19/predictions-and-role-of-interventions-for-covid-19-outbreak-in-india-52903e2544e6">pre-lockdown (March 21)</a>, <a href="https://medium.com/@covind_19/historic-lockdown-prediction-models-to-study-lockdown-effects-and-the-role-of-data-in-the-crisis-a0afeeec5a6">studying lockdown (April 3)</a>, and <a href="https://medium.com/@covind_19/unlocking-the-40-day-national-lockdown-in-india-there-is-no-magic-key-de4e43177cb4">unlocking the lockdown (April 24)</a></p>
        <p>
            Directory of <a href="https://bhramarm.medium.com">all Medium articles</a></p>
        <p>
            COV-IND-19 app <a href="https://github.com/umich-cphds/cov-ind-19">source code repository</a> and <a href="https://github.com/umich-cphds/cov-ind-19-data">data repository</a></p>
        <p>
            R package: <a href="https://github.com/umich-biostatistics/SEIRfansy">SEIRfansy</a></p>
    `
    document.getElementById('content').appendChild(references)
}

// builds metrics page
function buildMetrics() {
    metrics = document.createElement('div')
    metrics.setAttribute('id', 'metrics_page')
    document.getElementById('content').appendChild(metrics)
    
    // metrics
    f = document.createElement('iframe')
    link = 'https://raw.githack.com/umich-cphds/cov-ind-19-data/master/source_data/package-data/raw/metrics_table_full.html'
    f.setAttribute('src', link)
    f.setAttribute('title', 'metrics table')
    f.setAttribute('id', 'metrics')
    f.setAttribute('height', '1277px')
    metrics.appendChild(f)

    imgLink = 'https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/package-data/processed/r_forest_plot.png'
    i = document.createElement('img')
    i.setAttribute('src', imgLink)
    i.setAttribute('style', 'max-width:800px;')
    i.setAttribute('width', '100%')
    metrics.appendChild(i)
}

// builds to top matter
function makeTopMatter() {
    topmatter = document.createElement('div')
    topmatter.setAttribute('class', 'topmatter')

    topmatter.innerHTML = 
    `
        <h3 class = "title">COV-IND-19 Study Group</h3>
        <div class="sidebar-image">
            <img src="https://raw.githubusercontent.com/umich-cphds/cov-ind-19/master/app/www/group_logo.png" height="200" width="200"/>
        </div>
        <div class="main">
            <p class="first-text">
                Welcome to the COV-IND-19 web app. We aim to provide a resource to
                describe the COVID-19 outbreak in India to date as well as prediction
                models under various hypothetical scenarios. The figure and forecasting
                models update as new data becomes available (i.e., at least daily). You
                may download PNG files of each figure by clicking on the camera icon
                when you are hovering within each plot. Please cite our medium article
                and this website in any publication that you use this resource for.
            </p>
            <p>
                The COV-IND-19 study group is comprised of: Maxwell Salvatore,
                Alexander Rix, Michael Kleinsasser, Daniel Barker, Lili Wang, Rupam
                Bhattacharyya, Soumik Purkayastha, Debashree Ray, Shariq Mohammed,
                Aritra Halder, Debraj Bose, Peter Song, Mousumi Banerjee, Veera
                Baladandayuthapani, and Parikshit Ghosh. Led by PI <a href="http://www-personal.umich.edu/~bhramar/">Bhramar Mukherjee</a>.
            </p>
            <p>
                Please direct inquiries to <a href="mailto:mmsalva@umich.edu">Maxwell Salvatore</a>, <a href="mailto:alexrix@umich.edu">Alexander Rix</a>, <a href="mailto:mkleinsa@umich.edu">Michael Kleinsasser</a>, <a href="mailto:evcohen@umich.edu">Evan Cohen</a>, and <a href="mailto:bhramar@umich.edu">Bhramar Mukherjee</a>
            </p>
            <p>
                Feel free to browse the <a href="https://github.com/umich-cphds/covind">source code</a>
                and the underlying <a href="https://github.com/umich-cphds/cov-ind-19-data">data repository</a> 
                that run this app.
            </p>
        </div>
            
        <div class="main-sub1">
            <h4>Sources</h4>
            <p>
                Non-India country-level data source: <a href="https://github.com/CSSEGISandData/COVID-19">JHU CSSE COVID-19 GitHub</a></p>
            <p>
                India National and State / Union Territory data sources:
                    Through October 17, 2021 - <a href="https://www.covid19india.org">covid19india.org</a></p>
            <p>
                After October 17, 2021 - Count data: <a href="https://www.mohfw.gov.in/">Ministry of Health and Family Welfare (MoHFW)</a> Vaccine data: <a href="https://www.cowin.gov.in/">CoWIN</a></p>
            <p>
                R modeling package: <a href="https://github.com/lilywang1988/eSIR">eSIR R package</a></p>
        </div>

        <div class="main-sub2">
            <h4>How to cite COVIND19.org:</h4>
            <p>
                Ray, D., Salvatore, M., Bhattacharyya, R., Wang, L., Du, J., Mohammed, S., … Mukherjee, B. (2020). Predictions, Role of Interventions and Effects of a Historic National Lockdown in India's Response to the the COVID-19 Pandemic: Data Science Call to Arms. <em>Harvard Data Science Review</em>. <a href="https://doi.org/10.1162/99608f92.60e08ed5">https://doi.org/10.1162/99608f92.60e08ed5</a></p>
        </div>
    `
    document.getElementById('content').appendChild(topmatter)
}

//------------------------------------------------------------------//
// handles communication with and resizing of snapshot
window.addEventListener('message', function (e) {
    var frame = document.getElementById('snapshot');
    frame.setAttribute('height', e.data + 20 + 'px')
});

window.addEventListener('resize', function (e) {
    if (document.getElementById('national'))
    {
        iFrame = document.getElementById('snapshot');
        iFrame.contentWindow.postMessage("resize", "*");
    }
});
//------------------------------------------------------------------//
