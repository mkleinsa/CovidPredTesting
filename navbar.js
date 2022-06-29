// add state options to menu
dataURL = "https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/package-data/processed/new_everything.csv"
statesList = [];
Plotly.d3.csv(dataURL, function(data) {

    for (var i = 0; i < data.length; i++) {
        if (!statesList.includes(data[i]["place"]) && data[i]["place"] != "India")
        {
            statesList.push(data[i]["place"]);
        }
    }

    statesList.sort()

    for (var i = 0; i < statesList.length; i++)
    {
        state = document.createElement("a");
        state.innerHTML = statesList[i];
        state.setAttribute("id", statesList[i]);
        state.setAttribute("class", "inactive-state")
        document.getElementById("dropdown-content").appendChild(state);
    }
})

//----------------------------------------------------------------------------------------------------//

// page changer
document.addEventListener("click", function(e) {
    // toggle state nav bar appearance
    if (e.target.id == "States")
    {
        toggleMenu();
    }
    // if non-active menu button (besides states) is pressed ***NEED TO IMPLEMENT NON_ACTIVE PART
    else if (e.target.parentNode.parentNode.tagName == "MENU" && e.target.classList.contains("inactive"))
    {
        siteBuildUp(e.target.id);

        // if states dropdown is open, close it
        if (document.getElementById("dropdown-content").classList.contains("dropdown-active"))
        {
            toggleMenu();
        }
    }
    // specific state pressed
    else if (e.target.parentNode.id == "dropdown-content" && e.target.classList.contains("inactive-state"))
    {
        siteBuildUp(e.target.id)
        document.getElementById("States").classList.add("active")
        e.target.classList.replace("inactive-state", "active-state");
        toggleMenu();
    }
    // if open and anywhere is clicked
    else if (document.getElementById("dropdown-content").classList.contains("dropdown-active")) {
        toggleMenu();
    }
})

function toggleMenu() {
    document.getElementById("dropdown-content").classList.toggle("dropdown-active");
    document.getElementById("States").classList.toggle("States-active");
}

function siteBuildUp(siteName) {
    siteBreakdown();
    switch(siteName)
    {
        // note: clicking 'states' is handled elsewhere
        case 'National':
            document.getElementById("navbar").firstElementChild.setAttribute("id", "national");
            document.getElementById(siteName).classList.add("active")
            buildNationalSite();
            break;
        case 'Metrics':
            document.getElementById("navbar").firstElementChild.setAttribute("id", "metrics");
            document.getElementById(siteName).classList.add("active")
            buildMetrics();
            break;
        case 'References':
            document.getElementById("navbar").firstElementChild.setAttribute("id", "references");
            document.getElementById(siteName).classList.add("active")
            buildReferences();
            break;
        // case where a state is clicked
        default:
            document.getElementById("navbar").firstElementChild.setAttribute("id", "states");
            buildPlotSite(siteName)
            break;        
    }
}

function siteBreakdown()
{
    // cleanup content elements
    while (document.getElementById("content").firstChild)
    {
        document.getElementById("content").firstChild.remove()
    }

    // toggle page active
    if (document.getElementById("navbar").firstElementChild.id == "national")       
    {
        document.getElementById("National").classList.replace("active", "inactive");
    }
    else if (document.getElementById("navbar").firstElementChild.id == "states")
    {
        document.getElementById("States").classList.replace("active", "inactive");
        for (var elements=document.getElementsByClassName('active-state'), i = 0, l = elements.length; l > i; i++) {
            elements[0].classList.replace('active-state', 'inactive-state');
        }
    }
    else if (document.getElementById("navbar").firstElementChild.id == "metrics")
    {
        document.getElementById("Metrics").classList.replace("active", "inactive");
    }
    else if (document.getElementById("navbar").firstElementChild.id == "references")
    {
        document.getElementById("References").classList.replace("active", "inactive");
    }
}