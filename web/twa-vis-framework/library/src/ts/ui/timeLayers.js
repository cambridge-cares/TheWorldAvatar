///////////////////
// Inside start function
if (scenariosEnabled) {
    manager.showScenarioSelector();
    // Poll until selectScenarioResolved becomes true
    const intervalId = setInterval(() => {
        if (manager.scenarioHandler.selectScenarioResolved) {
            clearInterval(intervalId); // Stop polling
            fetchScenarioTimes(manager.scenarioHandler.agentBaseURL, manager.scenarioHandler.selectedScenario);
        }
    }, 100);
} else {
    // Plot the default visible data
    manager.plotData();
}
///////////////////
function fetchScenarioTimes(agentBaseURL, scenarioID) {
    fetch(agentBaseURL + /getScenarioTimes/ + scenarioID)
        .then(response => response.json())
        .then(data => {
            scenarioTimesData = data;
            populateSlider();
            changeLabel(0);
        })
        .catch(error => {
            console.error('Error fetching times from CentralStackAgent/getScenarioTimes:', error);
        });
}

function populateSlider() {
    const firstIndex = scenarioTimesData.dim_time_index[0].value;
    const lastIndex = scenarioTimesData.dim_time_index[scenarioTimesData.dim_time_index.length - 1].value;
    const firstLabel = scenarioTimesData.dim_time_index[0].label;
    const lastLabel = scenarioTimesData.dim_time_index[scenarioTimesData.dim_time_index.length - 1].label;
    console.log(firstIndex, firstLabel, lastIndex, lastLabel)

    const rangeSlider = document.getElementById('range-slider');
    rangeSlider.min = firstIndex;
    rangeSlider.max = lastIndex;

    const startTime = document.getElementById('sliderStartTime');
    const endTime = document.getElementById('sliderEndTime');
    startTime.innerHTML = firstLabel;
    endTime.innerHTML = lastLabel;
}

function changeLabel(currentTimeIndex) {
    const label = scenarioTimesData.dim_time_index[currentTimeIndex].label
    const sliderLabel = document.getElementById('timeSliderLabel')
    sliderLabel.innerHTML = label;
}

window.addEventListener('load', function () {

    const slider = document.getElementById('range-slider');

    slider.addEventListener('input', function (e) {

        let value = e.target.value;
        console.log(value);
        manager.loadDefinitionsFromURL(manager.scenarioHandler.getDataURL(), value).then(() => manager.plotData());
        window.currentTimeIndex = value;
        console.log('changing label');
        changeLabel(value - 1);
        manager.showFeature(window.currentFeature, window.currentFeature["properties"]);
    });
});