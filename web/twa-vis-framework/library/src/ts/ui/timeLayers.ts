let scenarioTimesData: any;
let manager: any;

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

function fetchScenarioTimes(agentBaseURL: string, scenarioID: string): void {
    fetch(`${agentBaseURL}/getScenarioTimes/${scenarioID}`)
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

function populateSlider(): void {
    const firstIndex = scenarioTimesData.dim_time_index[0].value;
    const lastIndex = scenarioTimesData.dim_time_index[scenarioTimesData.dim_time_index.length - 1].value;
    const firstLabel = scenarioTimesData.dim_time_index[0].label;
    const lastLabel = scenarioTimesData.dim_time_index[scenarioTimesData.dim_time_index.length - 1].label;
    console.log(firstIndex, firstLabel, lastIndex, lastLabel)

    const rangeSlider = document.getElementById('range-slider') as HTMLInputElement;
    rangeSlider.min = firstIndex;
    rangeSlider.max = lastIndex;

    const startTime = document.getElementById('sliderStartTime') as HTMLElement;
    const endTime = document.getElementById('sliderEndTime') as HTMLElement;
    startTime.innerHTML = firstLabel;
    endTime.innerHTML = lastLabel;
}

function changeLabel(currentTimeIndex: number): void {
    const label = scenarioTimesData.dim_time_index[currentTimeIndex].label
    const sliderLabel = document.getElementById('timeSliderLabel') as HTMLElement;
    sliderLabel.innerHTML = label;
}

window.addEventListener('load', function () {

    const slider = document.getElementById('range-slider') as HTMLInputElement;

    slider.addEventListener('input', function (e) {

        let value = e.target.value;
        console.log(value);
        manager.loadDefinitionsFromURL(manager.scenarioHandler.getDataURL(), value).then(() => manager.plotData());
        window.currentTimeIndex = value;
        console.log('changing label');
        changeLabel(Number(value) - 1);
        manager.showFeature(window.currentFeature, window.currentFeature["properties"]);
    });
});
