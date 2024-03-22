window.addEventListener('load', function () {

    const slider = document.getElementById('range-slider');

    slider.addEventListener('input', function (e) {
        let value = e.target.value;
        console.log(value);
        manager.loadDefinitionsFromURL(manager.scenarioHandler.getDataURL(), value).then(() => manager.plotData());
        window.currentTimeIndex = value;
        manager.showFeature(window.currentFeature);
    });


    slider.addEventListener('mouseover', function (e) {
        let value = e.target.value;
        changeLabel(manager.scenarioHandler.agentBaseURL, manager.scenarioHandler.selectedScenario, value);
    });

    function populateSlider(agentBaseURL, scenarioID) {
        fetch(agentBaseURL + /getScenarioTimes/ + scenarioID)
            .then(response => response.json())
            .then(data => {
                const firstIndex = data.dim_time_index[0].value;
                const lastIndex = data.dim_time_index[data.dim_time_index.length - 1].value;
                const firstLabel = data.dim_time_index[0].label;
                const lastLabel = data.dim_time_index[data.dim_time_index.length - 1].label;
                console.log(firstIndex, firstLabel, lastIndex, lastLabel)

                slider.min = firstIndex;
                slider.max = lastIndex;

                const startTime = document.getElementById('sliderStartTime');
                const endTime = document.getElementById('sliderEndTime');
                startTime.innerHTML = firstLabel;
                endTime.innerHTML = lastLabel;
            })
            .catch(error => {
                console.error('Error fetching times from CentralStackAgent/getScenarioTimes:', error);
            });
    };

    function changeLabel(agentBaseURL, scenarioID, currentTimeIndex) {
        fetch(agentBaseURL + /getScenarioTimes/ + scenarioID)
            .then(response => response.json())
            .then(data => {
                const label = data.dim_time_index[currentTimeIndex].value
                const sliderLabel = document.getElementById('timeSliderLabel')
                sliderLabel.innerHTML = label;
            });
    };

});