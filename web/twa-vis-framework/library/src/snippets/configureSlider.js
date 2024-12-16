// Put this script somewhere in your index to add slider actions if the scenario enpoint returns a dimension

// Create a new manager instance
var manager = new Manager(MapProvider.MAPBOX);
window.manager = manager;

const timeSliderActions = function (e) {
    let value = e.target.value;
    manager.loadDefinitionsFromURL(manager.scenarioHandler.getDataURL(), value).then(() => manager.plotData());
    window.currentTimeIndex = value;
    console.log('changing label to value index ', value);
    manager.sliderHandler.changeLabel(Object.values(manager.sliderHandler.scenarioTimesData)[0], value - 1); // first array in the time dimensions list
    manager.showFeature(window.currentFeature, window.currentFeature["properties"]);
};

const sliderActionsArray = [timeSliderActions] // and actions for other sliders if they existed

manager.setSliderActionsArray(sliderActionsArray);