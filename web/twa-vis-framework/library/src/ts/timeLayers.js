window.addEventListener('load', function () {
    document.getElementById('range-slider').addEventListener('input', function (e) {
        let value = e.target.value;
        console.log(value);
        manager.loadDefinitionsFromURL(manager.scenarioHandler.getDataURL(), value).then(() => manager.plotData());
        window.currentTimeIndex = value;
        manager.showFeature(window.currentFeature);
    });
});