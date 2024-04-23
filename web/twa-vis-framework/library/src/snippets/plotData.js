// Once the underlying style has loaded...
MapHandler.MAP.on("style.load", function () {
    // Load registered images and linked files
    manager.loadImagesAndLinks().then(() => {
        buildLegend();
        buildTimeControls();

        // Check if the settings contains a scenarios endpoint
        let scenariosEnabled = manager.checkForScenarios();

        if (scenariosEnabled) {
            manager.showScenarioSelector();
        } else {
            // Plot the default visible data
            manager.plotData();
        }
    });
});