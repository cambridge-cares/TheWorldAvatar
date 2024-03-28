MapHandler.MAP.on("style.load", function () {
    // Load registered images and linked files
    manager.loadImagesAndLinks().then(() => {
        // Check if the settings contains a scenarios endpoint
        let scenariosEnabled = manager.checkForScenarios();
        if (scenariosEnabled) {
            manager.showScenarioSelector();
            // Poll until selectScenarioResolved becomes true
            const intervalId = setInterval(() => {
                if (manager.scenarioHandler.selectScenarioResolved) {
                    clearInterval(intervalId); // Stop polling
                    populateSlider(manager.scenarioHandler.agentBaseURL, manager.scenarioHandler.selectedScenario);
                }
            }, 100);
        } else {
            // Plot the default visible data
            manager.plotData();
        }
    });
});