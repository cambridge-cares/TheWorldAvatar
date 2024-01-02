# TWA-VF: CReDo specific features

This page documents the advanced features of the TWA-VF specific for the [CReDo project](https://digitaltwinhub.co.uk/credo). It is intended that these features be updated to become generic features within the TWA, usable on any project, but currently are only available as part of CReDo.

## Scenario handling

Whilst the generic TWA-VF does support "parallel-world" scenarios, this is based on the current implementation within CReDo; the generic TWA backend does not support the scenario system at all. To enable the scenario selection features within the TWA-VF, the following parameters need to be added to the visualisation's `index.html` file.

```json
    "scenarioAgent": "https://public-url-to-centralstackagent",
    "scenarioDataset": "dataset-name-eg-power-or-water-or-healthcare"
```

Additionally, the visualisation's `index.html` may need updates to add the HTML control for changing scenarios, and some JS for triggering the scenario selection screen. The two snippets of required code are shown below, but can also be seen within the example visualisations.

HTML adding scenario control button:
```html
<!-- Scenario change control -->
<div id="scenarioChangeContainer" class="controlBlock expanded" onclick="manager.showScenarioSelector()">
    <div class="tooltip">
        <p><b>Change scenario</b></p>
        <span class="tooltiptext right" style="width: 225px !important;">Change the current scenario</span>
        <div id="currentScenarioName" style="font-style: italic; font-size: 8.5pt;"></div>
    </div>
</div>
```

Javascript for detecting scenario support and triggering selection (if settings are enabled):
```js
// Check if the settings contains a scenarios endpoint
let scenariosEnabled = manager.checkForScenarios();
if(scenariosEnabled) {
    // Show the scenario selection pane
    manager.showScenarioSelector();

} else {
    // Plot the default visible data
    manager.plotData();
}
```

## Getting metadata through the FIA

The CReDo project setup also includes an agent (the CReDoAccessAgent) that performs a similar function to the FeatureInfoAgent (FIA) but adds support for scenarios. Because of this it requires a different URL format when querying for meta & time data. To force the TWA-VF to use this format, rather than the default FIA format, the following parameter needs to be added to the visualisation's `settings.json` file.

Note: a feature request has been submitted on the CReDo project to update this URL route/format to better match that of the FIA, removing the need for this distinction. 

```json
    "credo": true
```