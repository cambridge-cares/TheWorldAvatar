window.addEventListener('load', function () {

    // const layerDefinitions = {};

    // function storeLayerDefinition(layer) {
    //     layerDefinitions[layer.id] = { ...layer.definition };
    // }


    // function getSourceURL(sourceID) {
    //     return MapHandler.MAP.getSource(sourceID)['tiles'][0];
    // }

    // // const workingURL = 
    // function changeSource(sourceURL, sourceID, layerIDs, value) {
    //     let newurl = `${sourceURL}&dim_time_index=${value}`
    //     const type = 'vector'

    //     // Remove the old source
    //     if (MapHandler.MAP.getSource(sourceID)) {
    //         for (let layerID of layerIDs) {
    //             storeLayerDefinition(MapHandler.MAP.getLayer())
    //             MapHandler.MAP.removeLayer(layerID);
    //             console.log(layerID + ' removed');
    //         }
    //         MapHandler.MAP.removeSource(sourceID);
    //         console.log(sourceID + ' removed');
    //     }


    //     MapHandler.MAP.addSource('my-vector-source', {
    //         type: 'vector',
    //         url: 'http://localhost:1798/geoserver/EpHpvNNb/wms?service=WMS&version=1.1.0&request=GetMap&layers=EpHpvNNb%3APower&bbox={bbox-epsg-3857}&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile'
    //     });

    //     for (let layerID of layerIDs) {
    //         MapHandler.MAP.addLayer({
    //             'id': layerID,
    //             'type': type,
    //             'source': sourceID,
    //         });
    //         console.log(layerID + ' added');
    //     }

    //     console.log(sourceID + ' updated\n new url is ' + newurl);
    // }

    // const heatURL = getSourceURL('0.0.heat-source');
    // const powerURL = getSourceURL('0.2.power-connections');

    document.getElementById('range-slider').addEventListener('input', function (e) {
        let value = e.target.value;
        console.log(value);
        manager.loadDefinitionsFromURL(manager.scenarioHandler.getDataURL(), value).then(() => manager.plotData());
        window.currentTimeIndex = value;
        manager.showFeature(window.currentFeature);
        // changeSource(heatURL, '0.0.heat-source', ['0.0.heat-layer'], value);
        // changeSource(powerURL, '0.2.power-connections', ['0.2.power-conn-layer-under', '0.2.power-conn-layer-core', '0.2.power-conn-layer-arrow'], value);
        // all the layers and sources that we want to move
    });
});