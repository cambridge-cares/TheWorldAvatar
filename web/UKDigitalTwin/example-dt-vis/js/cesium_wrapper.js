class CesiumWrapper {

    _viewer;

    _layers;

    _allSources;

    constructor(containerName) {

        this._viewer = new Cesium.Viewer(containerName, {
            imageryProvider: Cesium.createWorldImagery({
                style: Cesium.IonWorldImageryStyle.ROAD
            }),
            timeline: false,
            animation: false
        });

        this._layers = new Map();

        // Note that the data source cache is keyed by data path, not by name,
        // since data source names can collide between groups.
        this._allSources = new Map();

    }

    setStyle(style) {
        // TODO: implement?
    }

    getStyle() {
        return {
            layers: this._layers
        }
    }

    addLayer(layer) {
        this._layers.set(layer.id, layer);
    }

    removeLayer(layerID) {
        this._layers.delete(layerID);
    }

    getLayer(layerID) {
        return this._layers.get(layerID);
    }

    setLayoutProperty(layerID, property, value) {
        let layer = this._layers.get(layerID);
        layer.layout[property] = value;
        if(property == "visibility") {
            this._viewer.dataSources.getByName(layer.source)[0].show = value == "visible";
        }
    }

    on(event, callback) {
        if (event === "style.load") {
            func();
        } else if (event==="mousemove" || event ==="mouseleave" || event=="mouseenter" || event=="click"){
            this._viewer.canvas.addEventListener(event, callback);
        } else {
            console.warn("Event \"" + event + "\" not supported for Cesium viewer.");
        }
    }

    flyTo(options) {
        // Might be able to implement smooth flight? But Cesium's lookAt behaves differently from its flyTo;
        // the latter sets your aerial position and looks from that, while the former of choosing a ground
        // location you are looking at and sets what distance and angle you are looking at it from.
        this.jumpTo(options);
    }

    jumpTo(options) {
        this._viewer.camera.lookAt(
            Cesium.Cartesian3.fromDegrees(
                options.center[0],
                options.center[1]
            ),
            new Cesium.HeadingPitchRange(
                Cesium.Math.toRadians(options.bearing),
                Cesium.Math.toRadians(options.pitch-90),
                75000 / options.zoom
            )
        );
        // Reset the camera movement behaviour, which is weird after camera.lookAt.
        this._viewer.camera.lookAtTransform(Cesium.Matrix4.IDENTITY);
    }

    addSource(name, options) {
        if (options.type != "geojson") {
            console.error("Non-GeoJSON sources not supported for Cesium viewer: " + options.location);
            return;
        }
        if (this._allSources.has(options.data)) {
            this._viewer.dataSources.add(this._allSources.get(options.data));
        } else {
            Cesium.GeoJsonDataSource.load(options.data).then((dataSource) => {
                dataSource.name = name;
                this._allSources.set(options.data, dataSource);
                this._viewer.dataSources.add(this._allSources.get(options.data));
                for (var entity of dataSource.entities.values) {
                    // Normally we should be able to just do entities[i].properties.PropertyName, but the normal property keys are not valid JavaScript names.
                    if(entity.polygon != null) {
                        // Determine color
                        let props = entity.properties;
                        let color = props["fill-extrusion-color"] ?? props["fill-color"] ?? "#666666";
                        let outlineColor = props["circle-stroke-color"] ?? props["fill-outline-color"] ?? "#000000";
                        entity.polygon.material = Cesium.Color.fromCssColorString(color.valueOf());
                        entity.polygon.outlineColor = Color.fromCssColorString(outlineColor.valueOf());
                    }
                }
            }).otherwise(function (error) {
                console.log(error);
            });
        }
    }

    getSource(name) {
        return this._viewer.dataSources.getByName(name);
    }

    removeSource(name) {
        console.log(name);
        return this._viewer.dataSources.remove(this._viewer.dataSources.getByName(name)[0]);
    }

    getCenter() {
        return {
            lng: Cesium.Math.toDegrees(Cesium.Cartographic.fromCartesian(this._viewer.camera.position).longitude),
            lat: Cesium.Math.toDegrees(Cesium.Cartographic.fromCartesian(this._viewer.camera.position).latitude)
        };
    }

    getZoom() {
        // Not really well-defined for Cesium viewer
        return 0;
    }

    getPitch() {
        return Cesium.Math.toDegrees(this._viewer.camera.pitch);
    }

    getBearing() {
        return Cesium.Math.toDegrees(this._viewer.camera.heading);
    }

    loadImage(url, func) {
        // TODO: implement
        return func(null, null);
    }

    addImage(imageName, image) {
        return null;
    }

    setPaintProperty(layerID, name, value) {
        // TODO: implement
    }

    getCanvas() {
        return viewer.canvas;
    }

    /**
     * 
     * @param {(x,y)} point point to query
     * @returns {Feature[]} features in line of mouse pointer, from back to front
     */
    queryRenderedFeatures(point) {

    }


}