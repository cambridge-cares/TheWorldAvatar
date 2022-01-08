

class CesiumWrapper {

    _viewer;

    // Key: layer name (e.g. "roofs")
    // Value: metadata objects loaded from leaf directory meta.js files in the data.
    _layers;

    // Key: image name (e.g. "alpha")
    // Value: image url
    _images;

    _eventHandler;

    constructor(containerName) {

        this._viewer = new Cesium.Viewer(containerName, {
            imageryProvider: Cesium.createWorldImagery({
                style: Cesium.IonWorldImageryStyle.ROAD
            }),
            timeline: false,
            animation: false
        });

        this._layers = new Map();

        this._images = new Map();

        this._eventHandler = new Cesium.ScreenSpaceEventHandler(this._viewer.scene.canvas);
    }

    setStyle(style) {
        // TODO: implement?
    }

    setTerrain(style) {
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

    moveLayer(layerID) {

    }

    setLayoutProperty(layerID, property, value) {
        let layer = this._layers.get(layerID);
        layer.layout[property] = value;
        if (property == "visibility") {
            this._viewer.dataSources.getByName(layer.source)[0].show = value == "visible";
        }
    }

    on(event, callback) {
        if (event === "style.load") {
            callback();
        } else if (event === "mousemove" || event === "click") {
            this._viewer.canvas.addEventListener(event, (e) => {
                let screenPoint = new Cesium.Cartesian2(e.clientX, e.clientY);
                let cartesian = this._viewer.camera.pickEllipsoid(screenPoint, this._viewer.scene.globe.ellipsoid);
                let cartographic = Cesium.Cartographic.fromCartesian(cartesian);
                callback({
                    point: screenPoint,
                    lngLat: {
                        lng: Cesium.Math.toDegrees(cartographic.longitude),
                        lat: Cesium.Math.toDegrees(cartographic.latitude)
                    }
                });
            });
        } else if (event === "click") {
            this._eventHandler.setInputAction((click) => {
                callback({ point: movement.endPosition });
            }, Cesium.ScreenSpaceEventType.LEFT_CLICK);
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
                Cesium.Math.toRadians(options.pitch - 90),
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
        Cesium.GeoJsonDataSource.load(options.data).then((dataSource) => {
            dataSource.name = name;
            this._viewer.dataSources.add(dataSource);
            for (let entity of dataSource.entities.values) {
                if (entity.polygon) {
                    // This is to support Mapbox property names; Cesium ion also natively supports color specification by "fill" and "stroke".
                    let props = entity.properties;
                    let fillColorHex = props["fill-extrusion-color"] ?? props["fill-color"] ?? props["circle-color"] ?? "#666666";
                    let fillColor = Cesium.Color.fromCssColorString(fillColorHex.valueOf());
                    let fillColorProperty = new Cesium.CallbackProperty((time, result) => {
                        if (entity.state?.hover) {
                            return Cesium.Color.lerp(fillColor, Cesium.Color.WHITE, 0.5, result);
                        } else {
                            return result = fillColor;
                        }
                    }, false);
                    let outlineColorHex = props["fill-outline-color"] ?? props["circle-stroke-color"] ?? "#000000";
                    let outlineColor = Cesium.Color.fromCssColorString(outlineColorHex.valueOf());
                    entity.polygon.material = new Cesium.ColorMaterialProperty(fillColorProperty);
                    entity.polygon.outlineColor = outlineColor;
                } else if (entity.billboard) {
                    if (entity.properties["icon-image"]) {
                        entity.billboard.image = new Cesium.CallbackProperty(
                            (time, result) => result = this._images[entity.properties["icon-image"]].valueOf(),
                            false);
                        // By default it is a pin with the bottom center on the coordinates; this changes
                        // the image to also vertically center on the coordinates, which I believe is the MapBox behaviour.
                        entity.billboard.verticalOrigin = 0;
                    } else if (entity.properties["circle-color"]) {
                        // This is to support Mapbox property names; Cesium ion also natively supports color specification "marker-color".
                        entity.billboard.color = new Cesium.Color.fromCssColorString(entity.properties["circle-color"].valueOf());
                    }
                } else if (entity.polyline && entity.properties["line-color"]) {
                    // This is to support Mapbox property names; Cesium ion also natively supports color specification by "stroke".
                    let color = new Cesium.Color.fromCssColorString(entity.properties["line-color"].valueOf());
                    entity.polyline.material = new Cesium.ColorMaterialProperty(color);
                }
                entity.dataSource = dataSource;
            }
        }).otherwise(function (error) {
            console.log(error);
        });
    }

    getSource(name) {
        return this._viewer.dataSources.getByName(name);
    }

    removeSource(name) {
        return this._viewer.dataSources.remove(this._viewer.dataSources.getByName(name)[0]);
    }

    getCenter() {
        let cartographic = Cesium.Cartographic.fromCartesian(this._viewer.camera.position);
        return {
            lng: Cesium.Math.toDegrees(cartographic.longitude),
            lat: Cesium.Math.toDegrees(cartographic.latitude)
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
        return func(null, url);
    }

    // icon_handler.js *basically* calls
    //   map.loadImage(imageURL, (error, image) => map.addImage(imageName, image));
    // where imageName is the last segment of imageURL.
    // MapBox internally matches imageName against the "icon-image" property of data
    // to render the correct image for each feature. I don't know what MapBox is doing,
    // but in our case we actually just let the URL pass through loadImage, and so our
    // addImage actually takes imageName and *imageURL* as the second argument.
    addImage(imageName, image) {
        this._images[imageName] = image;
    }

    setPaintProperty(layerID, name, value) {
        // TODO: implement
    }

    getCanvas() {
        return this._viewer.canvas;
    }

    /**
     * Queries rendered features in line of a screen point.
     * @param {Cartesian2} point point to query
     * @returns {Feature[]} features in line of mouse pointer, from back to front
     */
    queryRenderedFeatures(point) {
        let feature = this._viewer.scene.pick(point);
        if (feature) {
            return [{
                id: feature.id._id,
                layer: this.getLayer(feature.id.dataSource.name),
                properties: feature.id.properties
            }];
        } else {
            return [];
        }
    }

    setFeatureState(featureSpecification, state) {
        let dataSource = this.getSource(featureSpecification.source)[0];
        let entity = dataSource?.entities.getById(featureSpecification.id);
        if (entity != null) entity.state = state;
    }

}