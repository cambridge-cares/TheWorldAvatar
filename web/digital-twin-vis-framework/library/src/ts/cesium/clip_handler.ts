/**
 * Handles the creation, registration, and settings of clipping planes
 * for 3D tiles set layers.
 */
class ClipHandler {

    /**
     * Clipping plane collections keyed by layer name.
     */
    private static PLANES = {};

    /**
     * Current plane heights keyed by layer name.
     */
    private static HEIGHTS = {};

    /**
     * Visual geometries used to show planes, keyed by layer name.
     */
    private static GEOMETRIES = {};

    /**
     * Currently selected clipping plane.
     */
    private static SELECTED_PLANE;

    /**
     * Dictionary of layer names and keys that
     * have clipping enabled.
     */
    private layers;
    
    /**
     * Slider control.
     */
    private slider;

    /**
     * Minimum, maximum, and starting values (m).
     */
    private minValue;
    private maxValue;
    private startValue;

    /**
     * Custom levels within slider.
     */
    private labels;

    private static OFFSET = 0;

    /**
     * Constructor.
     */
    public ClipHandler(layers) {
        // Empty
    }

    /**
     * 
     * @param layers 
     */
    public setLayers(layers) {
        this.layers = layers;
    }

    /**
     * Adds overall controls to select an individual layer
     * and show/hide clipping plane geometries.
     */
    public addControls() {
        let self = this;

        if(document.getElementById("clipControlContainer") == null) {
            //  Controls not defined in HTML file, generate here

            // New container for controls
            let controlContainer = document.createElement("div");
            controlContainer.id = "clipControlContainer";
            controlContainer.classList.add("controlBlock");
            controlContainer.innerHTML = `
                <div id="controlTitle" class="controlTitle">
                    <p style="width: 100%;">Clipping Planes</p>
                </div>
            `;

            let controlContents = document.createElement("div");
            controlContents.classList.add("controlContents");
            controlContainer.appendChild(controlContents);
            
            let controlParent = document.getElementById("controlContainer");
            let helpIcon = document.getElementById("helpContainer");
            controlParent.insertBefore(controlContainer, helpIcon);

            // Enable clipping
            let checkContainer = document.createElement("div");
            checkContainer.id = "clipEnableContainer";
            controlContents.appendChild(checkContainer);

            let enableLabel = document.createElement("p");
            enableLabel.innerHTML = "Enable clipping planes?";
            checkContainer.appendChild(enableLabel);

            let enableCheck = document.createElement("input");
            enableCheck.type = "checkbox";
            enableCheck.id = "clipEnableCheck"
            checkContainer.appendChild(enableCheck);

            enableCheck.addEventListener("change", function() {
                self.onEnableChange(enableCheck.checked);
            });

            // Show geometry
            let showContainer = document.createElement("div");
            showContainer.id = "clipShowContainer";
            showContainer.style.display = "none";
            controlContents.appendChild(showContainer);

            let showLabel = document.createElement("p");
            showLabel.innerHTML = "Show plane geometry?";
            showContainer.appendChild(showLabel);

            let showCheck = document.createElement("input");
            showCheck.type = "checkbox";
            showCheck.checked = true;
            showCheck.id = "clipShowCheck"
            showContainer.appendChild(showCheck);

            showCheck.addEventListener("change", function() {
                self.onShowGeometryChange(showCheck.checked);
            });

            // Select layer
            let selectContainer = document.createElement("div");
            selectContainer.id = "clipSelectContainer";
            selectContainer.style.display = "none";
            controlContents.appendChild(selectContainer);

            let selectLabel = document.createElement("p");
            selectLabel.innerHTML = "Target layer:";
            selectContainer.appendChild(selectLabel);

            let selectCombo = document.createElement("select");
            selectCombo.id = "clipSelectCombo"
            selectContainer.appendChild(selectCombo);

            let placeholder = document.createElement("option");
            placeholder.selected = true;
            placeholder.disabled = true;
            placeholder.hidden = true;
            placeholder.text = "Please select a layer...";
            selectCombo.appendChild(placeholder);

            selectCombo.addEventListener("change", function() {
                self.onTargetLayerChange(selectCombo.value);
            });

        } else {
            // Controls already defined in HTML, just add listeners
            let enableCheck = document.getElementById("clipEnableCheck") as HTMLInputElement;
            enableCheck.addEventListener("change", function() {
                self.onEnableChange(enableCheck.checked);
            });

            let showCheck = document.getElementById("clipShowCheck") as HTMLInputElement;
            showCheck.addEventListener("change", function() {
                self.onShowGeometryChange(showCheck.checked);
            });

            let selectCombo = document.getElementById("clipSelectCombo") as HTMLSelectElement;
            selectCombo.addEventListener("change", function() {
                self.onTargetLayerChange(selectCombo.value);
            });
        }

        // Populate selection drop down with list of compatible layers
        let selectCombo = document.getElementById("clipSelectCombo") as HTMLSelectElement;
        for(let name in this.layers) {
            let option = document.createElement("option");
            option.value = this.layers[name];
            option.text = name;
            selectCombo.appendChild(option);
        }
    }

    /**
     * Fires when the "Enable clipping planes?" check is changed.
     * 
     * @param {Boolean} value should planes be enabled.
     */
    private onEnableChange(value) {
        let showContainer = document.getElementById("clipShowContainer");
        let selectContainer = document.getElementById("clipSelectContainer");
        let selectCombo = document.getElementById("clipSelectCombo") as HTMLSelectElement;

        if(showContainer != null) showContainer.style.display = (value) ? "block" : "none";
        if(selectContainer != null) selectContainer.style.display = (value) ? "block" : "none";

        if(!value) {
            // Remove slider control
            let sliderParent = document.getElementById("sliderParent");
            document.body.removeChild(sliderParent);

            // Turn off clipping plane
            let layerID = ClipHandler.SELECTED_PLANE.id;
            let tileset = CesiumUtils.getPrimitive(layerID);
            if(tileset?.clippingPlanes != null) {

                // Disable clipping planes
                tileset.clippingPlanes.enabled = false;

                // Turn off plane geometry
                let plane = ClipHandler.GEOMETRIES[layerID].plane;
                plane.show = false;
            } 

            // Clear selection
            ClipHandler.SELECTED_PLANE = null;

        } else {
            let showCombo = document.getElementById("clipSelectCombo") as HTMLSelectElement;

            if(showCombo.selectedIndex !== 0) {
                // If there's already a selection
                this.onTargetLayerChange(selectCombo.value);

            } else if($("#clipSelectCombo option").length === 2){
                // If there's only one valid option
                showCombo.selectedIndex = 1;
                this.onTargetLayerChange(selectCombo.value);
            }
        }
    }

    /**
     * Fires when the "Show plane geometry?" check is changed.
     * 
     * @param {Boolean} value should geometry be visible 
     */
    private onShowGeometryChange(value) {
         if(ClipHandler.SELECTED_PLANE != null) {
            let layerID = ClipHandler.SELECTED_PLANE.id;
            let plane = ClipHandler.GEOMETRIES[layerID].plane;
            plane.show = value;
        }
    }

    /**
     * Changes the target layer for clipping plane functionality.
     * 
     * @param layerIDs single string with all layer IDs behind the selected
     * layer name (separated by '|' character).
     */
    private onTargetLayerChange(layerIDs) {
        console.log("Changing target layers for clipping planes to: " + layerIDs);

        // Turn clipping plane off on old layer (if needed)
        if(ClipHandler.SELECTED_PLANE != null) {
            // Turn off clipping plane
            let layerID = ClipHandler.SELECTED_PLANE.id;
            let tileset = CesiumUtils.getPrimitive(layerID);
            if(tileset?.clippingPlanes != null) {
                tileset.clippingPlanes.enabled = false;
                ClipHandler.GEOMETRIES[layerID].plane.show = false;
            } 
        }

        let found = false;

        // Iterate through linked layer IDs to
        // find root layer with clipping enabled
        let splitLayers = (layerIDs.includes("|")) ? layerIDs.split("|") : [layerIDs];
        splitLayers.forEach(layerID => {

            let layerObj = Manager.DATA_STORE.getLayerWithID(layerID);

            if(!found && layerObj != null && layerObj.definition.hasOwnProperty("clipping")) {

                // Check if the tileset itself has a height offset
                let sourceObj = layerObj.source;
                if("position" in sourceObj.definition) {
                    ClipHandler.OFFSET = sourceObj.definition["position"][2];
                } else {
                    ClipHandler.OFFSET = 0;
                }

                // Calculate real start height for slider
                let startHeight =  layerObj.definition["clipping"]["start"];
                if(ClipHandler.PLANES[layerID] != null) {
                    startHeight = ClipHandler.PLANES[layerID].get(0).distance;
                    startHeight += ClipHandler.OFFSET;
                }

                // Build a new slider component
                this.createSlider(
                    layerObj.definition["clipping"]["min"],
                    layerObj.definition["clipping"]["max"],
                    startHeight,
                    layerObj.definition["clipping"]["labels"]
                );

                // Cache labels
                this.labels = layerObj.definition["clipping"]["labels"];

                // Link the pre-created planes to the tileset
                let tileset = CesiumUtils.getPrimitive(layerID);
                let planeCollection = ClipHandler.PLANES[layerID];

                if(tileset["clippingPlanes"] == null) {
                    tileset["clippingPlanes"] = planeCollection;
                } else {
                    tileset["clippingPlanes"].enabled = true;
                }

                // Link clipping plane to controls (if not done)
                if(ClipHandler.GEOMETRIES[layerID] == null) {
                    this.linkClippingPlane(layerID, tileset, layerObj.definition["clipping"]);
                    ClipHandler.SELECTED_PLANE = ClipHandler.PLANES[layerID].get(0);
                } else {
                    ClipHandler.SELECTED_PLANE = ClipHandler.PLANES[layerID].get(0);

                    let showCombo = document.getElementById("clipShowCheck") as HTMLInputElement;
                    ClipHandler.GEOMETRIES[layerID].plane.show = showCombo.checked;
                }
                found = true;
            }
        });
    }

    /**
     * Initialises and registers a clippling plane for the
     * input tileset layer.
     * 
     * @param layerID ID of layer containing plane
     * @param defaultHeight default plane height (m)
     */
    public initialiseClippingPlane(layerID, defaultHeight) {

        // Check if the tileset itself has a height offset
        let layerObj  = Manager.DATA_STORE.getLayerWithID(layerID);
        let sourceObj = layerObj.source;
        if("position" in sourceObj.definition) {
            defaultHeight -= sourceObj.definition["position"][2];
        }

        // New clipping plane
        let plane = new Cesium.ClippingPlane(
            new Cesium.Cartesian3(0.0, 0.0, -1.0),
            defaultHeight
        );
        plane.id = layerID;

        // New clipping plane collection
        let planeCollection = new Cesium.ClippingPlaneCollection({
            planes: [ plane ],            
            edgeWidth: 0.0
        });

        // Register plane collection
        ClipHandler.PLANES[layerID] = planeCollection;

        // Return the plane collection
        return planeCollection;
    }

    /**
     * Link an existing clipping plane to a newly created
     * 3D tileset object.
     * 
     * @param layerID ID of layer containing plane
     * @param tileset target tileset 
     * @param settings clip plane settings
     */
    public linkClippingPlane(layerID, tileset, settings) {
        let planeCollection = ClipHandler.PLANES[layerID];
        let clipPlane = planeCollection.get(0);

        let defaultHeight = settings["start"];

        // Once the tileset is loaded onto the map
        tileset.readyPromise.then(function() {
            let boundingSphere = tileset.boundingSphere;
            let radius = boundingSphere.radius;

            // The plane is initially positioned at the tileset's root transform. Apply an 
            // additional matrix to center the clipping plane on the bounding sphere center.
            if (!Cesium.Matrix4.equals(tileset.root.transform, Cesium.Matrix4.IDENTITY)) {
                planeCollection.modelMatrix = Cesium.Matrix4.fromTranslation(
                    new Cesium.Cartesian3(0.0, 0.0, defaultHeight)
                );
            }

            // Get the position of the tileset, so we can place the clipping plane there.
            let posTileset = tileset.boundingSphere.center;

            // Adjust the height to the default height specified
            let carto = Cesium.Cartographic.fromCartesian(posTileset);
            let surface = Cesium.Cartesian3.fromRadians(
                carto.longitude,
                carto.latitude,
                0.0
            );
            
            let result = Cesium.Matrix4.multiplyByPoint(
                Cesium.Transforms.eastNorthUpToFixedFrame(surface),
                new Cesium.Cartesian3(0, 0, ClipHandler.OFFSET),
                new Cesium.Cartesian3()
            );

            // Rotate to match model
            let layerObj = Manager.DATA_STORE.getLayerWithID(layerID);
            let sourceObj = layerObj.source;
            let orientation = null;
            
            if("rotation" in sourceObj.definition) {
                let setting = sourceObj.definition["rotation"];

                // Apply the tileset's rotation to the clipping plane as well
                let hpr = new Cesium.HeadingPitchRoll(setting[2], setting[1], setting[0]);
                orientation = Cesium.Transforms.headingPitchRollQuaternion(result, hpr);
            }

            // Get initial visibility state
            let showCheck = document.getElementById("clipShowCheck") as HTMLInputElement;
            let showState = (showCheck != null) ? showCheck.checked : true;

            // Add visible 2D planes to represent the functional clipping height
            let planeEntity = MapHandler.MAP.entities.add({
                position: result,
                orientation: orientation,
                id: layerID,
                show: showState,
                plane: {
                    dimensions: new Cesium.Cartesian2(radius * 1.41, radius * 1.41),
                    material: Cesium.Color.LIGHTSKYBLUE.withAlpha(0.33),
                    outline: true,
                    outlineColor: Cesium.Color.LIGHTSKYBLUE.withAlpha(0.66),
                    plane: new Cesium.CallbackProperty(
                        ClipHandler.createPlaneUpdateFunction(clipPlane, layerID),
                        false
                    ),
                    show: showState
                }
            });

            planeEntity.name = "clipping-plane";
            planeEntity.plane.name = "clipping-plane";

            // Store visual entity for plane
            ClipHandler.GEOMETRIES[layerID] = planeEntity;
            return tileset;
        });
    }

    /**
     * 
     */
    private resetSlider() {
        let sliderParent = document.getElementById("sliderParent");
        if(sliderParent != null) {
            document.body.removeChild(sliderParent);
        }
    }

    /**
     * Creates a vertical slider component to control
     * the height (in metres) of the current clipping 
     * plane.
     * 
     * @param minValue minimum height (m)
     * @param maxValue maximum height (m)
     * @param startValue starting height (m)
     * @param labels dictionary keyed by specific heights to add labels at (values are label text)
     */
    public createSlider(minValue, maxValue, startValue, labels) {
        this.minValue = minValue;
        this.maxValue = maxValue;
        this.startValue = startValue;

        // Reset if needed
        this.resetSlider();

        // Container for all slider components
        let sliderParent = document.createElement("div");
        sliderParent.id = "sliderParent";
        sliderParent.classList.add("expanded");
        sliderParent.innerHTML = `
            <div id="sliderTop">
                <div id="sliderLeft">
                </div>
                <div id="sliderRight">
                </div>
            </div>
            <div id="sliderBottom">
                <input id="sliderInput" type="number" step="0.1"></input>
                <p style="text-align: right; float: right;">m</p>
            </div>
        `;
        document.body.appendChild(sliderParent);

        // Build that actual slider control
        this.slider = $("#sliderRight").slider({
            min: minValue,
            max: maxValue,
            value: startValue,
            step: ((maxValue - minValue) / 1000),
            range: "min",
            orientation: "vertical",
            slide: this.updateInput,
            change: this.updateInput
        });

        // Add event to number input
        let input = document.getElementById("sliderInput") as HTMLInputElement;
        input.value = startValue;

        input.addEventListener("change", this.updateSlider);

        // Build and add absolute value labels
        this.addLabels(labels);

        // Listen for resize event on the slider
        let self = this;
        new ResizeObserver(function() {
            self.addLabels(self.labels);
        }).observe(sliderParent);
    }

    /**
     * Generate and add labels for specific heights.
     * 
     * @param labels  dictionary keyed by specific heights to add labels at (values are label text)
     */
    private addLabels(labels) {
        let diff = this.maxValue - this.minValue;
        let sliderHeight = $("#sliderRight").height();
        let sliderLeft = document.getElementById("sliderLeft");

        // Reset content
        sliderLeft.innerHTML = "";

        // Iterate through labels
        for(let key in labels) {
            let value = Number(key);

            // Calculate pixel position of step
            let normalised = (value - this.minValue) / diff;
            let height = normalised * sliderHeight;

            // Build component to show step
            let sliderStep = document.createElement("div");
            sliderStep.id = String(value);
            sliderStep.classList.add("sliderStep");
            sliderStep.style.top = String(sliderHeight - height + 20);
            sliderStep.innerHTML = labels[key];
            sliderLeft.appendChild(sliderStep); 

            // Add click listener
            let self = this;
            sliderStep.addEventListener("click", function() {
                let value = Number(sliderStep.id);
                self.slider.slider("value", value);
            });
        }
    }

    /**
     * Update the value of the numerical input to match
     * the current value of the slider, then moves the plane.
     */
    private updateInput() {
        let input = document.getElementById("sliderInput") as HTMLInputElement;
        let value = Number($("#sliderRight").slider("value")).toFixed(2);

        input.value = String(value);
        ClipHandler.movePlane(Number(input.value) - ClipHandler.OFFSET);
    }

    /**
     * Update the value of slider to match the
     * current value of the number input, then moves the plane.
     */
    private updateSlider() {
        let input = document.getElementById("sliderInput") as HTMLInputElement;
        let value = Number(input.value).toFixed(2);

        $("#sliderRight").slider("value", value);
        ClipHandler.movePlane(Number(value) - ClipHandler.OFFSET);
    }

    /**
     * Move the currently selected clipping plane to the input height.
     * 
     * @param newHeight desired height (m)
     */
    public static movePlane(newHeight) {
        if (Cesium.defined(ClipHandler.SELECTED_PLANE)) {
            let planeID = ClipHandler.SELECTED_PLANE.id._id;

            ClipHandler.HEIGHTS[planeID] = newHeight;
            ClipHandler.SELECTED_PLANE.distance = newHeight;
        } else {
            console.error("Clipping plane for this layer has not been defined.");
        }
    }

    /**
     * Handles the update of a plane's distance.
     * 
     * @param plane plane to update
     * @param tileset ID of tileset plane is attached to
     * 
     * @returns updated plane 
     */
     public static createPlaneUpdateFunction(plane, tilesetID) {
        return function () {
            if (Cesium.defined(ClipHandler.SELECTED_PLANE)) {
                let selectedID = ClipHandler.SELECTED_PLANE.id._id;
                if(tilesetID !== selectedID) return plane;

                let height = ClipHandler.HEIGHTS[tilesetID];
                plane.distance = height;
            }
            return plane;
        };
    }

}
// End of class.