/**
 * This class handles reading and storing global visualisation settings.
 */
class Settings {

    /**
     * JSON settings
     */
    private settings: Object;

    /**
     * Load global settings.
     * 
     * @param settings location of the data.json file
     * 
     * @returns Promise that fulfills when all loading is complete
     */
     public loadSettings(settingsFile: string) {
        console.log("Reading settings file at: "+ settingsFile);

        let self = this;
        return $.getJSON(settingsFile, function(json) {
            self.settings = json;

            if(self.settings["imagery"] == null) {
                switch(Manager.PROVIDER) {
                    case MapProvider.MAPBOX:
                        MapboxUtils.generateDefaultImagery();
                    break;

                    case MapProvider.CESIUM:
                        CesiumUtils.generateDefaultImagery();
                    break;
                }
            }
        }).fail((error) => {
             throw error;
        });    
    }

    /**
     * Returns the setting value for the input key.
     * 
     * @param key setting key
     * @returns value (or null)
     */
    public getSetting(key: string) {
        if(this.settings != null) {
            return this.settings[key];
        }
        return null;
    }

    /**
     * Overrides or adds a new setting
     */
    public putSetting(key: string, value: Object) {
        this.settings[key] = value;
    }

}