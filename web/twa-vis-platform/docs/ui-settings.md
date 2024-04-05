# Configuration: UI Settings

The `config/ui-settings.json` file, which should be present within the `uploads` directory, provides UI settings for the visualisation. This includes settings such as whether landing pages are provided, any custom branding logos etc.

It is intended that this file (along with other configuration files) are provided by the deploying developer via Docker volumes created with the standard TWA Stack. As such, there may be no off-the-shelf example file committed to this repository. 


## Format

Below is an example of the contents for a valid `ui-settings.json` file with additional comments explaining each entry. The format of the file should be consistent whether implementing mapbox or cesium maps.

> [!NOTE]  
> The comments seen below are for explanation purposes only, they are not valid JSON. If wishing to use this content in production, remove the comments first.

```json
{
    "branding": {
        "logo": "/images/whatever.svg", // Custom branding logo
        "toolbarLogo": "/images/defaults/toolbar-logo.svg"  // Custom logo for the toolbar (should be 5:1 aspect ratio)
    },
    "modules": {
        "landing": true,  // Should the landing page be enabled
        "help": true,     // Should the help page be enabled
        "map": true,      // Should the map page be enabled
        "dashboard": true // Should the dashboard page be enabled
    }
}

```
