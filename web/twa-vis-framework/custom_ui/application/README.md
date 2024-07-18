# Custom TWA VF Application
This directory contains various `index.html` for different applications based on [The World Avatar Visualisation Framework](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework).

## 1. Development
This directory has been split into different subdirectories for each application. Please follow the format when developing the custom UI component for new applications. 

At the moment, the available applications are:

1) asset_display
To display asset datasheets within the VF alongside the metadata. It is intended to create a more convenient and seamless user experience to read data from multiple sources in one interface.

2) search_entity
To search for specific urban entities based on user inputs. It is currently only available for Mapbox, and work for Cesium is in development.

### 1.1 Component File Reference
Add the following lines in the `index.html` to reference the generated component css and js files. Please adjust the directory paths depending on the actual location for your needs. One suggestion is to keep it within the same `twa-vf` directory as the `twa-vf` css and js files to keep this simple and organised.

```
<script src="./twa-vf/component.min.js" charset="utf-8"></script>
<link href="./twa-vf/component.min.css" rel="stylesheet" />
```

### 1.2 Component usage
In general, the workflow for adding new UI components is to **(1)** retrieve a parent element, **(2)** initialise your component with a constructor, and **(3)** use the `render` method. You will need to add this code directly to the `<script>` section of the `index.html`.

At the moment, the parent HTML element may be in the following states:
1) Non-existent
If you require a new parent element, please add the HTML element directly to the `<body>` of the document. You may also require styling this element in the css file.

2) Always available
Developers can directly retrieve this with either the `document.getElementById()` or `document.getElementByClassName()` methods.

3) Available during specific events
This category of elements may be added or removed dynamically when loaded. You will require the [mutation observer API](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver) to monitor the HTML DOM tree for changes. When the parent HTML element is added, the custom component can then be added to the VF. Please see the `asset_display` subdirectory for a sample.

## 2. Deployment
Please place the corresponding `index.html` alongside the generated `component.min.js` and `component.min.css` from the `component` directory into your web server to visualise the updated VF.