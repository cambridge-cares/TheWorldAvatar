# Custom UI Components
This directory contains the source code and compilation builds to create custom UI components for different applications in [The World Avatar Visualisation Framework](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework). 

## 1. Component Creation
### 1.1 Requirements
Basic knowledge on CSS and Typescript

### 1.2 Typescript Development
#### 1.2.1 Dynamic Component
In the `src` directory, developers can find the core `DynamicComponent` class in the `/ts/dynamic_component.ts`. It is intended not to modify this class. Instead, please develop your application-specific component by extending this class. Once completed, create a separate subdirectory and place this new component into that subdirectory. A sample example is found at `./src/ts/datasheet/asset_display_component.ts`. 

By default, the DynamicComponent generates the following HTML elements:
```
<div id="dynamicInterfaceContainer">
  <div id="dynamicComponentTitle">Custom Title</div>
  <div id="dynamicContent"></div>
</div>
```
When developing their components, developers should take note of the following core methods. 
1) `constructor`
Constructs the core structure of the dynamic component with a custom title from input. This should be called in the child component's constructor as the first line. You may alter these elements in the child component through the `container`, `container_title`, or `container_content` fields.
2) `render`
A method to attach the custom component and its child nodes to an existing HTML element in the TWA VF. This should be called in the `<script>` section of the `index.html`.

The following helper functions are available in `/ts/dynamic_component.utils`:
1) `createHTMLElement`
A helper method to create a new HTML element with your required type, id or classes for changing its display.
2) `createDiv`
A helper method to create a new Div element with your required id or classes for changing its display.

Once ready, please place your component into a distinct subdirectory. This should be similar to `./src/ts/datasheet/asset_display_component.ts`. 

#### 1.2.2 Supplementary Components
There are also supplementary components to support the different applications available in the `shared` subdirectory:

1) `Loader`: A loader element that is fixed to the center of the screen. Can be hidden or shown with the respective methods.

2) `Overlay`: An overlay element that will darken the entire screen when active. Can be hidden or shown with the respective methods.

### 1.3 Styling
Developers can edit the core styling for their needs in `src/css/component.css`. For styling custom components, please add the new classes and ids in the css file before compiling.

## 2. Compilation
This directory has been designed to compile the Typescript and CSS source code through a Docker container. Other compilation workflows are beyond the scope of this document. 

Before compiling, please feel free to delete any of the subdirectory if you do not require the code to reduce the bundle.

Please run the following code to compile on Docker:
```
docker compose up -d
```

When successful, an `output` directory will be created with two files - `component.min.css` and `component.min.js`. Please place them with the corresponding `index.html` for deploying your application on the stack or other avenues.