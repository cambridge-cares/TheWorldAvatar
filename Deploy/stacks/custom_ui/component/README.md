# Custom UI Components
This directory contains the source code and compilation builds to create custom UI components for different applications in [The World Avatar Visualisation Framework](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework). 

## 1. Component Creation
### 1.1 Requirements
Basic knowledge on CSS and Typescript

### 1.2 Typescript Development
In the `src` directory, developers can find the core `DynamicComponent` class in the `/ts/dynamic_component.ts`. It is intended not to modify this class. Instead, please develop your application-specific component by extending this class. Once completed, create a separate subdirectory and place this new component into that subdirectory. A sample example is found at `./src/ts/datasheet/asset_display_component.ts`. 

By default, the DynamicComponent generates the following HTML elements:
```
<div id="dynamicInterfaceContainer">
  <button id="dynamicComponentTitle">Custom Title</button>
  <div id="dynamicContent"></div>
</div>
```
When developing their components, developers should take note of the following core methods. 
1) `constructor`
Constructs the core structure of the dynamic component with a custom title from input. This should be called in the child component's constructor as the first line.
2) `createDiv`
A helper method to create a new Div element with your required id or classes for changing its display.
3) `getContent`
A helper method so that developers can update the content from their child component for their requirements.
4) `render`
A method to attach the custom component to an existing HTML element in the TWA VF. This should be called in the `<script>` section of the `index.html`.

Once ready, please place your component into a distinct subdirectory. This should be similar to `./src/ts/datasheet/asset_display_component.ts`. 

### 1.3 Styling
Developers can edit the core styling for their needs in `src/css/component.css`. For styling custom components, please add the new classes and ids in the css file before compiling.

## 2. Compilation
This directory has been designed to compile the Typescript and CSS source code through a Docker container. Other compilation workflows are beyond the scope of this document. To do so, please run the following code:
```
docker compose up -d
```

When successful, an `output` directory will be created with two files - `component.min.css` and `component.min.js`. Please place them with the corresponding `index.html` for deploying your application on the stack or other avenues.