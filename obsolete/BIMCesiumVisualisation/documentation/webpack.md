# Webpack Tutorial
## 1) Introduction
[Webpack](https://github.com/webpack/webpack) is an open-source module bundling tool for Javascript files. It also supports the bundling of front-end assets such as HTML, CSS, images, and other data files.

The main advantages are:
1. Provides a module management system for better file, script, and dependency management
2. More efficient development workflows
3. Faster rendering performance for production builds

<sub>* Do note that this tutorial is not meant to cover every aspect (please visit their [documentation](https://webpack.js.org/concepts) for more information). It is only intended to help facilitate your development work with minimal understanding.</sub>

## 2) Contents
The minimal requirements for any Webpack project must be included in the same directory (referred to as `<root>`):
1. `<root>\package.json` - For package and dependency management in NodeJS (equivalent to Maven in Java or PyPI in Python)
2. `<root>\webpack.config.js` - For configuring Webpack
3. `<root>\src\index.js` - For including Javascript code to be loaded into a minimal HTML file

## 3) Installing Node.js dependencies
If you require to add new Javascript libraries, run the following code* in `cmd` terminal at `<root>`
```
npm install --save-dev <module>
```
<sub>* This will save the library only for development workflows. It is NOT recommended to save these libraries in production builds.</sub>

For modules to be used within the application such as Axios, React, Vue or Chart.JS, please include the `--save` flag instead to save the libraries for production builds. 

## 4) Adding data
Add your data into the `<root>\src\data` folder, and the current configurations will copy the entire folder into the final build. If you wish to add other kinds of data, please look into the documentation for more information.

## 5) Adding new separate scripts
### Javascript
If you are planning to split the `index.js` into several files, please remember to import and export functions according to the [ES6 module syntax](https://webpack.js.org/api/module-methods/#es6-recommended).

In a new `test.js` scripts:
```
export function javascriptFunction(){//code chunk here};
```
In `index.js`:
```
import {javascriptFunction} from "./test.js";
```
