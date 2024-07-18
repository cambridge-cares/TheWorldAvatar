# TWA VF - Custom UI
This repository contains the required tools to create custom UI components for different applications in [The World Avatar Visualisation Framework](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework). These components are designed to be interjectable to any version of the VF at any position in the browser. They are only intended to be a **temporary** solution to showcase existing applications.

The repository is split into two directories. Please read their dedicated `READMEs` within the directory for a better understanding of this repository.
1) `component` 
This directory contains the source code and compilation instructions to generate additional component (javascript and css) files for the `index.html` in the VF. Although the compilation process has been simplified through Docker, a basic understanding of CSS, Javascript and Typescript syntax is necessary to develop these components.

2) `application`
This directory contains the `index.html` files of various applications to demonstrate the usage of these components and version control their developments.