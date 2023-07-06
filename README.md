<p align="center"><a href="https://theworldavatar.io" target="_blank" rel="noopener noreferrer"><img width="100%" src="./web/media/twa-readme-header.png" alt="The World Avatar logo"></a></p>

<p align="center">
    <a href="#"><img src="https://img.shields.io/badge/PRs-Welcome-green&style=flat" alt="PRs Welcome"></a>
    <a href="#"><img src="https://img.shields.io/badge/Licence-MIT-green&style=flat" alt="License"></a>
    <br/>
    <a href="#"><img src="https://img.shields.io/github/contributors-anon/cambridge-cares/TheWorldAvatar?color=blue&label=Contributors" alt="Contributors"></a>
    <a href="#"><img src="https://img.shields.io/github/stars/cambridge-cares/TheWorldAvatar?color=blue&label=Stars" alt="Stars"></a>
    <a href="#"><img src="https://img.shields.io/github/forks/cambridge-cares/TheWorldAvatar?color=blue&label=Forks" alt="Forks"></a>
</p>

## Introduction ##

The World Avatar project aims to create a digital ‘avatar’ of the real world. The digital world is composed of a dynamic knowledge graph (KG) that contains concepts and data that describe the world, and an ecosystem of autonomous computational agents that simulate the behaviour of the world and that update the concepts and data so that the digital world remains current in time. A knowledge graph is a network of data expressed as a directed graph, where the nodes of the graph are concepts or their instances (data items) and the edges of the graph are links between related concepts or instances. Knowledge graphs are often built using the principles of Linked Data. They provide a powerful means to host, query and traverse data, and to find and retrieve related information

The World Avatar represents information in a dynamic knowledge graph using technologies from the Semantic Web stack. Unlike a traditional database, the World Avatar contains an ecosystem of autonomous computational agents that continuously update it. The agents are described ontologically as part of the knowledge graph, and are able to perform actions on both concepts and instances. This design enables agents to update and restructure the knowledge graph, and allows them to discover and compose other agents simply by reading from and writing to the knowledge graph.

## Key Features ##

Listed below are a number of the key technical features available within The World Avatar ecosystem. More information on these, and other features, can be see on [The World Avatar Wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki).

**TWA Stack:**<br/>
The knowledge graph and its agents are hosted using collections of containers. How to use them is explained in the [stack manager](./Deploy/stacks/dynamic/stack-manager) and [stack data uploader](./Deploy/stacks/dynamic/stack-data-uploader) folders.

**TWA Base Library:**<br/>
The [base lib](./JPS_BASE_LIB) is a collection of functionality that is shared among many parts of the code. Core functions include the ability to generate and upload TBoxes, query KGs and RDBs, implement RESTful APIs, and triple cloning.

**Digital Twin Visualisation Framework:**<br/>
The [DTVF](./web/digital-twin-vis-framework) is a container that provides a web-page with which a user can view and explore geospatial information held in the TWA ecosystem. This includes not only map data, 3D-representations of buildings, and building information models (BIM), but also any data or information connected to them within the KG, such as data calculated by models, acquired sensor data, or other time series data.

**Intelligent Agents:**<br/>
Agents are pieces of software that act on the knowledge graph - reading from it, writing to it, thus making it evolve dynamically in time. The currently available agents can be found in [the agents folder](./Agents). 

**Object Graph Mapper:**<br/>
The main purpose of the [OGM](./core/ogm) is to provide a layer of abstraction so that developers can write code that interacts with the KG without having to write queries or updates in a query language such as SPARQL. An OGM achieves this by mapping concepts and their properties as represented in the KG to classes with member fields in an object oriented programming language (e.g. Java). A developer can then simply write their code using classes and objects like they would write any other code.


## Documentation ##

You can find the documentation for The World Avatar project in the repository's [Wiki pages](https://github.com/cambridge-cares/TheWorldAvatar/wiki).

## Contributing ##

Collaboration is at the heart of The World Avatar project. Development happens publicly on GitHub, and we are grateful to the community for contributing bugfixes and improvements. Read below to learn how you can take part in improving The World Avatar.

[**Code of Conduct**](https://www.contributor-covenant.org/version/2/1/code_of_conduct/)
* The Organization for Ethical Source has produced an Open Source Code of Conduct that we expect project participants to adhere to. Please read the full text so that you can understand what actions will and will not be tolerated.
  
[**Contributing Guide**](https://github.com/cambridge-cares/TheWorldAvatar/wiki/How-to-Contribute)
* Read our contributing guide to learn about our development process, how to propose bugfixes and improvements, and how to build and test your changes to The World Avatar.
  
[**Good First Issues**](https://github.com/cambridge-cares/TheWorldAvatar/labels/good%20first%20issue)
* To help you get your feet wet and get you familiar with our contribution process, we have a list of good first issues that contain bugs that have a relatively limited scope. This is a great place to get started developing The World Avatar

## Licence ##

The World Avatar is [MIT licensed](./LICENSE.txt).

## Useful links ##

* [The World Avatar Website](https://theworldavatar.io/)
* Key Collaborators:
  * [CARES](https://www.cares.cam.ac.uk/), [CoMo](https://como.ceb.cam.ac.uk/), [CMCL](https://www.cmcl.io/), [CMPG](https://www.cmpg.io/)
