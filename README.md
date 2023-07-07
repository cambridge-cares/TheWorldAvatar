## Introduction ##

The World Avatar project aims to create a digital ‘avatar’ of the real world. The digital world is composed of a dynamic knowledge graph (KG) that contains concepts and data that describe the world, and an ecosystem of autonomous computational agents that simulate the behaviour of the world and that update the concepts and data so that the digital world remains current in time. A knowledge graph is a network of data expressed as a directed graph, where the nodes of the graph are concepts or their instances (data items) and the edges of the graph are links between related concepts or instances. Knowledge graphs are often built using the principles of Linked Data. They provide a powerful means to host, query and traverse data, and to find and retrieve related information.

The World Avatar represents information in a dynamic knowledge graph using technologies from the Semantic Web stack. Unlike a traditional database, the World Avatar contains an ecosystem of autonomous computational agents that continuously update it. The agents are described ontologically as part of the knowledge graph, and are able to perform actions on both concepts and instances. This design enables agents to update and restructure the knowledge graph, and allows them to discover and compose other agents simply by reading from and writing to the knowledge graph.

## Key technical features ##

Here are a few pointers on where to start:

### The stack

The knowledge graph and its agents are hosted using collections of containers. How to use them is explained in the [stack manager](./Deploy/stacks/dynamic/stack-manager) and [stack data uploader](./Deploy/stacks/dynamic/stack-data-uploader) folders.

##### Digital Twin Visualisation Framework

The [DTVF](./web/digital-twin-vis-framework) is a container that provides a web-page with which a human user can view and explore geospatial information held in the KG. This includes not only map data, 3D-representations of buildings, and building information models (BIM), but also any data or information connected to them within the KG, such as data calculated by models, acquired sensor data, or other time series data. The Feature Information Agent (FIA) is the link that enables the interaction between the visualisation front-end and the KG: When the visualisation needs to display something, e.g. because a user clicked on a feature, the FIA will retrieve the required information from the KG.

### Agents

Agents are pieces of software that act on the knowledge graph - reading from it, writing to it, thus making it evolve dynamically in time. The currently available agents can be found in [the agents folder](./Agents). Here are a few selected examples:

##### Access Agent

The main aim of the [Access Agent](./Agents/AccessAgent) is to provide a layer of abstraction for knowledge graph access, so that agents/developers/human users do not need to know which data is being hosted where. The agent is meant to provide a single point of access, with the routing of the queries/updates to the correct place happening behind the scenes, hidden to the outside. The routing is meant to be dynamic, i.e. new stores can be added on the fly, and existing ones can change location or be removed on the fly. Also note that even though the point of access appears singular to the outside, it can still be distributed over multiple locations, each having an access agent deployed, and is thus capable of dealing with large numbers of queries.

##### Forecasting Agent

The [Forecasting Agent](./Agents/ForecastingAgent) is meant to read (parts of) one or more time series from the KG (upon HTTP request), predict the time series for a given time interval in the future, and instantiate this as a new time series in the KG. Two different methods are available for the forecasting: 1) A Temporal Fusion Transformer (TFT) – a state-of-the-art deep learning method especially well-suited to time series forecasting, and 2) Prophet - a 'traditional' statistical method developed by Facebook. TFT requires pre-training, whereas Prophet can be applied directly without training. Both methods are capable of dealing with trends, seasonal effects, and holidays.

##### IFC Agents

There are two agents, namely the [IfcOwlConverterAgent](./Agents/IfcOwlConverterAgent) and the [Ifc2OntoBIMAgent](./Agents/Ifc2OntoBIMAgent), dedicated to turning Industry Foundation Classes (IFC) models of buildings into semantic Building Information Modelling (BIM) representations. A third agent, the [Ifc2TilesetAgent](./Agents/Ifc2TilesetAgent), can then produce 3D tilesets that can be visualised in the [DTVF](#digital-twin-visualisation-framework).

##### Data Bridge Agent

The KG is hosted as a distributed collection of containers of storage solutions and agents. The [Data Bridge Agent](./Agents/DataBridgeAgent) facilitates ingestion of external data sources and connecting agents that are not (as yet) integrated into the collective of containers.

##### Email Agent

A mature, distributed KG consists of a large number of containers running large numbers of storage solutions (triple stores and relational databases) and autonomous agents, which include agents running computational models, data acquisition and processing agents, as well as agents actuating real-world changes (e.g. laboratory experiments, building management systems, etc.). Although all these operations are automated as much as possible, in practical deployment, many things can go wrong or require human action. For this purpose, the [Email Agent](./Agents/EmailAgent) that can notify a human operator or developer whenever manual intervention is required.

### The base library

The [base lib](./JPS_BASE_LIB) is a collection of functionality that is shared among many parts of the code. Noteworthy parts include:

##### TBox Generator

The [TBox Generator](./JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/converter) is a tool to generate the TBox of an ontology, i.e. the definition of concepts and relationships within it, from a CSV file. The input is a simple, plain-text, tabular definition of concepts and their relationships. The generated output can be in the form of either a collection of subject-predicate-object triples, to be uploaded directly to a triple store, or an OWL file – an industry-standard XML-based format.

##### Agent template classes

A developer who wants to create their own agents in Java may want to use the classes [here](./JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/agent).

##### Derived Information Framework

The [Derived Information Framework](./JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation) library provides an ontological representation of provenance within a KG. That is, it captures which pieces of information stored in a KG are derived from which others and how (i.e. using which agent). It further enables checking whether a derived piece of information is up-to-date, and if not, can trigger an update of outdated information by calling the agent responsible for deriving the information in the first place. The derivation client that is part of the library manages the ontological mark-up required in the KG as well as the process of triggering the relevant agent and updating the derived information if it has been found to be out-of-date when accessed.

##### Time-series client

This library facilitates dealing with time series, which are ubiquitous in real-world applications, thus minimising code repetition for developers. Ontological mark-up, in the form of triples in a triple store, is designed to be minimal, just containing basic high-level information such as names and types of quantities of which a time series is meant to be stored. The actual data, i.e. time stamps and associated values of the quantities, are being held in a relational database, thus maintaining favourable execution speed and storage space efficiency. The time-series client can be found [here](./JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries).

##### Cloning tool

In addition to the [Data Bridge Agent](#data-bridge-agent), a [cloning tool](./JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/tools/cloning) exists to clone a repository of triples (in a triple store) into another.

##### Python wrapper

Although most of the core functionality is written in Java, the [py4jps](./JPS_BASE_LIB/python_wrapper) library makes this available to Python developers.

### Other core libraries

##### Object Graph Mapper

The main purpose of the [OGM](./core/ogm) is to provide a layer of abstraction so that developers can write code that interacts with (i.e. reads from and writes to) the KG without having to write queries or updates in a query language such as SPARQL. An OGM achieves this by mapping concepts and their properties as represented in the KG to classes with member fields in an object oriented programming language (e.g. Java). A developer can then simply write their code using classes and objects like they would write any other code, and any changes at object-level can simply be pushed to or pulled from the KG, with all the complexities of queries and updates taken care of and hidden away by the OGM. This dramatically simplifies and expedites software development around the KG.

##### Down-sampling library

Amongst applications in which time series data is being acquired from sensors, in high-frequency cases it may not be feasible (nor sensible) to store all of the raw data. For such situations, the [down-sampling library](./core/downsampling) allows storing the acquired data as time series in the KG but at (much) reduced temporal resolution. Different statistical methods can be chosen for this down-sampling, in order to minimise information loss depending on the application, such as averaging over a given time interval, or maximum, minimum, median, or instantaneous values.

## Useful links ##

* [The World Avatar](http://theworldavatar.com/)
* [CARES](https://www.cares.cam.ac.uk/)
* [Knowledge graphs](https://como.ceb.cam.ac.uk/research/twa/) at the [Computational Modelling Group](https://como.ceb.cam.ac.uk/)
* [Knowledge graphs](https://cmclinnovations.com/digitalisation/knowledge-graphs/) at [CMCL](https://cmclinnovations.com/)
