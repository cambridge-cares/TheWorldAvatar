# Kings Lynn Project

This repository contains a selection of instructions and (required) helper projects/scripts related to the Kings Lynn Flooding use case.

Each sub-folder contains a dedicated README explaining its role within the overall project workflow.
The codes are kept under version control in Markus Hofmeister's private github repository. In case access is needed, feel free to reach out at `mh807@cam.ac.uk`.


&nbsp;
## Stack Deployment

The [StackDeployment] repository contains a step-by-step guide on how to spin up the entire Docker Stack for the King's Lynn use case and instantiate all relevant data. It links to other projects and helper scripts where appropriate.


&nbsp;
## Utilities

The [Utilities] repository contains several helper functions to be used in the context of the King's Lynn use case. Some of them provide workarounds for current shortcomings in the Docker Stack or existing agents, while others provide general utility functions (i.e. importing/exporting triples to Blazegraph).


&nbsp;
## Digital Twin Visualisation Framework (DTVF)

The [DTVF] repository contains required code and resources to deploy the Digital Twin Visualisation Framework to visualise the King's Lynn use case. It also hosts relevant data and instructions to spin up the [Feature Info Agent].


<!-- Links -->
[StackDeployment]: StackDeployment
[Utilities]: Utilities
[DTVF]: DTVF
[Feature Info Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent