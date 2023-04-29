# COFmer Drawing Agent

## Dependencies
Python 3.6 or later
RDkit

## Repository Overview

The COFmer Drawing Agent is a Python-based output agent code designed to generate a drawing mol representation of a COFmer. It uses three main pieces of information: an assembly model, precursors, and linkage formation reactions (LFR), which can be provided by a user or a knowledge graph and are handled by an input-output handler unit. However, to create a drawing mol representation, the code requires additional information about the exact molecular structure of the core units of the precursor (referred to as core chemical building units or CoreCBU) and linkage units. This information can be obtained through a query of the knowledge graph, or in cases where a rapid display is required, it can be stored in the agent's knowledge base.

The Operations Unit of COFmer Draw-Agent handles all operations related to the drawing of the COFmer. In follow-up documentation, we provide details on the data formats that are handled by the Operations Unit to derive an output drawing of the COFmer. Additionally, we provide information on the code subcomponents and dependencies involved in the programming of the Operations Unit.

## Usage and Applications

The algorithms in this repository can be used for a wide range of applications related to covalent organic frameworks, including but not limited to:

- Visual depiction of existing or potentially novel Covalent Organic Frameworks
- Description of COF(mer)s based on their chemical composition and covalent bonding

## Workflow

This code is currently under development. One can run it from main.py.
## License

This project is licensed under the MIT License.
