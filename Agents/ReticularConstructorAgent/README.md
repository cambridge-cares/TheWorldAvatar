# ReticularConstructorAgent v0.1.0

ReticularConstructorAgent is a developmental software suite created by Aleksandar Kondinski, designed to facilitate the construction and management of reticular structures, with a notable focus on covalent organic frameworks (COFs).

## Dependencies

- **Python**: Version 3.7 or later.
- **Atomic Simulation Environment (ASE)**
- **pandas**
- **autografs**

## Repository Structure Overview

The repository primarily consists of Python modules for reticular structure management and a data directory for input/output management:

- **autografs**: Integrates the AuToGraFS software and manages related functionalities, databases, and utilities.
- **cof_logic**: Contains Python cache files related to COF logic management.
- **Data**: Houses various kinds of data and output, such as codes, custom data, csv files, molecular outputs, and stacked Covalent Organic Frameworks (COFs) in CIF format.

## Usage and Applications

This code is currently under development. One can run it from `main.py`.

### AuToGraFS Integration
Seamless integration with the Automatic Topological Generator for Framework Structures (AuToGraFS) software, enhancing its application towards the generation of reticular structures.

### Reticular Topologies Creation
Efficient generation of reticular topologies using AuToGraFS’s tools and libraries.

### COF Stacker
COFStacker, a Python class included in this suite, handles the management of atomic and lattice vector data extracted from a crystallographic file in the `.extxyz` format. The main application involves modifying and stacking layers of COFs using various stacking modes (AA and AB) and subsequently writing the modified atomic data back into diverse file formats. Example usage of COFStacker and method details can be found in the [`cof_stacker.py`](cof_stacker.py) file.

### COF Workflow
The [`cof_workflow.py`](cof_workflow.py) encompasses a COFProcessor class, which includes a workflow for reading COF data, determining 2D COF structures, and extracting COF parameters. This class is capable of reading filtered COFs and precursor data from CSV files, extracting parameters and performing further logic based on it.

## Licence

This project is licensed under the MIT Licence.

## Future Development

Future versions will aim at implementing more robust mechanisms for reticular topology generation, stacking different types of COFs, and integrating more seamlessly with knowledge graphs and other semantic databases. Enhanced data management, better integration with other software and databases, and user-friendly interfaces are also in the pipeline.

For more information and assistance, please refer to the inline documentation available in the Python scripts.
