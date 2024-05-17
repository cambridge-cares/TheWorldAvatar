# Molecular Data Processing

This repository contains a script to process molecular data by matching graphs from `.mol` and `.xyz` files and generating new `.xyz` files with combined bond information for valid mappings.

## Overview

The script performs the following steps:
1. Reads a csv list of molecules for which both mol and csv files exist. 
2. Checks for the presence of corresponding `.mol` and `.xyz` files.
3. Finds subgraph isomorphisms between the `.mol` and `.xyz` files.
4. Generates new `.xyz` files with combined bond information for valid mappings.
5. Saves the results.

## Dependencies

- `numpy`
- `pandas`
- `networkx`
- `os`
- `logging`