# AI for Public Health

The AI for Public Health project focuses on integrating environmental features and physical activity datasets to quantify dynamic personal environmental exposures and support epidemiological analysis. This folder provides detailed instructions and tools for establishing an interoperable platform that incorporates key environmental attributes—such as food hygiene ratings, greenspaces, and locations of sports, entertainment, education, healthcare, and public infrastructure—along with time-series GPS trajectories that capture physical activity.

## Common Script
The [Common_Script] folder offers tools for extracting and converting raw data from XML and PDF formats. It facilitates the standardisation of XML-based food hygiene rating datasets into CSV files and the transformation of Points of Interest (PDF documents) into structured CSV outputs and ontology-driven representations.

## Stack Deployment
The [Stack_Deployment] folder contains a step-by-step guide on how to spin up the entire Docker Stack and instantiate all relevant data. It links to other projects and helper scripts where appropriate.

## Queries
The [Queries] folder contains SQL scripts designed to evaluate individual exposure to environmental features. These queries include methods for calculating exposure to food retailers, greenspaces, and other points of interest based on GPS trajectories, leveraging spatial analysis techniques such as buffers and proximity calculations to produce dynamic results. The SQL queries form the foundation for developing SPARQL-based calculations, which will be utilised in the interoperable platform.

## Visualisation
The [Visualisation] folder specifies customisations required to drive the TWA Visualisation Platform (TWA-VP).

<!-- Links -->
[Common_Script]: ./Common_Script/
[Stack_Deployment]: ./Stack_Deployment/
[Queries]: ./Queries/
[Visualisation]: ./TWA-VP/

## Authors ##
Jiying Chen (jc2341@cam.ac.uk), Nov 2024
