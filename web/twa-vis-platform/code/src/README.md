# TWA Visualisation Platform (TWA-ViP) Source Code

## Overview

The `src` directory contains the core source code of the application, structured to facilitate ease of development and maintenance.

## Directory Structure

* `/_tests`: Contains the unit and integration tests for the application aims to cover critical functionalities of the application.

* `/app`: This is the heart of the application, containing the main application logic, routing, and the entry point of the application. It includes the setup for the application's pages and API routes.

* `/io`: Handles input/output operations of the application. It includes logic for data fetching, processing, and interacting with external APIs.

* `/map`: Dedicated to the map functionalities of the application. It contains components and logic for integrating and controlling map-related features, using libraries like Mapbox or CesiumJS.

* `/state`: Manages the global state of the application. This includes Redux stores, actions, reducers, and context API files. It's essential for maintaining the application state across different components.

* `/types`: Contains TypeScript type definitions. These types are used throughout the application to ensure type safety and consistency.

* `/ui`: Houses the UI components of the application. It includes reusable UI elements, layout components, and styling information.

* `/utils`: A collection of utility functions that provide common functionalities used across the application includes date formatting, data transformations, or custom hooks.
