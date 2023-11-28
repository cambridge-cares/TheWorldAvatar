# Developing the platform

This directory contains all of the source code, configuration files, and build-time resources required to compile and package the TWA Visualisation Platform.

The TWA Visualisation Platform takes the form of a [Next.js](https://nextjs.org/) project. Next.js is a framework that sits atop the [React.js library](https://react.dev/); on top of the component-based UI offering from React, Next.js adds support for server-side code, custom routing, and update data fetching routines.

## Project structure

The project structure should match the recommended Next.js project structure, utilising the optional `src` directory. More details can be found on the Next.js website, but a brief rundown is presented below.

* `public/`: Contains static, build-time resources (icons, fonts etc.)
* `src/`: Application source code (primarily Typescript classes)
   * `_tests/`: Jest based unit tests
   * `app`: App router directory, contains publically discoverable pages
   * `io`: Logic classes for input/output handling
   * `ui`: Custom UI components
   * `utils`: Common utilities
* `next.config.js`: Configuration module for Next.js projects
* `next-env.d.ts`: Exports Next.js types for the Typescript compiler
* `package.json`: Node project confifuration file (also contains configuration for Jest and ESLint)
* `server.js`: Starts a custom HTTP server that allows hosting of additional static-resource directories
* `tsconfig.json`: Configuratio for the Typescript compiler

## Requirements

Before attempting local development with the platform, the below software needs to be installed and configured. Note that it is recommended that development takes place locally (rather than within WSL or in a Docker container), as the fast-refresh functionality encounters issues if file changes do not take place directly on the host OS.

* Git
* Docker
* VSCode (recommended, other IDEs are available)
* Node.js & npm

In addition to the above software requirements, it is also recommended that developers bring themselves up to date with the basics of the core technologies being utilised. The most critical of these for basic understanding of the code are listed below.

* Typescript
* React
* Next.js

## Installation

Installation on the host machine can be carried out from the `code` directory by running the `npm install` command. This will read the `package.json` file and install all required modules within a new `node_modules` directory (that should not be committed); this process may take several minutes.

## Execution

Once installed, the project can be run in development mode by using the `npm run dev` command, again from within the `code` directory. 

Running the code in development mode will additionally add watches to source code files, automatically triggering the server to refresh/rerender pages when changes are made (this is known as "hot-loading").

Once running, the front-page of the application should be available at [http://localhost:3000](http://localhost:3000).