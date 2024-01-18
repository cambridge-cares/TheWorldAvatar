# TWA Visualisation Platform

This directory contains all of the source code, configuration files, and build-time resources required to compile and package the TWA Visualisation Platform (TWA-ViP). The TWA Visualisation Platform is the next iteration of The World Avatar project's visualisation toolkit, improving on the legacy TWA Visualisation Framework (TWA-VF). Its aim is to offer a quick & easy way to setup geospatial visualisations, analytical tools, and static online content.

The TWA Visualisation Platform takes the form of a [Next.js](https://nextjs.org/) project written using [TypeScript](https://www.typescriptlang.org/), utilising both client and server side codes. Next.js is a framework that sits atop the [React.js library](https://react.dev/); on top of the component-based UI offering from React, Next.js adds support for server-side code, custom routing, and update data fetching routines.


## Development Requirements

Before attempting local development with the platform, the below software needs to be installed and configured. Note that it is recommended that development takes place locally (rather than within WSL or in a Docker container), as the fast-refresh functionality encounters issues if file changes do not take place directly on the host OS.

* Node.js & npm
* `MAPBOX_USERNAME` environment variable
* `MAPBOX_API_KEY` environment variable

Note that the environment variables listed above are only needed during local development. In production, Docker secrets within The Stack will handle these.

In addition to the above software requirements, it is also recommended that developers bring themselves up to date with the basics of the core technologies being utilised. The most critical of these for basic understanding of the code are listed below.

* Typescript
  * [W3C's TypeScript Tutorial](https://www.w3schools.com/typescript/)
  * [Other TypeScript Tutorials](https://www.typescripttutorial.net/)
* React
  * [React Quick Start](https://react.dev/learn)
  * [Tutorial from tutorialspoint](https://www.tutorialspoint.com/reactjs/index.htm)
* Next.js
  * [Create a Next.js App](https://nextjs.org/learn-pages-router/basics/create-nextjs-app)
  * [A Complete Beginner's Guide to Next.js](https://welearncode.com/beginners-guide-nextjs/)

## Project structure

The project structure should match the recommended Next.js project structure (you can read about that [here](https://nextjs.org/docs/getting-started/project-structure)), utilising the optional `src` directory. More details can be found on the Next.js website, but a brief rundown is presented below.

* `public/`: Contains static, build-time resources (icons, fonts etc.)
* `src/`: Application source code (primarily Typescript classes)
   * `_tests/`: Jest based unit tests
   * `app`: App router directory, contains publicly discoverable pages and API routes
   * `io`: Logic classes for input/output handling
   * `ui`: Custom UI components
   * `utils`: Common utilities
* `.eslintrc.js`: Configuration for ESLint
* `next-env.d.ts`: Exports Next.js types for the Typescript compiler
* `next.config.js`: Configuration module for Next.js projects
* `package.json`: Node project configuration file (also contains configuration for Jest)
* `server.js`: Starts a custom HTTP server that allows hosting of additional static-resource directories
* `tsconfig.json`: Configuration for the Typescript compiler

## Installation

Installation on the host machine can be carried out from the `code` directory by running the `npm install` command. This will read the `package.json` file and install all required modules within a new `node_modules` directory (that should not be committed); this process may take several minutes.

## Execution

Once installed, and the required configuration files provided (see [here](#runtime-resources)), the project can be run in development mode by using the `npm run dev` command, again from within the `code` directory. 

On some Windows machines, this may causes the below error to appear, if this does happen, simply run the `npm install -g win-node-env` command to address it.

```
"NODE_ENV" is not recognized as an internal or external command, operable command or batch file.
```

Running the code in development mode will additionally add watches to source code files, automatically triggering the server to refresh/rerender pages when changes are made (this is known as "hot-loading").

Once running, the front-page of the application should be available at [http://localhost:3000](http://localhost:3000).

## Architecture

The code architecture aims to follow industry standard usages of Next.js and React. Worth noting though that the TWA-ViP is a work in progress, and not a perfect, static codebase. A brief breakdown is given below, but more detailed information can be seen within the code (and the code's comments) itself.

Following Next.js' current routing system (known as the `AppRouter`), rather that the older `PagesRouter`, the [app/page.tsx](./src/app/page.tsx) file acts as the entry point to the application when accessed by the user via a web browser. This reads the UI settings file on the server, then proceeds to load the Landing Page; or if disabled, the Map container.

The [GlobalContainer](./src/ui/global-container.tsx) class acts as a wrapper for all pages within the application. It provides global functionality such as: custom right-click menu, top-level toolbar, Redux store provider (more on the latter later).

The current design of the application presents a number of different pages, each with their own URL route. This includes pages such as the landing page, the visualisation page, and a number of optional additional pages (generated from user-provided Markdown files). It's worth noting however that this could also be accomplished by presenting the application as a single page, with these components swapping in and out. Whilst this would remove the ability to bookmark/provide a link for a particular page, it may be more efficient and should be investigated. 

The [LandingPage](./src/ui/pages/landing.tsx) class contains a landing page with static content pulled from an optional markdown file (which can be provided at runtime, e.g. in a Docker volume) along with links to [StaticContentPage](./src/ui/content/static-content-page.tsx) instances each also reading from optional markdown files.

The [MapContainer](./src/app/visualisation/page.tsx) element contains the map element provided by either Mapbox or CesiumJS.

### State Management

React has a strange workflow that results in a specific method being required to store and update a component's state. In effect, it means that you cannot store a component's state in class/global variables like you would in a regular Javascript class.

If you create a custom component as a class or function (in this example let's say we have a `MyComponent` class that extends from `React.Component`), create an instance and try to render it, it's not the instance of the class that actually gets rendered; React creates an `Element` from your component instance and uses that. This is why you cannot then call methods to change the variables within your component instance. This is explained far clearer in [the top answer here](https://stackoverflow.com/questions/30971395/difference-between-react-component-and-react-element) as well as in the article [here](https://www.seanmcp.com/articles/storing-data-in-state-vs-class-variable/).

To manage a component's state, React provides per-component state management hook (read about them [here](https://www.freecodecamp.org/news/what-is-state-in-react-explained-with-examples/)). However, the state is stored within the component instance; this means that if you show/hide components by not rendering them from a parent component, each time they reappear they will have been reinitialised with a default state.

In addition to the above, creating component that can interact with other components is quite tricky. Depending on how far apart they are in the UI tree, this could involve passing around a metric crap-tonne of callback functions as component properties. To address this, [Redux](https://redux.js.org/tutorials/essentials/part-1-overview-concepts) has been added to the project to allow for a global store of states; allowing one component to update a global state, and another to listen for changes in it and update accordingly. At the time of writing, this is primarily used in the custom right-click (context) menu; an option to hide the Map controls ribbon is added once the map is shown.

Note that this may not be the optimal solution, some further research is warranted once we've spent more time working with React.


## Runtime Resources

By default, Next.js offers the `public` directory for housing resources such as images, videos, configuration files etc. Unfortunately, Next requires this directory and its contents to be present at build time. To provide a location in which deploying developers can add their context-specific images & configurations, the `uploads` directory aims to be the target for mounting Docker volumes.

Uploads content provided by the deploying developer should match the below directory structure.

* `config/`: Should contain config/settings files.
  * `data.json`: Specifies data sources and layers to be mapped.
  * `map-settings.json`: Non-data specific configuration for maps, format is documented [here](../docs/map-settings.md)
  * `ui-settings.json`: UI configuration settings, format is documented [here](../docs/ui-settings.json).
* `images/`: Custom image files.
* `optional-pages/`: Markdown files for optional static content (with metadata from [gray-matter](https://www.npmjs.com/package/gray-matter)).
* `style-overrides.css`: Optional CSS overrides.

> [!NOTE]  
> At the time of writing, a method allowing deploying developers to add their own custom code to the pre-generated TWA-ViP image has not been identified. This is something that needs further investigation.