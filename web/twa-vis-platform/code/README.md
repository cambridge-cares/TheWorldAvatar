# TWA Visualisation Platform (TWA-ViP)

This directory contains the source code, configuration files, and build-time resources required to compile and package the TWA Visualisation Platform (TWA-ViP). The Platform represents the evolution of The World Avatar project's visualisation toolkit, improving the legacy TWA Visualisation Framework (TWA-VF). It aims to offer a quick & easy way to set up geospatial visualisations, analytical tools, and static online content.

The TWA Visualisation Platform takes the form of a [Next.js](https://nextjs.org/) project written using [TypeScript](https://www.typescriptlang.org/), utilising both client and server-side codes. Next.js is a framework that sits atop the [React.js library](https://react.dev/); on top of the component-based UI offering from React, Next.js adds support for server-side code, custom routing, and update data fetching routines.

This document is split into two key sections: [Architecture](#1-architecture) and [Local Development](#2-local-development-workflow).

## 1. Architecture

The code architecture adheres to industry-standard practices for Next.js and React. It is worth noting, though, that the TWA-ViP is a work in progress and not a perfect, static codebase. A brief breakdown is given below, but more detailed information can be seen within the code (and the code's comments) itself.

The project structure should match the recommended Next.js project structure (you can read about that [here](https://nextjs.org/docs/getting-started/project-structure)), utilising the optional `src` directory. More details can be found on the Next.js website, but a brief rundown is presented below.

* `src/`: Contains the application's source code, primarily in Typescript.
  * `_tests/`: Jest based unit tests
  * `app`: The app router directory contains publicly discoverable pages and API routes
  * `io`: Logic classes for input/output handling
  * `map`: Map container and their related utility methods
  * `state`: State management container based on Redux to ensure consistent application behaviour
  * `types`: Custom data types used in the project
  * `ui`: Custom UI components
  * `utils`: Common utilities
* `.eslintrc.js`: Configuration for ESLint
* `next-env.d.ts`: Exports Next.js types for the Typescript compiler
* `next.config.js`: Configuration module for Next.js projects
* `package.json`: Node project configuration file (also contains configuration for Jest)
* `server.js`: Starts a custom HTTP server that allows hosting of additional static-resource directories
* `tsconfig.json`: Configuration for the Typescript compiler

### 1.1 Routing

Following Next.js' current routing system (known as the `AppRouter`), rather that the older `PagesRouter`, the [app/page.tsx](./src/app/page.tsx) file acts as the entry point to the application when accessed by the user via a web browser. This reads the UI settings file on the server and then proceeds to load the Landing Page or, if disabled, the Map container.

The current design of the application presents a number of different pages, each with their own URL route. This includes pages such as the landing page, the visualisation page, and a number of optional additional pages (generated from user-provided Markdown files). It's worth noting, however, that this could also be accomplished by presenting the application as a single page, with these components swapping in and out. Whilst this would remove the ability to bookmark/provide a link for a particular page, it may be more efficient and should be investigated.

The [LandingPage](./src/ui/pages/landing.tsx) child component contains a landing page with static content pulled from an optional markdown file (which can be provided at runtime, e.g. in a Docker volume) along with links to [StaticContentPage](./src/ui/content/static-content-page.tsx) instances each also reading from optional markdown files.

### 1.2 Component Hierarchy

Next.js offers standardised special files to adhere to their **established component hierarchy**. Notably, `layout.tsx` provides a common layout for both existing routes and directly nested routes. `loading.tsx` serves as a fallback UI displayed immediately when navigating the application in the event of longer rendering. The component hierarchy is established as **Layout -> Loading -> Page**. Within the `layout` component, the [GlobalContainer](./src/ui/global-container.tsx) child component acts as a wrapper for all pages within the application. It provides global functionality such as a custom right-click menu, top-level toolbar, and Redux store provider (see [here](#14-state-management)).

### 1.3 Server vs Client components

The Next.js application is delineated into **server** and **client** components across the network boundary. The server hosts the application code, which remains inaccessible to the client unless the client sends a request for data access. On the other hand, the client refers to the browser on a user's device, which sends requests to a server and transforms the server's response into a user interface displayed in the browser. By default, Next.js uses **Server components**.

To indicate the use of client components, `'use client'` should be placed at the top of the component file. It's important to note that while Server components can render Client components, any nested components under these indicated Client components are automatically considered Client components.

Following these conventions, all server components are situated in the `app` directory, whereas all client components reside in the `ui` directory. Helper methods for either type of component are available in the other directories. It's essential to maintain a distinction between the server component (namely at `page.ts`) and the rendered content on the page (consisting of client component).

### 1.4 State Management

React follows a unique workflow that results in a specific method being required to store and update a component's state. In effect, it means that you cannot store a component's state in class/global variables like you would in a regular Javascript class.

If you create a custom component as a class or function (in this example let's say we have a `MyComponent` class that extends from `React.Component`), create an instance and try to render it, it's not the instance of the class that actually gets rendered; React creates an `Element` from your component instance and uses that. This is why you cannot then call methods to change the variables within your component instance. This is explained far clearer in [the top answer here](https://stackoverflow.com/questions/30971395/difference-between-react-component-and-react-element) as well as in the article [here](https://www.seanmcp.com/articles/storing-data-in-state-vs-class-variable/).

To manage a component's state, React provides a per-component state management hook (read about them [here](https://www.freecodecamp.org/news/what-is-state-in-react-explained-with-examples/)). However, the state is stored within the component instance; this means that if you show/hide components by not rendering them from a parent component, each time they reappear, they will have been reinitialised with a default state.

In addition to the above, creating a component that can interact with other components is quite tricky. Depending on how far apart they are in the UI tree, this could involve passing around a metric crap tonne of callback functions as component properties. To address this, [Redux](https://redux.js.org/tutorials/essentials/part-1-overview-concepts) has been added to the project to allow for a global store of states, allowing one component to update a global state and another to listen for changes in it and update accordingly. At the time of writing, this is primarily used in the custom right-click (context) menu; an option to hide the Map controls ribbon is added once the map is shown.

Note that this may not be the optimal solution. Some further research is warranted once we've spent more time working with React.

### 1.5 Runtime Resources

Next.js uses the `public` directory by default to house resources such as images, videos, configuration files etc. Unfortunately, Next requires this directory and its contents to be present at build time. To provide a location in which deploying developers can add their context-specific images & configurations, the `uploads` directory aims to be the target for mounting Docker volumes. Do note that this directory will be mapped automatically within a Docker container to the corresponding location for server or client interactions, but requires manual modification for local development.

The uploaded content provided by the deploying developer should match the directory structure below.

* `config/`: Should contain config/settings files.
  * `data.json`: Specifies data sources and layers to be mapped.
  * `map-settings.json`: Non-data specific configuration for maps. The format is documented [here](../docs/map-settings.md)
  * `ui-settings.json`: UI configuration settings, format is documented [here](../docs/ui-settings.json).
* `images/`: Custom image files.
* `optional-pages/`: Markdown files for optional static content (with metadata from [gray-matter](https://www.npmjs.com/package/gray-matter)).
* `style-overrides.css`: Optional CSS overrides.

## 2. Local Development Workflow

### 2.1 Requirements

Before attempting local development with the platform, the below software needs to be installed and configured.

* Node.js & npm
* `MAPBOX_USER` environment variable
* `MAPBOX_API_KEY` environment variable

Note that the environment variables listed above are only needed during local development. In production, Docker secrets within The Stack will handle these.

In addition to the above software requirements, it is also recommended that developers bring themselves up to date with the basics of the core technologies being utilised. The most critical for a basic understanding of the code are listed below.

* Typescript
  * [W3C's TypeScript Tutorial](https://www.w3schools.com/typescript/)
  * [Other TypeScript Tutorials](https://www.typescripttutorial.net/)
* React
  * [React Quick Start](https://react.dev/learn)
  * [Tutorial from tutorialspoint](https://www.tutorialspoint.com/reactjs/index.htm)
* Next.js
  * [Create a Next.js App](https://nextjs.org/learn-pages-router/basics/create-nextjs-app)
  * [A Complete Beginner's Guide to Next.js](https://welearncode.com/beginners-guide-nextjs/)

## 2.2 Installation

To install on the host machine, navigate to the `code` directory and run the `npm install` command. This will read the `package.json` file and install all required modules within a new `node_modules` directory (that should not be committed); this process may take several minutes.

## 2.3 Configuration

In order to ensure that the code runs as expected, set the contents of the `uploads` directory to `<root>\code\public\`.

## 2.4 Execution

Once installed and the required configuration files are provided, the project can be run in development mode by using the `npm run dev` command, again from within the `code` directory.

On some Windows machines, this may cause the below error to appear; if this does happen, run the `npm install -g win-node-env` command to address it.

```bash
"NODE_ENV" is not recognized as an internal or external command, operable command or batch file.
```

In development mode, the code also adds watches to source code files, automatically triggering the server to refresh/rerender pages when changes are made (known as "hot-loading").

Once running, the front page of the application should be available at [http://localhost:3000](http://localhost:3000).

> [!NOTE]  
> At the time of writing, a method allowing deploying developers to add their own custom code to the pre-generated TWA-ViP image has not been identified. This is something that needs further investigation.
