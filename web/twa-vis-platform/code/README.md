# TWA Visualisation Platform (TWA-ViP)

This directory contains the source code, configuration files, and build-time resources required to compile and package the TWA Visualisation Platform (TWA-ViP). The Platform represents the evolution of The World Avatar project's visualisation toolkit, improving the legacy TWA Visualisation Framework (TWA-VF). It aims to offer a quick & easy way to set up geospatial visualisations, analytical tools, and static online content.

The TWA Visualisation Platform takes the form of a [Next.js](https://nextjs.org/) project written using [TypeScript](https://www.typescriptlang.org/), utilising both client and server-side codes. Next.js is a framework that sits atop the [React.js library](https://react.dev/); on top of the component-based UI offering from React, Next.js adds support for server-side code, custom routing, and update data fetching routines.

This document is split into three key sections: [Architecture](#1-architecture), [Style Guide](#2-style-guide) and [Local Development](#3-local-development-workflow).

## Table of Contents

- [1. Architecture](#1-architecture)
  - [1.1 Routing](#11-routing)
  - [1.2 Component Hierarchy](#12-component-hierarchy)
  - [1.3 Server vs Client Components](#13-server-vs-client-components)
  - [1.4 Architecture](#14-state-management)
  - [1.5 Runtime Resources](#15-runtime-resources)
  - [1.6 Reverse Proxy Urls](#16-reverse-proxy-urls)
  - [1.7 Dependent-Services](#17-dependent-services)
- [2. Style Guide](#2-style-guide)
  - [2.1 File-extensions](#21-file-extensions)
  - [2.2 Code Conventions](#22-code-conventions)
  - [2.3 Client Side](#23-client-side)
- [3. Local Development Workflow](#3-local-development-workflow)
  - [3.1 Requirements](#31-requirements)
  - [3.2 Installation](#32-installation)
  - [3.3 Configuration](#33-configuration)
  - [3.4 Execution](#34-execution)

## 1. Architecture

The code architecture adheres to industry-standard practices for Next.js and React. It is worth noting, though, that the TWA-ViP is a work in progress and not a perfect, static codebase. A brief breakdown is given below, but more detailed information can be seen within the code (and the code's comments) itself.

The project structure should match the recommended Next.js project structure (you can read about that [here](https://nextjs.org/docs/getting-started/project-structure)), utilising the optional `src` directory. More details can be found on the Next.js website, but a brief rundown is presented below.

- `src/`: Contains the application's source code, primarily in Typescript.
  - `_tests/`: Jest based unit tests
  - `app`: The app router directory contains publicly discoverable pages and API routes
  - `io`: Logic classes for input/output handling
  - `map`: Map container and their related utility methods
  - `state`: State management container based on Redux to ensure consistent application behaviour
  - `types`: Custom data types used in the project
  - `ui`: Custom UI components
  - `utils`: Common utilities
- `.eslintrc.js`: Configuration for ESLint
- `next-env.d.ts`: Exports Next.js types for the Typescript compiler
- `next.config.js`: Configuration module for Next.js projects
- `package.json`: Node project configuration file (also contains configuration for Jest)
- `server.js`: Starts a custom HTTP server that allows hosting of additional static-resource directories
- `tsconfig.json`: Configuration for the Typescript compiler

CSS colors, font and icon sizes are standardised and available as variables in `src/ui/css/globals.css`. If you require additional colors or sizes, add them to the file. Having global css variables is preferable to accommodate dark and light themes while improving maintainence.

### 1.1 Routing

Following Next.js' current routing system (known as the `AppRouter`), rather that the older `PagesRouter`, the [app/page.tsx](./src/app/page.tsx) file acts as the entry point to the application when accessed by the user via a web browser. This reads the UI settings file on the server and then proceeds to load the Landing Page or, if disabled, the Map container.

The current design of the application presents a number of different pages, each with their own URL route. This includes pages such as the landing page, the visualisation page, and a number of optional additional pages (generated from user-provided Markdown files). It's worth noting, however, that this could also be accomplished by presenting the application as a single page, with these components swapping in and out. Whilst this would remove the ability to bookmark/provide a link for a particular page, it may be more efficient and should be investigated.

The [LandingPage](./src/ui/pages/landing.tsx) child component contains a landing page with static content pulled from an optional markdown file (which can be provided at runtime, e.g. in a Docker volume) along with links to [StaticContentPage](./src/ui/content/static-content-page.tsx) instances each also reading from optional markdown files.

### 1.2 Component Hierarchy

Next.js offers standardised special files to adhere to their **established component hierarchy**. Notably, `layout.tsx` provides a common layout for both existing routes and directly nested routes. `loading.tsx` serves as a fallback UI displayed immediately when navigating the application in the event of longer rendering. The component hierarchy is established as **Layout -> Loading -> Page**. Within the `layout` component, the [GlobalContainer](./src/ui/global-container.tsx) child component acts as a wrapper for all pages within the application. It provides global functionality such as a custom right-click menu, navigation bar, and Redux store provider (see [here](#14-state-management)).

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

Next.js uses the `public` directory by default to house resources such as images, videos, configuration files etc. Unfortunately, Next requires this directory and its contents to be present at build time. To provide a location in which deploying developers can add their context-specific images & configurations, the `code/public` directory aims to be the target for mounting Docker volumes. Do note that this directory will be mapped automatically within a Docker container to the corresponding location for server or client interactions, but requires manual modification for local development.

### 1.6 Reverse Proxy urls

The default Next.js configuration is designed to function seamlessly when the base URL starts directly from the domain with no page paths. However, in cases where page paths are utilised (e.g., `http://www.example.org/page/`), as seen in stack deployment and reverse proxy scenarios, additional configuration is necessary for Next.js to operate. This includes specifying the `assetPrefix` option in the `next.config.js` file.

To accommodate developers deploying applications to various subdomains and environments, developers of the visualisation platform should always utilise relative paths such as `./` for any routes or images within your Next.js application. For any subpath present in the deployed url, update your nginx configuration with the following lines field:

```
location ~* ^/redirect/path/.*/(_next|images)(/.*)?$ {
    return 301 /redirect/path/$1$2;
}
```

For developers developing the platform, please remove the `.` in the `assetPrefix` field in the `next.config.js` file so that any nested paths will work during development.

### 1.7 Dependent services

This project have the following dependent services:

1. [Feature Info Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent)

The Feature Info Agent serves to retrieve data from the Knowledge Graph and pass it back to the TWA ViP for further interactions when called. The platform can currently visualise the metadata and time series returned from the agent, whether in full or by parts. To make it queriable in parts, please ensure an IRI is passed for the subproperty so for eg `"Subproperty" : {"iri" : "PLACEHOLDER FOR IRI"}`. An optional `stack` parameter can also be passed if the subquery should be executed on another stack.

## 2. Style Guide

This guide outlines a set of standards and best practices to ensure consistency and maintainability in our codebase. While the codebase is still being developed and may not fully comply with the following standards, please follow the conventions stated here.

### 2.1 File extensions

- The `.tsx` extension should be employed for files containing JSX, inclusive of React components.
- The `.ts` extension should be employed for pure Typescript files for functions and constants.

For example, `page.tsx` contains a JSX page render, and `json.ts` contains type definition for JSON objects.

- The corresponding `.css` files should be placed directly with their associated component.

### 2.2 Code conventions

- Type definitions in the `type` folder should use `type` instead of `interface`.

### React components

- Use functional components instead of class components for React Components.
- Props for React components should use `interface` for type checking.

```typescript
// Good
interface ButtonProps {
  text: string;
  onClick: () => void;
}

function Button(props: ButtonProps) {
  return <button onClick={onClick}>{text}</button>;
}

// Avoid
type ButtonProps = {
  text: string;
  onClick: () => void;
};

class Button extends React.Component<ButtonProps> {
  render() {
    return <button onClick={this.props.onClick}>{this.props.text}</button>;
  }
}
```

### 2.3 Client-Side

All client-side elements using relative pathing should be wrapped around the `formatAppUrl` method in `utils/client-utils.ts`. This is necessary to ensure that the elements will properly retrieve the url when reverse proxy is utilised.

Additionally, reusable components are provided to facilitate this. For navigation, `AppLink` is available at `ui/navigation/link/link.tsx`. For graphics, import either `AppImage` or `IconComponent` from `ui/graphic/image/image.tsx` and `ui/graphic/icon/icon.tsx` respectively.

## 3. Local Development Workflow

### 3.1 Requirements

Before attempting local development with the platform, the below software needs to be installed and configured.

- Node.js & npm
- `MAPBOX_USERNAME` environment variable
- `MAPBOX_API_KEY` environment variable

Note that the environment variables listed above are only needed during local development. In production, Docker secrets within The Stack will handle these.

In addition to the above software requirements, it is also recommended that developers bring themselves up to date with the basics of the core technologies being utilised. The most critical for a basic understanding of the code are listed below.

- Typescript
  - [W3C's TypeScript Tutorial](https://www.w3schools.com/typescript/)
  - [Other TypeScript Tutorials](https://www.typescripttutorial.net/)
- React
  - [React Quick Start](https://react.dev/learn)
  - [Tutorial from tutorialspoint](https://www.tutorialspoint.com/reactjs/index.htm)
- Next.js
  - [Create a Next.js App](https://nextjs.org/learn-pages-router/basics/create-nextjs-app)
  - [A Complete Beginner's Guide to Next.js](https://welearncode.com/beginners-guide-nextjs/)

## 3.2 Installation

To install on the host machine, navigate to the `code` directory and run the `npm install` command. This will read the `package.json` file and install all required modules within a new `node_modules` directory (that should not be committed); this process may take several minutes.

## 3.3 Configuration

In order to ensure that the code runs as expected, all configuration files must be placed at `<root>\code\public\`.

## 3.4 Execution

Once installed and the required configuration files are provided, the project can be run in development mode by using the `npm run dev` command, again from within the `code` directory.

On some Windows machines, this may cause the below error to appear; if this does happen, run the `npm install -g win-node-env` command to address it.

```bash
"NODE_ENV" is not recognized as an internal or external command, operable command or batch file.
```

In development mode, the code also adds watches to source code files, automatically triggering the server to refresh/rerender pages when changes are made (known as "hot-loading").

Once running, the front page of the application should be available at [http://localhost:3000](http://localhost:3000).

> [!NOTE]  
> At the time of writing, a method allowing deploying developers to add their own custom code to the pre-generated TWA-ViP image has not been identified. This is something that needs further investigation.
