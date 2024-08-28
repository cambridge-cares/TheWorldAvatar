# Marie frontend

## Initial setup

If the backend endpoint is different from the value of `NEXT_PUBLIC_BACKEND_ENDPOINT` in [`.env`](.env), create a new file named `.env.local` and set its value here. Please note that the endpoint MUST have a trailing forward slash e.g. `http://123.123.123.123:3000/api/`, so that all URLs created relative to this base path are resolved correctly.

## Local installation

### Prerequisites

- [Node.js 20](https://nodejs.org/en/download/package-manager) or later (which includes npm)

### Steps

1. [Initial setup](#initial-setup).
2. `npm install`.
3. Start the server:
   - Development mode: `npm run dev`,
   - Production mode: 
     - Create production build: `npm run build`,
     - Start production server: `npm run start`.

   The app will be available at `localhost:3000`. 
   
   The port can be manually set by setting the argument `-p 8080` e.g. `npm run dev -- -p 8080` (note the separator `--` required for passing arguments into run-scripts).

## Deployment (via Docker)

First, followg the [Initial setup](#initial-setup). Then, execute the following commands.

```
docker build -t next-app-marie .
docker run --name next-app-marie -p 3000:3000 next-app-marie
```

The app will be available at `localhost:3000/demos/marie`. 

## Running Next app at a subpath behind an NGINX reverse proxy

1. Set the environment variable `BASE_PATH` to prepend all Next routes with this value. Its default value is set in `.env.production`, which can be overriden by `.env.local`. Note that the `BASE_PATH` value MUST NOT have any trailing slash.
2. Configure NGINX reverse proxy as follows, where `ROOT_URL` points to the public address where the Next app runs. 
   ```
   location ${BASE_PATH} {
    proxy_pass  ${ROOT_URL}${BASE_PATH};
   }
   ```

   For example, assume `ROOT_URL=http://123.123.123.123:3000` and `BASE_PATH=/demos/marie`, then the NGINX reverse proxy configuration should be:
   ```
   location /demos/marie {
    proxy_pass  http://123.123.123.123:3000/demos/marie;
   }
   ```
3. Proceed with starting the app, [locally](#local-installation) or [via Docker](#deployment-via-docker)


## Development

### Overview of tech stack

- State management: [built-in React](https://react.dev/learn/managing-state)
- Styling: [tailwindcss](https://tailwindcss.com/)
- Routing: Next's [App Router](https://nextjs.org/docs/app)

### UI components

The approach for UI component development in this repository is customisation of UI primitives. This serves the following purposes:
- Leverage well-tested implementations of common UI components.
- Each React primitive can be installed separately, making it easier to keep the entire app as lean as possible.
- React primitives are bare-bone and are thus very flexible for style customisation.

This repository makes extensive use of boilerplates provided by [shadcn](https://ui.shadcn.com/docs), which makes use of React primitive libraries such as [Radix UI](https://www.radix-ui.com/primitives) and [TanStack](https://tanstack.com/). Shadcn boilerplates provide examples of style customisation with tailwindcss and internal state management for more complex components e.g. [DataTable](https://ui.shadcn.com/docs/components/data-table).

### Image components

For rendering images bundled together with the app, please use [static imports](https://nextjs.org/docs/app/building-your-application/optimizing/images#local-images) for the `src` attribute. This ensures that images are rendered properly even when the app is deployed behind an NGINX reverse proxy.