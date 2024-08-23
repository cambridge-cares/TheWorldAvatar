# Marie frontend

## Local installation

### Prerequisites

- [Node.js 20](https://nodejs.org/en/download/package-manager) or later (which includes npm)

### Steps

1. Update the `.env.local` file with the URL of the backend server for the variable `NEXT_PUBLIC_BACKEND_ENDPOINT`.
1. `npm install`.
1. Start the server:
   - Development mode: `npm run dev`,
   - Production mode: 
     - Create production build: `npm run build`,
     - Start production server: `npm run start`.

   The app will be available at `localhost:3000`. 
   
   The port can be manually set by setting the argument `-p 8080` e.g. `npm run dev -- -p 8080` (note the separator `--` required for passing arguments into run-scripts).

## Deployment (via Docker)

```
docker build -t next-app-marie .
docker run --name next-app-marie -p 3000:3000 next-app-marie
```

The app will be available at `localhost:3000`. 

## Runing Next app at a subpath behind NGINX reverse proxy

1. Set the environment variable `BASE_PATH` to prepend all Next routes with this value. Its default value is set in `.env.production`, which can be overriden by `.env.local`. Note that the `BASE_PATH` value must not have any trailing slash.
1. Configure NGINX reverse proxy as follows, where `ROOT_URL` points to the public address where the Next app runs. 
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
1. Proceed with starting the app, [locally](#local-installation) or [via Docker](#deployment-via-docker)
