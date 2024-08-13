# Marie frontend

## Installation

### Prerequisites

- [Node.js 18.18](https://nodejs.org/en/download/package-manager) or later (which includes npm)

### Steps

1. Update the `.env` file with the URL of the backend server for the variable `NEXT_PUBLIC_BACKEND_ENDPOINT`.
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