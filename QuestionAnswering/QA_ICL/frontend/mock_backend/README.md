# General-purpose Mock API Server

This is a minimal mock server for Marie that supports:
- `GET` and `POST` requests,
- `application/json` and `text/event-stream` responses.

## Installation

- Prerequisites: `node>=18`.
- Install dependencies:
  ```{bash}
  npm install
  ```

## Usage

1. Place JSON files into the `/data` directory according to the convention `/data/{route}/{http-verb}.{response-type}.json`.
   - `{route}` must be a top-level route. KIV: support mocking for subroutes.
   - `{http-verb}` must be either `get` or `post`. 
   - `{response-type}` must be either `json` or `event-stream`.
     - For `json`, `{http-verb}.json.json` must be a valid JSON file.
     - For `event-stream`, the content of `{http-verb}.event-stream.json` must be a valid JSON Array.
2. Start the server.
   ```{bash}
   npm run dev
   ```