# General-purpose Mock API Server

## Installation

- Prerequisites: `node>=18`.
- Install dependencies:
  ```{bash}
  npm install
  ```

## Usage

1. Place JSON files into the `/data` directory according to the convention `/data/{route}/{http_verb}.json`.
1. Start the server.
   ```{bash}
   npm run dev
   ```