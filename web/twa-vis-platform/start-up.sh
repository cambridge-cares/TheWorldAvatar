#!/bin/bash

# Install dependencies (not devDependencies)
npm install --omit=dev

# Build your application
npm run build

# Start your application
npm run start