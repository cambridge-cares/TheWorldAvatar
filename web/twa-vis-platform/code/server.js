/**
 * This script starts a custom server that allows us to add additional
 * routes for serving static files that are provided at deploy time
 * (i.e. via a Docker volume).
 * 
 * By default, NextJS only allows static content in it's "public" 
 * directory, which expects its contents at build time.
 */

/* eslint-disable */
const express = require('express');
const next = require('next');

// Port to start on
const port = 3000;

// Determine which deploy mode
const dev = process.env.NODE_ENV !== 'production';

// Create the Next app
const app = next({ dev });
const handle = app.getRequestHandler();

// Running the app, async operation 
app.prepare().then(() => {
    const server = express();

    // Add route to volume files
    server.use("/uploads", express.static("../uploads"));

    // Redirecting all other requests to Next app
    server.all('*', (req, res) => {
        return handle(req, res);
    });

    // Do a little logging after starting
    server.listen(port, (err) => {
        if (err) throw err;
        console.log(`Running on port ${port}, development mode is: ${dev}`);
    });
});