/**
 * Custom Server for Next.js Application
 * 
 * Overview:
 * This server script enhances the Next.js application by enabling custom server functionality.
 * It primarily adds support for serving static files from a directory that's specified at deploy time,
 * facilitating the use of Docker volumes or similar deployment strategies.
 * 
 * Note:
 * Next.js, by default, serves static files from the 'public' directory, which requires contents to be present at build time.
 * This script extends that capability to allow serving files from a different directory after the build process.
 */

/* eslint-disable */
const express = require('express');
const next = require('next');

// Configure the server port; default to 3000 if not specified in environment variables
const port = process.env.PORT || 3000;

// Determine the deployment mode based on NODE_ENV; default to 'development' mode if not specified
const dev = process.env.NODE_ENV !== 'production';

// Initialise the Next.js application
const app = next({ dev });
const handle = app.getRequestHandler();

// Prepare the Next.js application and then start the Express server
app.prepare().then(() => {
    const server = express();

    // Serve static files from the 'uploads' directory, allowing for runtime configuration via the environment variable
    server.use('/uploads', express.static(process.env.UPLOADS_PATH || '../uploads'));

    // Handle all other requests using Next.js
    server.all('*', (req, res) => {
        return handle(req, res);
    });

    // Start listening on the specified port and log server status
    server.listen(port, (err) => {
        if (err) throw err;
        console.log(`Running on port ${port}, development mode is: ${dev}`);
    });
});
