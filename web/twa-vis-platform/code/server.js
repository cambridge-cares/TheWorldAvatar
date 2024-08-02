/**
 * Custom Server for Next.js Application
 *
 * Overview:
 * This server script enhances the Next.js application by enabling custom server functionality.
 * It primarily adds support for serving static files from a directory that's specified at deploy time,
 * facilitating the use of Docker volumes or similar deployment strategies.
 *
 * server side session storage (cookies) for keycloak authentication are configured here
 * 
 * Note:
 * Next.js, by default, serves static files from the 'public' directory, which requires contents to be present at build time.
 * This script extends that capability to allow serving files from a different directory after the build process.
 */

import express from "express";
import next from "next";

const session = require('express-session');
const Keycloak = require('keycloak-connect');

// Configure the server port; default to 3000 if not specified in environment variables
if (process.env.PORT) { console.log('port specified in .env file: ', process.env.PORT); }
const port = process.env.PORT || 3000;

// Determine the deployment mode based on NODE_ENV; default to 'development' mode if not specified
const dev = process.env.NODE_ENV !== "production";

// Initialise the Next.js application
const app = next({ dev });
const handle = app.getRequestHandler();

// Prepare the Next.js application and then start the Express server
app.prepare().then(() => {
  const server = express();
  server.set('trust proxy', true);

  // Configure the server to use session storage for keycloak authentication
  const memoryStore = new session.MemoryStore();
  server.use(
    session({
      secret: 'twa-login',
      resave: false,
      saveUninitialized: true,
      store: memoryStore,
    })
  );

  const keycloak = new Keycloak({ store: memoryStore });
  server.use(keycloak.middleware());

  server.get('/api/userinfo', keycloak.protect(), (req, res) => {
    const username = req.kauth.grant.access_token.content.preferred_username;
    const firstName = req.kauth.grant.access_token.content.given_name;
    const lastName = req.kauth.grant.access_token.content.family_name;
    res.json({ username, firstName, lastName });
  });

  server.get('/protected', keycloak.protect('oisin:secured'), (req, res, next) => {
    console.log(`Accessing ${req.path} by ${req.method}`);
    console.log(`User details: ${JSON.stringify(req.kauth.grant.access_token.content)}`);

    keycloak.protect('oisin:secured')(req, res, (err) => {
      if (err) {
        console.error("Error during Keycloak protection:", err);
        return next(err);
      }

      // Proceed with the original handler logic if there's no error
      const authorisation = "hello!";
      res.json({ authorisation });
    });
  });

  server.get('/logout', (req, res) => {
    req.logout(); // This tells Keycloak to logout
    req.session.destroy(() => { // This destroys the session
      res.clearCookie('connect.sid', { path: '/' }); // Clear the session cookie
      res.redirect('http://localhost:8081/realms/twa-test/protocol/openid-connect/logout?post_logout_redirect_uri=http%3A%2F%2Flocalhost%3A1997&client_id=oisin');
    });
  });

  server.get('/map', keycloak.protect());

  server.get('/CentralStackAgent/*', keycloak.enforcer('user:CentralStackAgent'))

  // Serve static files from the 'uploads' directory, allowing for runtime configuration via the environment variable
  server.use(
    "/uploads",
    express.static(process.env.UPLOADS_PATH || "../uploads")
  );

  // Handle all other requests using Next.js
  server.all("*", (req, res) => {
    return handle(req, res);
  });

  // Start listening on the specified port and log server status
  server.listen(port, (err) => {
    if (err) throw err;
    console.log(`Running on port ${port}, development mode is: ${dev}`);
  });
});