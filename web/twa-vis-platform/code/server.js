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

import session, { MemoryStore } from 'express-session';
import { createClient } from "redis"
import RedisStore from 'connect-redis';
import Keycloak from 'keycloak-connect';

const colorReset = "\x1b[0m";
const colorRed = "\x1b[31m";
const colorGreen = "\x1b[32m";
const colorYellow = "\x1b[33m";


// Configure the server port; default to 3000 if not specified in environment variables
if (process.env.PORT) { console.log('port specified in .env file: ', colorGreen, process.env.PORT, colorReset); }
const port = process.env.PORT || 3000;
const keycloakEnabled = process.env.KEYCLOAK === 'true';
console.log('keycloak authorisation required: ', keycloakEnabled ? colorYellow : colorGreen, process.env.KEYCLOAK, colorReset)
console.log('the following pages require keycloak authentication', process.env.PROTECTED_PAGES ? colorYellow : colorRed, process.env.PROTECTED_PAGES, colorReset)
console.log('the following pages require the', process.env.ROLE ? colorYellow : colorRed, process.env.ROLE, colorReset, 'role: ', process.env.ROLE_PROTECTED_PAGES ? colorYellow : colorRed, process.env.ROLE_PROTECTED_PAGES, colorReset)

// Determine the deployment mode based on NODE_ENV; default to 'development' mode if not specified
const dev = process.env.NODE_ENV !== "production";

// Initialise the Next.js application
const app = next({ dev });
const handle = app.getRequestHandler();
let store;

// Prepare the Next.js application and then start the Express server
app.prepare().then(() => {
  const server = express();

  if (keycloakEnabled) { // do keycloak auth stuff if env var is set
    server.set('trust proxy', true); // the client’s IP address is understood as the left-most entry in the X-Forwarded-For header.

    if (!dev) {
      let redisClient = createClient() // assumes client is running on localhost:6379, expose this in docker-compose!
      redisClient.connect().catch(console.error)
      store = new RedisStore({
        client: redisClient,
        prefix: "redis",
        ttl: undefined,
      });
      console.log(`development mode is:`, dev ? colorYellow : colorRed, dev, `-> connecting to redis session store`, colorReset);
    } else {
      store = new MemoryStore(); // use in-memory store for session data in dev mode
      console.log(`development mode is:`, dev ? colorYellow : colorRed, dev, `-> using in-memory session store (express-session MemoryStore())`, colorReset);
    }

    server.use(
      session({
        secret: 'login',
        resave: false,
        saveUninitialized: true,
        store: store,
      })
    );

    const keycloak = new Keycloak({ store: store });
    server.use(keycloak.middleware());

    server.get('/api/userinfo', keycloak.protect(), (req, res) => {
      const { preferred_username: userName, given_name: firstName, family_name: lastName, name: fullName, realm_access: { roles }, resource_access: clientRoles } = req.kauth.grant.access_token.content;
      res.json({ userName, firstName, lastName, fullName, roles, clientRoles });
    });

    server.get('/logout', (req, res) => {
      req.logout(); // Keycloak adapter logout
      req.session.destroy(() => { // This destroys the session
        res.clearCookie('connect.sid', { path: '/' }); // Clear the session cookie
      });
    });

    const protectedPages = process.env.PROTECTED_PAGES.split(',');
    protectedPages.forEach(page => {
      server.get(page, keycloak.protect());
    });
    const roleProtectedPages = process.env.ROLE_PROTECTED_PAGES.split(',');
    roleProtectedPages.forEach(page => {
      server.get(page, keycloak.protect(process.env.ROLE));
    });
  }

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
    console.log('Running at', colorGreen, `http://localhost:${port}`, colorReset, `development mode is:`, dev ? colorGreen : colorYellow, dev, colorReset);
  });
});