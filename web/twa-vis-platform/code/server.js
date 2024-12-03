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

const colourReset = "\x1b[0m";
const colourRed = "\x1b[31m";
const colourGreen = "\x1b[32m";
const colourYellow = "\x1b[33m";


// Configure the server port; default to 3000 if not specified in environment variables
if (process.env.PORT) { console.info('port specified in environment variable: ', colourGreen, process.env.PORT, colourReset); }
const port = process.env.PORT || 3000;
const keycloakEnabled = process.env.KEYCLOAK === 'true';
const redisHost = process.env.REDIS_HOST || 'localhost';
const redisPort = process.env.REDIS_PORT || 6379;

if (process.env.ASSET_PREFIX) { console.info('Resource and Asset Prefix: ', colourGreen, process.env.ASSET_PREFIX, colourReset); }

console.info('keycloak authorisation required: ', keycloakEnabled ? colourYellow : colourGreen, process.env.KEYCLOAK, colourReset)

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
    console.info('the following pages require keycloak authentication', process.env.PROTECTED_PAGES ? colourYellow : colourRed, process.env.PROTECTED_PAGES, colourReset)
    console.info('the following pages require the', process.env.ROLE ? colourYellow : colourRed, process.env.ROLE, colourReset, 'role: ', process.env.ROLE_PROTECTED_PAGES ? colourYellow : colourRed, process.env.ROLE_PROTECTED_PAGES, colourReset)

    server.set('trust proxy', true); // the client’s IP address is understood as the left-most entry in the X-Forwarded-For header.

    if (!dev) {
      let redisClient;
      console.info(`development mode is:`, colourGreen, dev, colourReset, `-> connecting to redis session store at`, colourGreen, `${redisHost}:${redisPort}`, colourReset);
      try {
        redisClient = createClient({
          socket: {
            host: redisHost,
            port: redisPort
          }
        });
      } catch (error) {
        console.info('Error while creating Redis Client, please ensure that Redis is running and the host is specified as an environment variable if this viz app is in a Docker container');
        console.error(error);
      }
      redisClient.connect().catch('Error while creating Redis Client, please ensure that Redis is running and the host is specified as an environment variable if this viz app is in a Docker container', console.error);
      store = new RedisStore({
        client: redisClient,
        prefix: "redis",
        ttl: undefined,
      });
    } else {
      store = new MemoryStore(); // use in-memory store for session data in dev mode
      console.info(`development mode is:`, dev ? colourYellow : colourRed, dev, colourReset, `-> using in-memory session store (express-session MemoryStore())`);
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

    const protectedPages = process.env.PROTECTED_PAGES.split(',');
    protectedPages.forEach(page => {
      server.get(page, keycloak.protect());
    });
    const roleProtectedPages = process.env.ROLE_PROTECTED_PAGES.split(',');
    roleProtectedPages.forEach(page => {
      server.get(page, keycloak.protect(process.env.ROLE));
      console.info('protecting page', page, 'with role', process.env.ROLE);
    });
  }

  // Handle all other requests using Next.js
  server.all("*", (req, res) => {
    return handle(req, res);
  });

  // Start listening on the specified port and log server status
  server.listen(port, (err) => {
    if (err) throw err;
    console.info('Running at', colourGreen, `http://localhost:${port}${colourReset}`,`(on host / inside container). Development mode :${dev ? colourYellow : colourGreen}`, dev, colourReset);
  });
});