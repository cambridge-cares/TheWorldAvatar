# Visualisation for timeline-test stack

## Deployment

1. Create these two files with the appropriate contents at the same level of this README

   - mapbox_username
   - mapbox_api_key

2. Replace <HOST_ADDRESS> in vis-files/public/config/data.json with the appropriate address (address to access the stack).

3. Replace <KEYCLOAK_URL> in vis-files/keycloak.json with the URL of the KeyCloak server, needs to be an address that can be accessed from client and the server.
   1. The configuration assumes a realm called `timeline` exists and a client called `dekstop-vis` is set up correctly with the correct redirect urls.
