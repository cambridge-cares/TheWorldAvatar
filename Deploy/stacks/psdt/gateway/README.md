This is a 'gateway' docker container based on the alpine nginx image,
whose sole purpose it is to redirect all web requests to a different
port on localhost. It is a work-around to circumvent the problem of
the stack not being reachable by web-requests coming through an ssh
tunnel, in spite of being reachable from localhost.

Run `./redeploy.sh` to (re)build the image and (re)deploy the container.

NB CORS headers are being added to all requests in order to avoid CORS
errors when a visualisation front-end container issues http requests
to one of the back-end containers.

The gateway also serves static files that are copied into it from the
`/static/` folder, including CORS headers. This can be used to serve
tilesets for Cesium visualisations:
- Copy the files  `main_network.kml` and `sub_network.kml` into `/gateway/static/ps/sewage/`.
- Copy the main Pirmasens building tileset into `/gateway/static/ps/buildings/lod2/`.
- Copy the Hugo Ball, Leibniz, measurement station, and house 45 tilesets into `/gateway/static/ps/buildings/lod3/`.

Don't forget to redeploy the container after updating the static folder.

Check the nginx configuration file for the port number on which the server is listening.
