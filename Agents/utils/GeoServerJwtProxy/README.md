# GeoServerJwtProxy

This is a proxy that decodes JWT tokens before passing the request to GeoServer, this container should be deployed on the same stack with the GeoServer to send requests to.

The request to this proxy should be in the form of `http://<HOST>:<PORT>/geoserver-jwt-proxy/MAP_REQUEST` where MAP_REQUEST shall be the request for GeoServer, e.g. `geoserver/twa/wms?service=WMS&version=1.1.0&request=GetMap&layers=twa:trajectoryDeviceId&bbox={bbox-epsg-3857}&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile`.

The proxy will use MAP_REQUEST and send it to the GeoServer instance within its stack, e.g. `http://<STACK-NAME>-geoserver:8080/MAP_REQUEST`.

Requests to this container should have header in the following form: `Authorization: Bearer token123`, where token123 is the JSON web token from KeyCloak. This proxy will decode the token and obtain the value of the subject, i.e. the value of "sub", refer (<https://auth0.com/docs/secure/tokens/json-web-tokens/json-web-token-claims>).

This container will contact KeyCloak and check the signature of the token to verify its validity.

## Environment variables

1) VIEWPARAM_NAME
    - This is the SQL view parameter name of the GeoServer layer (<https://docs.geoserver.org/main/en/user/data/database/sqlview.html>) that should match the subject of the KeyCloak user. E.g. if VIEWPARAM_NAME is specified as `user_id` and the value of `sub` is `user1`, it will append `&viewparams=user_id:user1` to the GeoServer request.

2) KEYCLOAK_SERVER
    - This should be the URL of KeyCloak relative to this container

3) KEYCLOAK_REALM
    - Realm name of the user requesting the GeoServer resource
