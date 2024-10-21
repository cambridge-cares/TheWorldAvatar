A very simple proxy that decodes JWT tokens before passing the request to GeoServer, this container should be deployed on the same stack with the GeoServer to send requests to.

Requests to this container should have header in the following form:
```
Authorization: Bearer token123
```

where token123 is the JSON web token from KeyCloak.