package com.cmclinnovations.stack.services.config;

import java.net.URI;
import java.net.URL;

public class Connection {

    private URL url;
    private URI uri;
    private URI externalPath;

    public Connection() {
    }

    public Connection(URL url, URI uri, URI externalPath) {
        this.url = url;
        this.uri = uri;
        this.externalPath = externalPath;
    }

    public URL getUrl() {
        return url;
    }

    public URI getUri() {
        return uri;
    }

    public URI getExternalPath() {
        return externalPath;
    }

}
