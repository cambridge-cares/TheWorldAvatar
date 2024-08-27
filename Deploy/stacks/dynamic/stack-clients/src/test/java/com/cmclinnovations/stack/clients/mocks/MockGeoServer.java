package com.cmclinnovations.stack.clients.mocks;

import java.net.MalformedURLException;

import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;

public class MockGeoServer extends MockHTTPService {

    public MockGeoServer() throws MalformedURLException {
        GeoServerClient.writeEndpointConfig(
                new RESTEndpointConfig("geoserver", url,
                        "user", null));
    }

}
