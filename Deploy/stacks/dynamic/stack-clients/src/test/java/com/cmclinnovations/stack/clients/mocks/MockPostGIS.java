package com.cmclinnovations.stack.clients.mocks;

import static org.mockito.Mockito.mockStatic;

import java.util.function.Consumer;

import javax.annotation.Nonnull;

import org.mockito.MockedStatic;
import org.mockito.Mockito;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class MockPostGIS {

    private final MockedStatic<@Nonnull PostGISClient> postGISClientFactoryMock;

    private final PostGISClient postGISClientMock;

    public MockPostGIS() {

        PostGISEndpointConfig endpointConfig = new PostGISEndpointConfig("postgis", "test-postgis", "1234",
                "user", null);

        ClientWithEndpoint.writeEndpointConfig(endpointConfig);

        postGISClientMock = Mockito.spy(PostGISClient.getInstance());

        postGISClientFactoryMock = mockStatic(PostGISClient.class);
        postGISClientFactoryMock.when(PostGISClient::getInstance).thenReturn(postGISClientMock);
    }

    public void addOverride(Consumer<PostGISClient> rule) {
        rule.accept(postGISClientMock);
    }

    public void close() {
        if (null != postGISClientFactoryMock) {
            postGISClientFactoryMock.closeOnDemand();
        }
    }

}
