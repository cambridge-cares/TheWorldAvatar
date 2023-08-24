package com.cmclinnovations.featureinfo.kg;

import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.EndpointType;

public class BaseHandlerTest {

    /**
     * Read in mock config file before running tests.
     */
    @BeforeAll
    public static void setup() {
        FeatureInfoAgent.CONFIG = new ConfigStore();

        // Add mock endpoints to the config
        FeatureInfoAgent.CONFIG.addEndpoint(
                new ConfigEndpoint("ONTOP", "http://my-fake-ontop.com/ontop/sparql", null, null, EndpointType.ONTOP));
    }

    @Test
    void testRunQuery() {
        BaseHandler baseHandler = new BaseHandler("http://dummy/iri", List.of()) {
        };

        String filteredQuery = baseHandler
                .filterOntopEndpoints("SERVICE [ONTOP] [ONTOP-test] [ontop-test2] [ONTOP] the end.");
        String expected = "SERVICE <http://my-fake-ontop.com/ontop/sparql> <http://my-fake-ontop.com/ontop-test/sparql> <http://my-fake-ontop.com/ontop-test2/sparql> <http://my-fake-ontop.com/ontop/sparql> the end.";
        Assertions.assertEquals(expected, filteredQuery, "Query not featured correctly.");
    }
}
