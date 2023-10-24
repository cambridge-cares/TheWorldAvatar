package com.cmclinnovations.featureinfo.config;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Tests the functionality of the NamespaceGetter class.
 */
public class NamespaceGetterTest {
    
    /**
     * Mock XML response when querying for Blazegraph namespaces.
     */
    private static String MOCK_BLAZEGRAPH_NAMESPACES;

    /**
     * Read in mock HTTP response before running tests.
     */
    @BeforeAll
    public static void setup() {
        try (InputStream is = NamespaceGetterTest.class.getResourceAsStream("/mock-blazegraph-namespaces.xml")) {
            BufferedReader bufferReader = new BufferedReader(new InputStreamReader(is));
            StringBuilder stringBuilder = new StringBuilder();

            String eachStringLine;
            while ((eachStringLine = bufferReader.readLine()) != null) {
                stringBuilder.append(eachStringLine).append("\n");
            }
            MOCK_BLAZEGRAPH_NAMESPACES = stringBuilder.toString();

        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not read mock HTTP response file!");
        }
    }

    /**
     * Use mock XML content to test the ability to parse the response
     * into a namespace [name, endpoint] map.
     */
    @Test
    public void testResponseParsing() {
        NamespaceGetter getter = new NamespaceGetter("http://fake-blazegraph-url.com/blazegraph", null, null);

        // Attempt to parse the mock XML result
        try {
            List<StackEndpoint> endpoints = new ArrayList<>();
            getter.parseResponse(endpoints, MOCK_BLAZEGRAPH_NAMESPACES);

            Assertions.assertTrue(endpoints.size() == 5, "Expected a different number of namespaces!");

        } catch(Exception exception) {
            Assertions.fail("Could not parse mock response!", exception);
        }
    }
}
// End of class.    