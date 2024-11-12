package com.cmclinnovations.stack.clients.ontop;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;

class OntopEndpointConfigTest {
    static final ObjectMapper mapper = JsonHelper.getMapper();

    private static Stream<Arguments> getArgsForWriteTests() {
        return Stream.of(
                Arguments.of("testConfig.json", "ontopBasic", "hosty", "1234", null),
                Arguments.of("testConfigWithURL.json", "ontopExplicitURL", "hosty", "1234", "http://hosty:1234/test"));
    }

    @ParameterizedTest
    @MethodSource("getArgsForWriteTests")
    void testWrite(String fileName, String name, String hostname, String port, String url) throws IOException {
        OntopEndpointConfig config = new OntopEndpointConfig(name, hostname, port, url);
        try (InputStream fileIS = OntopEndpointConfigTest.class.getResourceAsStream(fileName)) {
            // Compare to file with whitespace stripped
            Assertions.assertEquals(IOUtils.toString(fileIS, StandardCharsets.UTF_8).replaceAll("\\s", ""),
                    mapper.writeValueAsString(config));
        }
    }

    private static Stream<Arguments> getArgsForReadTests() {
        return Stream.of(
                Arguments.of("testConfig.json", "ontopBasic", "hosty", "1234", "http://hosty:1234/sparql"),
                Arguments.of("testConfigWithURL.json", "ontopExplicitURL", "hosty", "1234", "http://hosty:1234/test"),
                Arguments.of("oldTestConfigWithCreds.json", "ontopOldWithCreds", "hosty", "1234",
                        "http://hosty:1234/sparql"));
    }

    @ParameterizedTest
    @MethodSource("getArgsForReadTests")
    void testReadConfig(String fileName, String name, String hostname, String port, String url) throws IOException {
        OntopEndpointConfig config = mapper.readValue(OntopEndpointConfigTest.class.getResource(fileName),
                OntopEndpointConfig.class);

        Assertions.assertEquals(name, config.getName());
        Assertions.assertEquals(hostname, config.getHostName());
        Assertions.assertEquals(port, config.getPort());
        Assertions.assertEquals(url, config.getUrl());
    }
}
