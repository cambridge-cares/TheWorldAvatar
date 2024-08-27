package com.cmclinnovations.stack.clients.core.datasets;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@SuppressWarnings("null")
class MetadataTest {
    private static final ObjectMapper mapper = JsonHelper.getMapper();

    @ParameterizedTest
    @ValueSource(strings = {
            "{\"prefixes\":{\"p\":\"http://p/\"}}",
            "{\"prefixes\":{\"p\":\"http://p/\"}, \"triplePatterns\":\"?a p:b <http://c> ,  <http://e> . \\n <http://c> <http://d> <http://e> \\n \"}" })
    void testGetPrefixesSingle(String json) throws JsonProcessingException {

        Metadata metadata = mapper.readValue(json, Metadata.class);

        Assertions.assertEquals(List.of("PREFIX p: <http://p/>"), metadata.getPrefixes().stream().map(Prefix::getQueryString)
                .collect(Collectors.toList()));
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "{\"prefixes\":{\"p\":\"http://p/\",\"q\":\"http://q/\"}}",
            "{\"prefixes\":{\"p\":\"http://p/\",\"q\":\"http://q/\"}}, \"triplePatterns\":\"?a p:b <http://c> ,  <http://e> . \\n <http://c> <http://d> <http://e> \\n \"}" })
    void testGetPrefixesMultiple(String json) throws JsonProcessingException {

        Metadata metadata = mapper.readValue(json, Metadata.class);

        Assertions.assertEquals(
                Stream.of("PREFIX p: <http://p/>", "PREFIX q: <http://q/>")
                        .sorted().collect(Collectors.toList()),
                metadata.getPrefixes().stream().map(Prefix::getQueryString).sorted().collect(Collectors.toList()));
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "{}",
            "{\"prefixes\":{}}",
            "{\"prefixes\":{\"p\":\"http://p/\"}}",
            "{\"triplePatterns\":\"\"}" })
    void testGetTriplePatternsEmpty(String json) {
        Metadata metadata = Assertions.assertDoesNotThrow(() -> mapper.readValue(json, Metadata.class));

        Assertions.assertEquals(Optional.empty(), metadata.getTriplePatterns());
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "{\"triplePatterns\":\"<http://p/a> <http://q/b> <http://r/c>\"}",
            "{\"triplePatterns\":\"<http://p/a> <http://q/b> <http://r/c> .\"}",
            "{\"triplePatterns\":\"<http://p/a> <http://q/b> <http://r/c> \\n\"}",
            "{\"triplePatterns\":\"<http://p/a> <http://q/b> <http://r/c> \\n .\"}",
            "{\"triplePatterns\":\"<http://p/a> <http://q/b> <http://r/c> \\n .\"}",
            "{\"prefixes\":{\"p\":\"http://p/\"},\"triplePatterns\":\"<http://p/a> <http://q/b> <http://r/c> .\"}" })
    void testGetTriplePatternsSingle(String json) {
        Metadata metadata = Assertions.assertDoesNotThrow(() -> mapper.readValue(json, Metadata.class));

        Assertions.assertEquals("<http://p/a> <http://q/b> <http://r/c> .",
                metadata.getTriplePatterns().get().getQueryString());
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "{\"triplePatterns\":\"?a p:b <http://c> , <http://e> .\\n<http://c> <http://d> <http://e>\"}",
            "{\"triplePatterns\":\"?a p:b <http://c> , <http://e> .\\n<http://c> <http://d> <http://e> .\"}",
            "{\"triplePatterns\":\"?a p:b <http://c> , <http://e> .\\n<http://c> <http://d> <http://e> \\n\"}",
            "{\"triplePatterns\":\"?a p:b <http://c> , <http://e> .\\n<http://c> <http://d> <http://e> \\n .\"}",
            "{\"triplePatterns\":\"@target/test-classes/com/cmclinnovations/stack/clients/core/datasets/metadataFile.ttl\"}",
            "{\"prefixes\":{\"p\":\"http://p/\"}, \"triplePatterns\":\"?a p:b <http://c> , <http://e> .\\n<http://c> <http://d> <http://e> \\n \"}",
            "{\"prefixes\":{\"p\":\"http://p/\"}, \"triplePatterns\":\"?a p:b <http://c> , <http://e> .\\n<http://c> <http://d> <http://e> \\n \"}" })
    void testGetTriplePatternsMultiple(String json) {
        Metadata metadata = Assertions.assertDoesNotThrow(() -> mapper.readValue(json, Metadata.class));

        Assertions.assertEquals("?a p:b <http://c> , <http://e> .\n<http://c> <http://d> <http://e> .",
                metadata.getTriplePatterns().get().getQueryString());
    }
}
