package com.cmclinnovations.stack.clients.geoserver;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.opentest4j.MultipleFailuresError;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;
import com.fasterxml.jackson.databind.exc.ValueInstantiationException;

public class GeoserverOtherStaticFileTest {

    @Test
    void testGoodInputs() throws IOException {
        try (InputStream is = GeoserverOtherStaticFileTest.class.getResourceAsStream("otherFiles.json")) {
            Iterator<JsonNode> otherFiles = Assertions
                    .assertDoesNotThrow(() -> JsonHelper.getMapper().readTree(is)).elements();

            Assertions
                    .assertAll(Stream.iterate(otherFiles.next(), (a) -> otherFiles.hasNext(), (a) -> otherFiles.next())
                            .map(block -> () -> Assertions.assertDoesNotThrow(
                                            () -> JsonHelper.getMapper().readValue(block.toString(),
                                                    GeoserverOtherStaticFile.class),
                                            () -> "This block should not fail:\n" + block.toString())));
        }
    }

    @Test
    void testBadInputs() throws IOException {
        tryToDeserializeBadValues("badmappings.json", ValueInstantiationException.class);
    }

    @Test
    void testBadPartialInputs() throws IOException {
        tryToDeserializeBadValues("badpartialmappings.json", MismatchedInputException.class);
    }

    private void tryToDeserializeBadValues(String filename, Class<? extends Throwable> expectedException)
            throws MultipleFailuresError, IOException {
        try (InputStream is = GeoserverOtherStaticFileTest.class.getResourceAsStream(filename)) {
            Iterator<JsonNode> otherFiles = Assertions
                    .assertDoesNotThrow(() -> JsonHelper.getMapper().readTree(is)).elements();

            Assertions
                    .assertAll(Stream.iterate(otherFiles.next(), (a) -> otherFiles.hasNext(), (a) -> otherFiles.next())
                            .map(block -> () -> Assertions.assertThrows(expectedException,
                                    () -> JsonHelper.getMapper().readValue(block.toString(),
                                            GeoserverOtherStaticFile.class),
                                    () -> "This block should fail:\n" + block.toString())));
        }
    }
}
