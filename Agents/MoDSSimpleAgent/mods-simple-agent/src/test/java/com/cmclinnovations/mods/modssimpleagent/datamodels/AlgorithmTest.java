package com.cmclinnovations.mods.modssimpleagent.datamodels;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.fasterxml.jackson.databind.ObjectMapper;

final class AlgorithmTest {
    static final Integer maxNumberOfResultsDefaultValue = Integer.MAX_VALUE;
    static final ObjectMapper objectMapper = new ObjectMapper();

    @ParameterizedTest
    @MethodSource("jsonIntProvider")
    void maxNumberOfResultsDefaultValueTest(String json, int value) throws Exception {
        Algorithm algorithm = objectMapper.reader().readValue(json, Algorithm.class);
        assertEquals(value, algorithm.maxNumberOfResults());
    }

    static Stream<Arguments> jsonIntProvider() {
        return Stream.of(
                Arguments.of("{\"maxNumberOfResults\": 15}", 15),
                Arguments.of("{}", maxNumberOfResultsDefaultValue),
                Arguments.of("{\"maxNumberOfResults\": null}", maxNumberOfResultsDefaultValue),
                Arguments.of("{\"maxNumberOfResults\": 0}", 0),
                Arguments.of("{\"maxNumberOfResults\": \"56\"}", 56));
    }
}
