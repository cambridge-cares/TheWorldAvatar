package com.cmclinnovations.stack.clients.core;

import java.net.URL;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.ThrowingSupplier;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

class OptionTest {
    private static final ObjectMapper mapper = JsonHelper.getMapper();

    static Stream<? extends Arguments> getArgs() {
        return Stream.of(
                Arguments.of("\"\"", "empty", List.of()),
                Arguments.of("{}", "emptyObject", List.of()),
                Arguments.of("[]", "emptyArray", List.of()),
                Arguments.of("\"A\"", "scalar", List.of("A")),
                Arguments.of("[\"A\"]", "arrayScalar", List.of("A")),
                Arguments.of("[\"A\",\"B\",\"C\"]", "array", List.of("A", "B", "C")));
    }

    @ParameterizedTest(name = "{index}. {1}: {0} -> {2}")
    @MethodSource("getArgs")
    void parseString(String json, String name, List<String> expected) {
        ThrowingSupplier<@Nonnull Option> supplier = () -> mapper.readValue(json, Option.class);
        parseAndCompare(expected, supplier);
    }

    @ParameterizedTest(name = "{index}. {1}: OptionTest/{1}.json -> {2}")
    @MethodSource("getArgs")
    void parseFile(String unused, String name, List<String> expected) {
        URL jsonFile = OptionTest.class.getResource("OptionTest/" + name + ".json");
        ThrowingSupplier<@Nonnull Option> supplier = () -> mapper.readValue(jsonFile, Option.class);
        parseAndCompare(expected, supplier);
    }

    private void parseAndCompare(List<String> expected, ThrowingSupplier<@Nonnull Option> supplier) {
        List<String> actual = Assertions.assertDoesNotThrow(supplier).getOptionList();
        Assertions.assertEquals(expected, actual);
    }

    @Test
    void arrayOfOptions() {
        String json = "[\"\",{},[],\"A\", [\"A\"],[\"A\",\"B\",\"C\"]]";

        parseAndCompareList(() -> mapper.readValue(json, new TypeReference<List<Option>>() {
        }));
    }

    @Test
    void arrayOfOptionsFile() {
        URL jsonFile = OptionTest.class.getResource("OptionTest/arrayOfOptions.json");

        ThrowingSupplier<List<Option>> supplier = () -> mapper.readValue(jsonFile, new TypeReference<List<Option>>() {
        });

        parseAndCompareList(supplier);
    }

    private void parseAndCompareList(ThrowingSupplier<List<Option>> supplier) {
        List<List<String>> actual = Assertions
                .assertDoesNotThrow(supplier).stream().map(Option::getOptionList).collect(Collectors.toList());

        List<List<? extends Object>> expected = List.of(
                List.of(),
                List.of(),
                List.of(),
                List.of("A"),
                List.of("A"),
                List.of("A", "B", "C"));

        Assertions.assertEquals(expected, actual);
    }
}
