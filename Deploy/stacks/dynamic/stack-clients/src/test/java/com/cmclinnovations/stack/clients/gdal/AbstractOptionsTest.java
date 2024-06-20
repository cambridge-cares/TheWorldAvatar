package com.cmclinnovations.stack.clients.gdal;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;

public abstract class AbstractOptionsTest<T extends CommonOptions<T>> {

    protected static final String TEST_SOURCE = "testSource";
    protected static final String TEST_DESTINATION = "testDestination";

    protected List<String> getExpectedCommand(String command, String... explicitArgs) {
        return Stream.of(Stream.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES"),
                Stream.of(explicitArgs),
                Stream.of(TEST_SOURCE, TEST_DESTINATION))
                .flatMap(Function.identity())
                .collect(Collectors.toList());
    }

    protected final void checkCommand(@Nonnull T options, List<String> expected) {
        List<String> actual = List.of(options.generateCommand(TEST_SOURCE, TEST_DESTINATION));
        Assertions.assertEquals(expected, actual);
    }
}
