package com.cmclinnovations.stack.clients.gdal;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;

public class GDALOptionsTestHelper<T extends GDALOptions<T>> extends CommonOptionsTestHelper<T> {

    protected <F extends GDALOptionsTestHelper<T>> GDALOptionsTestHelper(String command, Class<T> classBeingTested) {
        super(command, classBeingTested);
    }

    protected void configureOptions(ArgsEnum optionEnum, @Nonnull T options) {
        switch (optionEnum) {
            case creationOptions:
                options.addCreationOption("COMPRESS", "NONE");
                options.addCreationOption("OVERVIEWS", "AUTO");
                break;
            default:
                super.configureOptions(optionEnum, options);
                break;
        }
    }

    protected List<String> getExpectedCommand(String... explicitArgs) {
        return Stream.of(Stream.of(
                getCommand(),
                "-if", "DummyFormat", "-of", "COG",
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES"),
                Stream.of(explicitArgs),
                Stream.of(TEST_SOURCE, TEST_DESTINATION))
                .flatMap(Function.identity())
                .collect(Collectors.toList());
    }

    protected void checkCommand(@Nonnull T options, List<String> expected) {
        List<String> actual = List.of(options.generateCommand(
                "DummyFormat",
                CommonOptionsTestHelper.TEST_SOURCE,
                CommonOptionsTestHelper.TEST_DESTINATION));
        Assertions.assertEquals(expected, actual);
    }
}
