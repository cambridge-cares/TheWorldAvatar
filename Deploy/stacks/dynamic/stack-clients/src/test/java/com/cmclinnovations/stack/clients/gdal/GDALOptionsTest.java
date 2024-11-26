package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

import javax.annotation.Nonnull;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class GDALOptionsTest {

    private static final @Nonnull GDALOptionsTestHelper<@Nonnull MockGDALOptions> TEST_HELPER = new GDALOptionsTestHelper<>(
            MockGDALOptions.TEST_COMMAND, MockGDALOptions.class);

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testTrivial(ArgsEnum ArgsEnum) {
        MockGDALOptions options = TEST_HELPER.createOptions(ArgsEnum);

        List<String> expected = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "creationOptionsFile" })
    void testReadCreationOption(ArgsEnum args) {
        MockGDALOptions options = TEST_HELPER.createOptions(args);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-co", "COMPRESS=NONE",
                "-co", "OVERVIEWS=AUTO");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "creationOptions", "creationOptionsFile" })
    void testAddCreationOption(ArgsEnum args) {
        MockGDALOptions options = TEST_HELPER.createOptions(args);

        options.addCreationOption("COMPRESS", "LZMA");
        options.addCreationOption("OVERVIEWS", "IGNORE_EXISTING");

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-co", "COMPRESS=LZMA",
                "-co", "OVERVIEWS=IGNORE_EXISTING");

        TEST_HELPER.checkCommand(options, expected);
    }
}
