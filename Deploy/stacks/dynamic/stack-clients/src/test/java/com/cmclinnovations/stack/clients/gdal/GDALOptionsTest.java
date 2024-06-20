package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class GDALOptionsTest extends AbstractOptionsTest<MockGDALOptions, GDALOptionsFactory> {

    protected GDALOptionsTest() {
        super(MockGDALOptions.TEST_COMMAND, new GDALOptionsFactory());
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testTrivial(ArgsEnum ArgsEnum) {
        MockGDALOptions options = getOptions(ArgsEnum);

        List<String> expected = getExpectedCommand();
        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "creationOptionsFile" })
    void testReadCreationOption(ArgsEnum args) {
        MockGDALOptions options = getOptions(args);

        List<String> expected = getExpectedCommand(
                "-co", "COMPRESS=NONE",
                "-co", "OVERVIEWS=AUTO");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "creationOptions", "creationOptionsFile" })
    void testAddCreationOption(ArgsEnum args) {
        MockGDALOptions options = getOptions(args);

        options.addCreationOption("COMPRESS", "LZMA");
        options.addCreationOption("OVERVIEWS", "IGNORE_EXISTING");

        List<String> expected = getExpectedCommand(
                "-co", "COMPRESS=LZMA",
                "-co", "OVERVIEWS=IGNORE_EXISTING");

        checkCommand(options, expected);
    }

}
