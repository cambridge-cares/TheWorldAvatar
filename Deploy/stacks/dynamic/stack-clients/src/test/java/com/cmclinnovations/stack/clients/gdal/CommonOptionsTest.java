package com.cmclinnovations.stack.clients.gdal;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import com.cmclinnovations.stack.clients.core.StackClient;

class CommonOptionsTest {

    private static final @Nonnull CommonOptionsTestHelper<@Nonnull MockCommonOptions> TEST_HELPER = new CommonOptionsTestHelper<>(
            MockCommonOptions.TEST_COMMAND, MockCommonOptions.class);

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testTrivial(ArgsEnum ArgsEnum) {
        MockCommonOptions options = TEST_HELPER.createOptions(ArgsEnum);

        List<String> expected = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "inputDatasetOpenOptionsFile" })
    void testReadInputDatasetOpenOption(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-oo", "X_POSSIBLE_NAMES=" + CommonOptionsTestHelper.X_POSSIBLE_NAMES_VALUE,
                "-oo", "Y_POSSIBLE_NAMES=" + CommonOptionsTestHelper.Y_POSSIBLE_NAMES_VALUE);

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "inputDatasetOpenOptions", "inputDatasetOpenOptionsFile" })
    void testAddInputDatasetOpenOption(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        options.addInputDatasetOpenOption("X_POSSIBLE_NAMES", "easting");
        options.addInputDatasetOpenOption("Y_POSSIBLE_NAMES", "northing");

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-oo", "X_POSSIBLE_NAMES=easting",
                "-oo", "Y_POSSIBLE_NAMES=northing");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testProcessKeyValuePair(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        ArrayList<String> actual = new ArrayList<>();

        options.processKeyValuePair(actual, "-lco", "SEPARATOR", "COMMA");

        List<String> expected = List.of("-lco", "SEPARATOR=COMMA");

        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile", "otherOptions", "otherOptionsFile" })
    void testAddOtherOptionScalar(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        options.addOtherOption("-select", "prob,county,cromeid,lucode");

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-select", "prob,county,cromeid,lucode");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile", "otherOptionsArray",
            "otherOptionsArrayFile" })
    void testAddOtherOptionArray(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        options.addOtherOption("-spat", "<xmin>", "<ymin>", "<xmax>", "<ymax>");

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-spat", "<xmin>", "<ymin>", "<xmax>", "<ymax>");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testAppendToArgs(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        List<String> expected = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testAppendToArgsWithExtra(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        List<String> actual = List.of(options.generateCommand(
                CommonOptionsTestHelper.TEST_SOURCE,
                CommonOptionsTestHelper.TEST_DESTINATION,
                "extraArg1", "extraArg2"));

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "extraArg1",
                "extraArg2");

        Assertions.assertEquals(expected, actual);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testWithAndGetEnv(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        options.withEnv("PG_USE_COPY", "YES");

        Map<String, String> actual = options.getEnv();

        Map<String, String> expected = Map.of("PG_USE_COPY", "YES");

        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridIn", "sridInFile" })
    void testGetSridIn(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        Assertions.assertEquals(CommonOptionsTestHelper.SRID_IN, options.getSridIn());

        List<String> expected = TEST_HELPER.getExpectedCommand("-a_srs", CommonOptionsTestHelper.SRID_IN);

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridOut", "sridOutFile" })
    void testGetSridOut(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        Assertions.assertEquals(CommonOptionsTestHelper.SRID_OUT, options.getSridOut());

        List<String> expected = TEST_HELPER.getExpectedCommand("-t_srs", CommonOptionsTestHelper.SRID_OUT);

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testProcessSRIDsNiether(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<Object> expected = List.of();
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridIn", "sridInFile" })
    void testProcessSRIDsIn(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<String> expected = List.of("-a_srs", CommonOptionsTestHelper.SRID_IN);
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = TEST_HELPER.getExpectedCommand(
                "-a_srs", CommonOptionsTestHelper.SRID_IN);

        TEST_HELPER.checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridOut", "sridOutFile" })
    void testProcessSRIDsOut(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<String> expected = List.of("-t_srs", CommonOptionsTestHelper.SRID_OUT);
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = TEST_HELPER.getExpectedCommand(
                "-t_srs", CommonOptionsTestHelper.SRID_OUT);

        TEST_HELPER.checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridBoth", "sridBothFile" })
    void testProcessSRIDsBoth(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<String> expected = List.of("-s_srs", CommonOptionsTestHelper.SRID_IN, "-t_srs",
                CommonOptionsTestHelper.SRID_OUT);
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = TEST_HELPER.getExpectedCommand(
                "-s_srs", CommonOptionsTestHelper.SRID_IN,
                "-t_srs", CommonOptionsTestHelper.SRID_OUT);

        TEST_HELPER.checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile", "sridIn", "sridInFile" })
    void testSetSridIn(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        options.setSridIn("1234");

        Assertions.assertEquals("1234", options.getSridIn());

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-a_srs", "1234");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile", "sridOut", "sridOutFile" })
    void testSetSridOut(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        options.setSridOut("1234");

        Assertions.assertEquals("1234", options.getSridOut());

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-t_srs", "1234");

        TEST_HELPER.checkCommand(options, expected);
    }

    // @Disabled("Need to work out a good way to handle files and the SCRATCH_DIR!")
    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testHandleFileArg(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        String filePath = Path.of(".").toAbsolutePath()
                .relativize(Path.of(Assertions.assertDoesNotThrow(
                        () -> CommonOptionsTest.class.getResource("testHandleFileArg.txt").toURI()))
                        .toAbsolutePath())
                .toString();

        try (MockedStatic<StackClient> utilities = Mockito.mockStatic(StackClient.class)) {
            utilities.when(StackClient::getScratchDir).thenReturn("./target/test_temp");

            String expected = "@" + Path.of(StackClient.getScratchDir(), filePath).toString();
            String actual = Assertions.assertDoesNotThrow(() -> options.handleFileArg("goodOption", "@" + filePath));
            Assertions.assertEquals(expected, actual);

            List<String> expectedCommand = TEST_HELPER.getExpectedCommand();
            TEST_HELPER.checkCommand(options, expectedCommand);
        }
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testHandleFileArgMissingFile(ArgsEnum args) {
        MockCommonOptions options = TEST_HELPER.createOptions(args);

        Assertions.assertThrows(RuntimeException.class,
                () -> options.handleFileArg("missingOption", "@testHandleFileArgMissingFile.txt"));
    }
}
