package com.cmclinnovations.stack.clients.gdal;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.gdal.MockCommonOptionsFactory.Args;

class CommonOptionsTest extends AbstractOptionsTest<MockCommonOptions> {

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "inputDatasetOpenOptionsFile" })
    void testReadInputDatasetOpenOption(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        List<String> expected = getExpectedCommand(command,
                "-oo", "X_POSSIBLE_NAMES=" + AbstractOptionsFactory.X_POSSIBLE_NAMES_VALUE,
                "-oo", "Y_POSSIBLE_NAMES=" + AbstractOptionsFactory.Y_POSSIBLE_NAMES_VALUE);

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile",
            "inputDatasetOpenOptions", "inputDatasetOpenOptionsFile" })
    void testAddInputDatasetOpenOption(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        options.addInputDatasetOpenOption("X_POSSIBLE_NAMES", "easting");
        options.addInputDatasetOpenOption("Y_POSSIBLE_NAMES", "northing");

        List<String> expected = getExpectedCommand(command,
                "-oo", "X_POSSIBLE_NAMES=easting",
                "-oo", "Y_POSSIBLE_NAMES=northing");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile" })
    void testProcessKeyValuePair(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        ArrayList<String> actual = new ArrayList<>();

        options.processKeyValuePair(actual, "-lco", "SEPARATOR", "COMMA");

        List<String> expected = List.of("-lco", "SEPARATOR=COMMA");

        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = getExpectedCommand(command);
        checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile", "otherOptions", "otherOptionsFile" })
    void testAddOtherOptionScalar(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        options.addOtherOption("-select", "prob,county,cromeid,lucode");

        List<String> expected = getExpectedCommand(command,
                "-select", "prob,county,cromeid,lucode");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile", "otherOptionsArray", "otherOptionsArrayFile" })
    void testAddOtherOptionArray(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        options.addOtherOption("-spat", "<xmin>", "<ymin>", "<xmax>", "<ymax>");

        List<String> expected = getExpectedCommand(command,
                "-spat", "<xmin>", "<ymin>", "<xmax>", "<ymax>");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile" })
    void testAppendToArgs(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        List<String> expected = getExpectedCommand(command);
        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile" })
    void testAppendToArgsWithExtra(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        List<String> actual = List.of(options.generateCommand(TEST_SOURCE, TEST_DESTINATION,
                "extraArg1", "extraArg2"));

        List<String> expected = getExpectedCommand(command,
                "extraArg1",
                "extraArg2");

        Assertions.assertEquals(expected, actual);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile" })
    void testWithAndGetEnv(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        options.withEnv("PG_USE_COPY", "YES");

        Map<String, String> actual = options.getEnv();

        Map<String, String> expected = Map.of("PG_USE_COPY", "YES");

        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = getExpectedCommand(command);
        checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "sridIn", "sridInFile" })
    void testGetSridIn(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        Assertions.assertEquals(AbstractOptionsFactory.SRID_IN, options.getSridIn());

        List<String> expected = getExpectedCommand(command,
                "-a_srs", AbstractOptionsFactory.SRID_IN);

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "sridOut", "sridOutFile" })
    void testGetSridOut(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        Assertions.assertEquals(AbstractOptionsFactory.SRID_OUT, options.getSridOut());

        List<String> expected = getExpectedCommand(command,
                "-t_srs", AbstractOptionsFactory.SRID_OUT);

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile" })
    void testProcessSRIDsNiether(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<Object> expected = List.of();
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = getExpectedCommand(command);
        checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "sridIn", "sridInFile" })
    void testProcessSRIDsIn(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<String> expected = List.of("-a_srs", AbstractOptionsFactory.SRID_IN);
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = getExpectedCommand(command,
                "-a_srs", AbstractOptionsFactory.SRID_IN);

        checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "sridOut", "sridOutFile" })
    void testProcessSRIDsOut(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<String> expected = List.of("-t_srs", AbstractOptionsFactory.SRID_OUT);
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = getExpectedCommand(command,
                "-t_srs", AbstractOptionsFactory.SRID_OUT);

        checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "sridBoth", "sridBothFile" })
    void testProcessSRIDsBoth(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        List<String> expected = List.of("-s_srs", AbstractOptionsFactory.SRID_IN, "-t_srs", AbstractOptionsFactory.SRID_OUT);
        Assertions.assertEquals(expected, actual);

        List<String> expectedCommand = getExpectedCommand(command,
                "-s_srs", AbstractOptionsFactory.SRID_IN,
                "-t_srs", AbstractOptionsFactory.SRID_OUT);

        checkCommand(options, expectedCommand);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile", "sridIn", "sridInFile" })
    void testSetSridIn(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        options.setSridIn("1234");

        Assertions.assertEquals("1234", options.getSridIn());

        List<String> expected = getExpectedCommand(command,
                "-a_srs", "1234");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile", "sridOut", "sridOutFile" })
    void testSetSridOut(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

        options.setSridOut("1234");

        Assertions.assertEquals("1234", options.getSridOut());

        List<String> expected = getExpectedCommand(command,
                "-t_srs", "1234");

        checkCommand(options, expected);
    }

    // @Disabled("Need to work out a good way to handle files and the SCRATCH_DIR!")
    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile" })
    void testHandleFileArg(Args args) {
        MockCommonOptions options = args.getOptions();
        String command = args.getCommand();

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

            List<String> expectedCommand = getExpectedCommand(command);
            checkCommand(options, expectedCommand);
        }
    }

    @ParameterizedTest
    @EnumSource(value = Args.class, names = { "trivial", "trivialFile" })
    void testHandleFileArgMissingFile(Args args) {
        MockCommonOptions options = args.getOptions();

        Assertions.assertThrows(RuntimeException.class,
                () -> options.handleFileArg("missingOption", "@testHandleFileArgMissingFile.txt"));
    }
}
