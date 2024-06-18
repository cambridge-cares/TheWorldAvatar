package com.cmclinnovations.stack.clients.gdal;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;

import com.cmclinnovations.stack.clients.core.StackClient;

class CommonOptionsTest extends OptionsTest<MockCommonOptions> {

    static class CommonOptionsArgumentsProvider extends OptionsArgumentsProvider<MockCommonOptions> {
        CommonOptionsArgumentsProvider() {
            super(MockCommonOptions.TEST_COMMAND, MockCommonOptions.class);
        }

        @Override
        protected void configureMockedOptions(String testName, MockCommonOptions options) {
            switch (testName) {
                case "testGetSridIn":
                case "testHandleSRIDsIn":
                case "testHandleSRIDsBoth":
                    options.setSridIn(OptionsArgumentsProvider.SRID_IN);
                    if ("testHandleSRIDsBoth" != testName) {
                        break;
                    }
                case "testGetSridOut":
                case "testHandleSRIDsOut":
                    options.setSridOut(OptionsArgumentsProvider.SRID_OUT);
                    break;
                default:
                    break;
            }
        }
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testAddInputDatasetOpenOption(MockCommonOptions options, String command) {
        options.addInputDatasetOpenOption("X_POSSIBLE_NAMES", "easting");
        options.addInputDatasetOpenOption("Y_POSSIBLE_NAMES", "northing");

        List<String> expected = List.of(
                command,
                "-oo", "X_POSSIBLE_NAMES=easting",
                "-oo", "Y_POSSIBLE_NAMES=northing",
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testAddKeyValuePair(MockCommonOptions options, String command) {
        ArrayList<String> actual = new ArrayList<>();

        options.addKeyValuePair(actual, "-lco", "SEPARATOR", "COMMA");

        List<String> expected = List.of("-lco", "SEPARATOR=COMMA");

        Assertions.assertEquals(expected, actual);

        List<String> expectedAll = List.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");

        checkCommand(options, expectedAll);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testAddOtherOptionScalar(MockCommonOptions options, String command) {
        options.addOtherOption("-select", "prob,county,cromeid,lucode");

        List<String> expected = List.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES",
                "-select", "prob,county,cromeid,lucode");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testAddOtherOptionArray(MockCommonOptions options, String command) {
        options.addOtherOption("-spat", "<xmin>", "<ymin>", "<xmax>", "<ymax>");

        List<String> expected = List.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES",
                "-spat", "<xmin>", "<ymin>", "<xmax>", "<ymax>");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testAppendToArgs(MockCommonOptions options, String command) {
        List<String> expected = List.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        checkCommand(options, expected);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testAppendToArgs2(MockCommonOptions options, String command) {
        List<String> actual = options.appendCommonToArgs("extraArg1", "extraArg2");

        List<String> expected = List.of(
                command,
                "extraArg1", "extraArg2",
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        Assertions.assertEquals(expected, actual);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testWithAndGetEnv(MockCommonOptions options, String command) {
        options.withEnv("PG_USE_COPY", "YES");

        Map<String, String> actual = options.getEnv();

        Map<String, String> expected = Map.of("PG_USE_COPY", "YES");

        Assertions.assertEquals(expected, actual);

        List<String> expectedAll = List.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        checkCommand(options, expectedAll);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testGetSridIn(MockCommonOptions options, String command) {
        Assertions.assertEquals(OptionsArgumentsProvider.SRID_IN, options.getSridIn());

        checkSRIDInCommand(options, command);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testGetSridOut(MockCommonOptions options, String command) {
        Assertions.assertEquals(OptionsArgumentsProvider.SRID_OUT, options.getSridOut());

        checkSRIDOutCommand(options, command);
    }

    @Disabled("Need to work out a good way to handle files and the SCRATCH_DIR!")
    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testHandleFileArg(MockCommonOptions options, String command) throws Exception {
        String filePath = Path.of(CommonOptionsTest.class.getResource("testHandleFileArg.txt").toURI()).toString();
        String expected = Path.of(StackClient.SCRATCH_DIR, "testHandleFileArg.txt").toString();
        String actual = options.handleFileArg("goodOption", "@" + filePath);
        Assertions.assertEquals(expected, actual);

        List<String> expectedAll = List.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        checkCommand(options, expectedAll);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testHandleFileArgMissingFile(MockCommonOptions options, String command) {
        Assertions.assertThrows(RuntimeException.class,
                () -> options.handleFileArg("missingOption", "@testHandleFileArgMissingFile.txt"));
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testHandleSRIDsNiether(MockCommonOptions options, String command) {
        ArrayList<String> args = new ArrayList<>();
        options.handleSRIDs(args);
        Assertions.assertEquals(List.of(), args);

        List<String> expectedAll = List.of(
                command,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        checkCommand(options, expectedAll);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testHandleSRIDsIn(MockCommonOptions options, String command) {
        ArrayList<String> args = new ArrayList<>();
        options.handleSRIDs(args);
        Assertions.assertEquals(List.of("-a_srs", OptionsArgumentsProvider.SRID_IN), args);

        checkSRIDInCommand(options, command);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testHandleSRIDsOut(MockCommonOptions options, String command) {
        ArrayList<String> args = new ArrayList<>();
        options.handleSRIDs(args);
        Assertions.assertEquals(List.of("-t_srs", OptionsArgumentsProvider.SRID_OUT), args);

        checkSRIDOutCommand(options, command);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testHandleSRIDsBoth(MockCommonOptions options, String command) {
        ArrayList<String> args = new ArrayList<>();
        options.handleSRIDs(args);
        Assertions.assertEquals(
                List.of("-t_srs", OptionsArgumentsProvider.SRID_OUT, "-s_srs", OptionsArgumentsProvider.SRID_IN), args);

        checkSRIDBothCommand(options, command);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testSetSridIn(MockCommonOptions options, String command) {
        options.setSridIn(OptionsArgumentsProvider.SRID_IN);
        Assertions.assertEquals(OptionsArgumentsProvider.SRID_IN, options.getSridIn());

        checkSRIDInCommand(options, command);
    }

    @ParameterizedTest
    @ArgumentsSource(CommonOptionsArgumentsProvider.class)
    void testSetSridOut(MockCommonOptions options, String command) {
        options.setSridOut(OptionsArgumentsProvider.SRID_OUT);
        Assertions.assertEquals(OptionsArgumentsProvider.SRID_OUT, options.getSridOut());

        checkSRIDOutCommand(options, command);
    }

    private void checkSRIDInCommand(MockCommonOptions options, String command) {
        List<String> expectedAll = List.of(
                command,
                "-a_srs", OptionsArgumentsProvider.SRID_IN,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        checkCommand(options, expectedAll);
    }

    private void checkSRIDOutCommand(MockCommonOptions options, String command) {
        List<String> expectedAll = List.of(
                command,
                "-t_srs", OptionsArgumentsProvider.SRID_OUT,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        checkCommand(options, expectedAll);
    }

    private void checkSRIDBothCommand(MockCommonOptions options, String command) {
        List<String> expectedAll = List.of(
                command,
                "-t_srs", OptionsArgumentsProvider.SRID_OUT,
                "-s_srs", OptionsArgumentsProvider.SRID_IN,
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES");
        checkCommand(options, expectedAll);
    }

}
