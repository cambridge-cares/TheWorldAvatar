package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class GDALTranslateOptionsTest extends AbstractOptionsTest<GDALTranslateOptions, GDALTranslateOptionsFactory> {

    protected GDALTranslateOptionsTest() {
        super("gdal_translate", new GDALTranslateOptionsFactory());
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testTrivial(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = getOptions(ArgsEnum);

        List<String> expected = getExpectedCommand();
        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testProcessSRIDsNiether(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = getOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(List.of(), actual);

        List<String> expected = getExpectedCommand();
        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridIn", "sridInFile" })
    void testProcessSRIDsIn(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = getOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(List.of("-a_srs", CommonOptionsFactory.SRID_IN), actual);

        List<String> expected = getExpectedCommand(
                "-a_srs", CommonOptionsFactory.SRID_IN);

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridOut", "sridOutFile" })
    void testProcessSRIDsOut(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = getOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(List.of("-mask", "none"), actual);

        List<String> expected = getExpectedCommand(
                "-mask", "none",
                "-co", "TARGET_SRS=" + CommonOptionsFactory.SRID_OUT,
                "-co", "ADD_ALPHA=NO");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridBoth", "sridBothFile" })
    void testProcessSRIDsBoth(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = getOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(
                List.of("-a_srs", CommonOptionsFactory.SRID_IN, "-mask", "none"), actual);

        List<String> expected = getExpectedCommand(
                "-a_srs", CommonOptionsFactory.SRID_IN,
                "-mask", "none",
                "-co", "TARGET_SRS=" + CommonOptionsFactory.SRID_OUT,
                "-co", "ADD_ALPHA=NO");

        checkCommand(options, expected);
    }


}
