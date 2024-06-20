package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Nonnull;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class GDALTranslateOptionsTest {

    private static final @Nonnull GDALTranslateOptionsTestHelper TEST_HELPER = new GDALTranslateOptionsTestHelper();

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testTrivial(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = TEST_HELPER.createOptions(ArgsEnum);

        List<String> expected = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile" })
    void testProcessSRIDsNiether(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = TEST_HELPER.createOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(List.of(), actual);

        List<String> expected = TEST_HELPER.getExpectedCommand();
        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridIn", "sridInFile" })
    void testProcessSRIDsIn(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = TEST_HELPER.createOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(List.of("-a_srs", CommonOptionsTestHelper.SRID_IN), actual);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-a_srs", CommonOptionsTestHelper.SRID_IN);

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridOut", "sridOutFile" })
    void testProcessSRIDsOut(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = TEST_HELPER.createOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(List.of("-mask", "none"), actual);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-mask", "none",
                "-co", "TARGET_SRS=" + CommonOptionsTestHelper.SRID_OUT,
                "-co", "ADD_ALPHA=NO");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "sridBoth", "sridBothFile" })
    void testProcessSRIDsBoth(ArgsEnum ArgsEnum) {
        GDALTranslateOptions options = TEST_HELPER.createOptions(ArgsEnum);
        ArrayList<String> actual = new ArrayList<>();

        options.processSRIDs(actual);

        Assertions.assertEquals(
                List.of("-a_srs", CommonOptionsTestHelper.SRID_IN, "-mask", "none"), actual);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-a_srs", CommonOptionsTestHelper.SRID_IN,
                "-mask", "none",
                "-co", "TARGET_SRS=" + CommonOptionsTestHelper.SRID_OUT,
                "-co", "ADD_ALPHA=NO");

        TEST_HELPER.checkCommand(options, expected);
    }
}
