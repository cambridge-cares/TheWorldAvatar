package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

import javax.annotation.Nonnull;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class Ogr2OgrOptionsTest {
    private static final @Nonnull Ogr2OgrOptionsTestHelper TEST_HELPER = new Ogr2OgrOptionsTestHelper();

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "datasetCreationOptions", "datasetCreationOptionsFile" })
    void testReadDatasetCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = TEST_HELPER.createOptions(args);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-dsco", "FORMAT=GML2");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "datasetCreationOptions", "datasetCreationOptionsFile" })
    void testAddCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = TEST_HELPER.createOptions(args);

        options.addDatasetCreationOption("FORMAT", "GML3Deegree");

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-dsco", "FORMAT=GML3Deegree");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "layerCreationOptions", "layerCreationOptionsFile" })
    void testReadLayerCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = TEST_HELPER.createOptions(args);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-lco", "GEOMETRY=AS_WKT");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "layerCreationOptions", "layerCreationOptionsFile" })
    void testAddLayerCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = TEST_HELPER.createOptions(args);

        options.addLayerCreationOption("GEOMETRY", "AS_XY");

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-lco", "GEOMETRY=AS_XY");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "outputDatasetOpenOptions", "outputDatasetOpenOptionsFile" })
    void testReadOutputDatasetOpenOption(ArgsEnum args) {
        Ogr2OgrOptions options = TEST_HELPER.createOptions(args);

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-doo", "FLATTEN_NESTED_ATTRIBUTES=YES");

        TEST_HELPER.checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "outputDatasetOpenOptions", "outputDatasetOpenOptionsFile" })
    void testAddOutputDatasetOpenOption(ArgsEnum args) {
        Ogr2OgrOptions options = TEST_HELPER.createOptions(args);

        options.addOutputDatasetOpenOption("FLATTEN_NESTED_ATTRIBUTES", "NO");

        List<String> expected = TEST_HELPER.getExpectedCommand(
                "-doo", "FLATTEN_NESTED_ATTRIBUTES=NO");

        TEST_HELPER.checkCommand(options, expected);
    }
}
