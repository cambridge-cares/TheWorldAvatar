package com.cmclinnovations.stack.clients.gdal;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class Ogr2OgrOptionsTest extends AbstractOptionsTest<Ogr2OgrOptions, Ogr2OgrOptionsFactory> {

    protected Ogr2OgrOptionsTest() {
        super("ogr2ogr", new Ogr2OgrOptionsFactory());
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "datasetCreationOptions", "datasetCreationOptionsFile" })
    void testReadDatasetCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = getOptions(args);

        List<String> expected = getExpectedCommand(
                "-dsco", "FORMAT=GML2");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "datasetCreationOptions", "datasetCreationOptionsFile" })
    void testAddCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = getOptions(args);

        options.addDatasetCreationOption("FORMAT", "GML3Deegree");

        List<String> expected = getExpectedCommand(
                "-dsco", "FORMAT=GML3Deegree");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "layerCreationOptions", "layerCreationOptionsFile" })
    void testReadLayerCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = getOptions(args);

        List<String> expected = getExpectedCommand(
                "-lco", "GEOMETRY=AS_WKT");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "layerCreationOptions", "layerCreationOptionsFile" })
    void testAddLayerCreationOption(ArgsEnum args) {
        Ogr2OgrOptions options = getOptions(args);

        options.addLayerCreationOption("GEOMETRY", "AS_XY");

        List<String> expected = getExpectedCommand(
                "-lco", "GEOMETRY=AS_XY");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "outputDatasetOpenOptions", "outputDatasetOpenOptionsFile" })
    void testReadOutputDatasetOpenOption(ArgsEnum args) {
        Ogr2OgrOptions options = getOptions(args);

        List<String> expected = getExpectedCommand(
                "-doo", "FLATTEN_NESTED_ATTRIBUTES=YES");

        checkCommand(options, expected);
    }

    @ParameterizedTest
    @EnumSource(value = ArgsEnum.class, names = { "trivial", "trivialFile",
            "outputDatasetOpenOptions", "outputDatasetOpenOptionsFile" })
    void testAddOutputDatasetOpenOption(ArgsEnum args) {
        Ogr2OgrOptions options = getOptions(args);

        options.addOutputDatasetOpenOption("FLATTEN_NESTED_ATTRIBUTES", "NO");

        List<String> expected = getExpectedCommand(
                "-doo", "FLATTEN_NESTED_ATTRIBUTES=NO");

        checkCommand(options, expected);
    }

    protected List<String> getExpectedCommand(String... explicitArgs) {
        return Stream.of(Stream.of(
                getCommand(),
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES",
                "-f", "PostgreSQL",
                "--config", "PG_USE_COPY=YES",
                "-lco", "LAUNDER=NO"),
                Stream.of(explicitArgs),
                Stream.of(TEST_DESTINATION, TEST_SOURCE))
                .flatMap(Function.identity())
                .collect(Collectors.toList());
    }
}
