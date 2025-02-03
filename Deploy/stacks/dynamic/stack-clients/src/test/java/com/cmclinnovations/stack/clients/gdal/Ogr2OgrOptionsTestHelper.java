package com.cmclinnovations.stack.clients.gdal;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.Nonnull;

public class Ogr2OgrOptionsTestHelper extends CommonOptionsTestHelper<Ogr2OgrOptions> {

    Ogr2OgrOptionsTestHelper() {
        super("ogr2ogr", Ogr2OgrOptions.class);
    }

    @Override
    protected void configureOptions(ArgsEnum optionEnum, @Nonnull Ogr2OgrOptions options) {

        switch (optionEnum) {
            case datasetCreationOptions:
                options.addDatasetCreationOption("FORMAT", "GML2");
                break;
            case layerCreationOptions:
                options.addLayerCreationOption("GEOMETRY", "AS_WKT");
                break;
            case outputDatasetOpenOptions:
                options.addOutputDatasetOpenOption("FLATTEN_NESTED_ATTRIBUTES", "YES");
                break;
            default:
                super.configureOptions(optionEnum, options);
                break;
        }
    }

    protected List<String> getExpectedCommand(String... explicitArgs) {
        return Stream.of(Stream.of(
                getCommand(),
                "-oo", "AUTODETECT_TYPE=YES",
                "-oo", "EMPTY_STRING_AS_NULL=YES",
                "-f", "PostgreSQL",
                "--config", "PG_USE_COPY", "YES",
                "-lco", "LAUNDER=NO"),
                Stream.of(explicitArgs),
                Stream.of(TEST_DESTINATION, TEST_SOURCE))
                .flatMap(Function.identity())
                .collect(Collectors.toList());
    }
}
