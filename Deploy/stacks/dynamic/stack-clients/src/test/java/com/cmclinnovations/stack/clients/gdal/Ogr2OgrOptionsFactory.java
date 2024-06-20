package com.cmclinnovations.stack.clients.gdal;

import javax.annotation.Nonnull;

public class Ogr2OgrOptionsFactory extends CommonOptionsFactory<Ogr2OgrOptions> {

    Ogr2OgrOptionsFactory() {
        super(Ogr2OgrOptions.class);
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

}
