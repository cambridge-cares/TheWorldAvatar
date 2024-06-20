package com.cmclinnovations.stack.clients.gdal;

import javax.annotation.Nonnull;

public class GDALOptionsFactory extends CommonOptionsFactory<MockGDALOptions> {

    GDALOptionsFactory() {
        super(MockGDALOptions.class);
    }

    protected void configureOptions(ArgsEnum optionEnum, @Nonnull MockGDALOptions options) {
        switch (optionEnum) {
            case creationOptions:
                options.addCreationOption("COMPRESS", "NONE");
                options.addCreationOption("OVERVIEWS", "AUTO");
                break;
            default:
                super.configureOptions(optionEnum, options);
                break;
        }
    }
}
