package com.cmclinnovations.stack.clients.gdal;

import javax.annotation.Nonnull;

final class GDALTranslateOptionsFactory extends CommonOptionsFactory<@Nonnull GDALTranslateOptions> {

    GDALTranslateOptionsFactory() {
        super("gdal_translate", GDALTranslateOptions.class);
    }
}