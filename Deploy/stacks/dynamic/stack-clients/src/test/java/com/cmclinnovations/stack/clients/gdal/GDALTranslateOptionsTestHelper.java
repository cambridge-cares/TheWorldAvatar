package com.cmclinnovations.stack.clients.gdal;

import javax.annotation.Nonnull;

final class GDALTranslateOptionsTestHelper extends CommonOptionsTestHelper<@Nonnull GDALTranslateOptions> {

    GDALTranslateOptionsTestHelper() {
        super("gdal_translate", GDALTranslateOptions.class);
    }
}