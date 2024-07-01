package com.cmclinnovations.stack.clients.gdal;

import javax.annotation.Nonnull;

final class GDALWarpOptionsTestHelper extends CommonOptionsTestHelper<@Nonnull GDALWarpOptions> {

    GDALWarpOptionsTestHelper() {
        super("gdalwarp", GDALWarpOptions.class);
    }
}