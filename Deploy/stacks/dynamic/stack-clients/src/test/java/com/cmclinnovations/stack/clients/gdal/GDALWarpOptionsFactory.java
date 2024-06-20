package com.cmclinnovations.stack.clients.gdal;

import javax.annotation.Nonnull;

final class GDALWarpOptionsFactory extends CommonOptionsFactory<@Nonnull GDALWarpOptions> {

    GDALWarpOptionsFactory() {
        super("gdalwarp", GDALWarpOptions.class);
    }
}