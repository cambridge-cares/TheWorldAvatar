package com.cmclinnovations.stack.clients.gdal;

public class MockGDALOptions extends GDALOptions<MockGDALOptions> {
    static final String TEST_COMMAND = "gdalCommand";

    public MockGDALOptions() {
        super(TEST_COMMAND);
    }
}