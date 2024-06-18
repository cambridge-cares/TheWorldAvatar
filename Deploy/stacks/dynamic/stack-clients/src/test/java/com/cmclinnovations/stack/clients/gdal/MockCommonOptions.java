package com.cmclinnovations.stack.clients.gdal;

public class MockCommonOptions extends CommonOptions<MockCommonOptions> {
    static final String TEST_COMMAND = "testCommand";

    public MockCommonOptions() {
        super(TEST_COMMAND);
    }
}