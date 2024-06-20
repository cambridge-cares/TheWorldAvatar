package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.List;

public class MockCommonOptions extends CommonOptions<MockCommonOptions> {
    static final String TEST_COMMAND = "commonCommand";

    public MockCommonOptions() {
        super(TEST_COMMAND);
    }

    protected final String[] generateCommand(String source, String destination, String... extraArgs) {

        List<String> args = new ArrayList<>(2 * extraArgs.length);

        return generateCommandInternal(args, source, destination, extraArgs);
    }
}