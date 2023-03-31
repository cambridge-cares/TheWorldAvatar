package com.cmclinnovations.stack.clients.gdal;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GDALTranslateOptions extends CommonOptions<GDALTranslateOptions> {

    private final Map<String, String> creationOptions = new HashMap<>();

    public GDALTranslateOptions addCreationOption(String name, String value) {
        creationOptions.put(name, value);
        return this;
    }

    String[] appendToArgs(String... args) {
        List<String> allArgs = appendCommonToArgs(args);

        creationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-co", name, value));

        return allArgs.toArray(args);
    }
}