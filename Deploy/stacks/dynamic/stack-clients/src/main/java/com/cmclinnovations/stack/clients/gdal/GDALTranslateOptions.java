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

    @Override
    protected void handleSRIDs(List<String> allArgs) {
        String sridIn = getSridIn();
        if (null != sridIn) {
            allArgs.add("-a_srs");
            allArgs.add(sridIn);
        }
        
        String sridOut = getSridOut();
        if (null != sridOut) {
            // This is a COG file format specific option.
            // Generic re-projection will require using gdalwarp instead/as well as
            // gdal_translate. gdalwarp is also faster when re-projecting.
            creationOptions.put("TARGET_SRS", sridOut);
        }
    }
}