package com.cmclinnovations.stack.clients.gdal;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GDALTranslateOptions extends CommonOptions<GDALTranslateOptions> {

    @JsonProperty
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
            // Need to disable adding alpha as this causes gdal_translate to add a mask band
            // to rasters that don't have a nodata value, which causes issues. This also
            // brings the output more inline with just using gdalswarp.
            creationOptions.put("ADD_ALPHA", "NO");
            // Masks seem not to be supported properly by PostGIS or GeoServer so disabling
            // them seem sensible.
            allArgs.add("-mask");
            allArgs.add("none");
        }
    }
}