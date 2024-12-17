package com.cmclinnovations.stack.clients.geoserver;

import javax.annotation.Nonnull;

import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class GeoserverOtherStaticFile {

    private final String source;
    private final String target;

    @JsonCreator
    private GeoserverOtherStaticFile(
            @JsonProperty(value = "source", required = true) @Nonnull String source,
            @JsonProperty(value = "target", required = true) @Nonnull String target) {

        FileUtils.checkPathIsRelative(source, "source");
        FileUtils.checkPathIsRelative(target, "target");

        this.source = source;
        this.target = target;
    }

    public String getSource() {
        return source;
    }

    public String getTarget() {
        return target;
    }

}
