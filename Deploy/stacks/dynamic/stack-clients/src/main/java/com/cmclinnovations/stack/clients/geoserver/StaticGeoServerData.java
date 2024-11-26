package com.cmclinnovations.stack.clients.geoserver;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

public class StaticGeoServerData {

    private final String iconsDir = null;
    private final Optional<List<GeoserverOtherStaticFile>> otherFiles = Optional.empty();

    public String getIconsDir() {
        return iconsDir;
    }

    public List<GeoserverOtherStaticFile> getOtherFiles() {
        return otherFiles.orElse(Collections.emptyList());
    }

}
