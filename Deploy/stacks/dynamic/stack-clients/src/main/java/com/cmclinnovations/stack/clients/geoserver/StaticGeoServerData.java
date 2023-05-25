package com.cmclinnovations.stack.clients.geoserver;

import java.util.Collections;
import java.util.List;

public class StaticGeoServerData {

    private String iconsDir;
    private List<GeoserverOtherStaticFile> otherFiles;

    public String getIconsDir() {
        return iconsDir;
    }

    public List<GeoserverOtherStaticFile> getOtherFiles() {
        return (null != otherFiles) ? otherFiles : Collections.emptyList();
    }

}
