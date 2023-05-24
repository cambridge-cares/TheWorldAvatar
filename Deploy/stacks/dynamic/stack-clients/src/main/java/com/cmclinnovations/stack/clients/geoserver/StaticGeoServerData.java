package com.cmclinnovations.stack.clients.geoserver;

import java.util.Collections;
import java.util.List;

public class StaticGeoServerData {

    private String iconsDir;
    private List<String> otherFiles;

    public String getIconsDir() {
        return iconsDir;
    }

    public List<String> getOtherFiles() {
        return (null != otherFiles) ? otherFiles : Collections.emptyList();
    }

}
