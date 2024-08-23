package com.cmclinnovations.stack.clients.geoserver;

import it.geosolutions.geoserver.rest.encoder.utils.ElementUtils;
import it.geosolutions.geoserver.rest.encoder.utils.XmlElement;

abstract class GeoServerElementUtils extends ElementUtils {
    public static boolean nodeIsSet(XmlElement element, String nodeName) {
        return ElementUtils.contains(element.getRoot(), nodeName, 1) != null;
    }
}
