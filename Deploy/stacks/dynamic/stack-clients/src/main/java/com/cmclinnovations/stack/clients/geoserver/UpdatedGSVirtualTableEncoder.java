package com.cmclinnovations.stack.clients.geoserver;

import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.VTGeometryEncoder;

public class UpdatedGSVirtualTableEncoder extends GSVirtualTableEncoder {

    private static final String ESCAPE_SQL = "escapeSql";

    public void setEscapeSql(boolean escapeSql) {
        add(ESCAPE_SQL, Boolean.toString(escapeSql));
    }

    public void setKeyColumn(String keycolumn) {
        addKeyColumn(keycolumn);
    }

    public void setGeometry(VTGeometryEncoder geomEnc) {
        addVirtualTableGeometry(geomEnc);
    }
}
