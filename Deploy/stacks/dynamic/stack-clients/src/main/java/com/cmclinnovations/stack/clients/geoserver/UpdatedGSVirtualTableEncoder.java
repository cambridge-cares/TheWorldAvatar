package com.cmclinnovations.stack.clients.geoserver;

import java.util.List;

import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.VTGeometryEncoder;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.VTParameterEncoder;

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

    public void setParameters(List<VTParameterEncoder> paramEncList) {
        paramEncList.stream().forEach(paramEnc -> addVirtualTableParameter(paramEnc));
    }
}
