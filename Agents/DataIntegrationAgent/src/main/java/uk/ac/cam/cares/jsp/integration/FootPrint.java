package uk.ac.cam.cares.jsp.integration;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Workflow of footprint extraction:
 * 1. thematic-- true: query ground surface and extract footprint
 * 2. thematic-- false: identify surface types, then extract footprint
 */
public class FootPrint {

    List<GeoObject3D> allObject3D = new ArrayList<>();
    boolean thematic = false;
    String surfaceType = null;

    protected void proFootPrint(String[] config, String thematicParams, String surfaceType) throws SQLException {

        GeoObject3D object3D = new GeoObject3D();
        object3D.setConfig(config);
        this.allObject3D = object3D.getObject3D();   
        this.surfaceType = surfaceType;     
        if(thematicParams.equals("true")){
            this.thematic = true;
            object3D.extractPrint_thematic(allObject3D, this.surfaceType);
        }else{
            this.thematic = false;
            object3D.identifySurface(allObject3D, this.surfaceType);
        }       
    }
}
