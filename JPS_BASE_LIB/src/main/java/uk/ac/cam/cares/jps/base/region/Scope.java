package uk.ac.cam.cares.jps.base.region;

import org.json.JSONObject;

public class Scope {
    /** Scope is the simulation domain for dispersion modelling
     * It contains the coordinates of the lower left and upper right corners along with the source CRS
     */
    double upperx;
    double uppery;
    double lowerx;
    double lowery;
    String sourceCRS;

    // constructor
    public Scope(JSONObject region) {
        /** 
         * Create a scope from region JSON Object
         */
        this.upperx = Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyUppercorner).get(Region.keyUpperx)));
        this.uppery = Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyUppercorner).get(Region.keyUppery)));
        this.lowerx = Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyLowercorner).get(Region.keyLowerx)));
        this.lowery = Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyLowercorner).get(Region.keyLowery)));
        this.sourceCRS = region.getString(Region.keySrsname);
    }

    public double[] getScopeCentre() {
        /**
         * Calculate scope centre from scope object.
         * index[0] = x coordinate, index[1] = y coordinate
         */
        double [] centreXY = new double[] {(this.lowerx + this.upperx)/2, (this.lowery + this.uppery)/2};
        return centreXY;
    }
}
