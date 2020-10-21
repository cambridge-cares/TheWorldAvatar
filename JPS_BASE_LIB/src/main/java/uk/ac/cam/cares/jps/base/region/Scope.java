package uk.ac.cam.cares.jps.base.region;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Scope {
    /** Scope is the simulation domain for dispersion modelling
     * It contains the coordinates of the lower left and upper right corners along with the source CRS
     */
    private double upperx;
    private double uppery;
    private double lowerx;
    private double lowery;
    private String sourceCRS;

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

    public String getUTMzone() {
        /**
         * Returns the UTM zone using the scope centre
         */
        int zoneNumber;

        // obtain x y coordinates of the centre
        double [] centre = this.getScopeCentre();

        // convert coordinates to latitude longitude
        if (!this.sourceCRS.equals(CRSTransformer.EPSG_4326)) {
            centre = CRSTransformer.transform(this.sourceCRS, CRSTransformer.EPSG_4326, centre);
        }

        // Determine zone based on longitude, the size of each UTM zone is 6 degrees
        zoneNumber = (int) Math.ceil((centre[0] + 180)/6);

        // determine whether it's north or south of the equator
        String NS = null; 
        if (centre[1]>0) {
        	NS = "N";
        }
        else {
        	NS = "S";
        }
        String UTMZone = String.valueOf(zoneNumber) + NS;
        return UTMZone;
    }
}
