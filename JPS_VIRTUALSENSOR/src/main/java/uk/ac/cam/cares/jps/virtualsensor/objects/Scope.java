package uk.ac.cam.cares.jps.virtualsensor.objects;

import java.time.Instant;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

/** 
 * Scope is the simulation domain for dispersion modelling
 * It contains the coordinates of the lower left and upper right corners along with the source CRS
 */

public class Scope {
	private Point upperCorner;
	private Point lowerCorner;
    private String srsname;

    public Scope(JSONObject region) {
    	this.upperCorner = new Point();
    	this.lowerCorner = new Point();
    	
    	this.upperCorner.setX(Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyUppercorner).get(Region.keyUpperx))));
    	this.upperCorner.setY(Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyUppercorner).get(Region.keyUppery))));
    	this.upperCorner.setSrsname(region.getString(Region.keySrsname));
    	
    	this.lowerCorner.setX(Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyLowercorner).get(Region.keyLowerx))));
    	this.lowerCorner.setY(Double.parseDouble(String.valueOf(region.getJSONObject(Region.keyLowercorner).get(Region.keyLowery))));
    	this.lowerCorner.setSrsname(region.getString(Region.keySrsname));
    	
    	this.srsname = region.getString(Region.keySrsname);
    }
    
    public Scope() {}
    
    public Scope(Scope sc) { // copy constructor
    	this.upperCorner = sc.getUpperCorner();
    	this.lowerCorner = sc.getLowerCorner();
    	this.srsname = sc.getSrsName();
    }
    
    public Point getUpperCorner() {
    	return this.upperCorner;
    }
    public void setUpperCorner(Point upperCorner) {
    	this.upperCorner = upperCorner;
    }
    public Point getLowerCorner() {
    	return this.lowerCorner;
    }
    public void setLowerCorner(Point lowerCorner) {
    	this.lowerCorner = lowerCorner;
    }
    
    public String getSrsName() {
        return this.srsname;
    }
    public void setSrsName(String srsname) {
    	this.srsname = srsname;
    }
    
    /**
     * Calculate scope centre from scope object.
     * index[0] = x coordinate, index[1] = y coordinate
     */
    public Point getScopeCentre() {
    	Point centre = new Point();
    	centre.setX((this.lowerCorner.getX() + this.upperCorner.getX())/2);
    	centre.setY((this.lowerCorner.getY() + this.upperCorner.getY())/2);
        centre.setSrsname(this.srsname);
        return centre;
    }

    /**
     * Returns the UTM zone using the scope centre
     */
    public String getUTMzone() {
        int zoneNumber;

        // obtain x y coordinates of the centre
        Point centre = this.getScopeCentre();

        // convert coordinates to latitude longitude
        centre.transform(CRSTransformer.EPSG_4326);

        // Determine zone based on longitude, the size of each UTM zone is 6 degrees
        zoneNumber = (int) Math.ceil((centre.getX() + 180)/6);

        // determine whether it's north or south of the equator
        String NS = null; 
        if (centre.getY()>0) {
            NS = "N";
        }
        else {
            NS = "S";
        }
        String UTMZone = String.valueOf(zoneNumber) + NS;
        return UTMZone;
    }

    /**
     * Obtain GMT time zone with coordinates of scope centre through Google API by default
     */
    public int getTimeZone() {
        int timeZone = 0;
        try {
            // obtain x y coordinates of the centre
            Point centre = getScopeCentre();
            centre.transform(CRSTransformer.EPSG_4326); // ensure it's in latlng

            // convert from s to hour
            timeZone = getTimeZoneFromGoogle(centre)/3600; 
        } catch (JPSRuntimeException e) {
            System.out.println(e.getMessage());
            System.out.println("WARNING: Google Map time zone API failure. Setting time zone to 0.");
            timeZone = 0;
        }
        return timeZone;
    }

    /** 
     * sends request to google API with centre of scope
     */
    private int getTimeZoneFromGoogle(Point centre) {
        String latlon = String.valueOf(centre.getY()) + "," + String.valueOf(centre.getX());
        URIBuilder builder = new URIBuilder().setScheme("https").setHost("maps.googleapis.com")
                .setPath("/maps/api/timezone/json");
        builder.setParameter("location", latlon);
        builder.setParameter("key", "AIzaSyBgm3-eMQauJ_dW4Cq66Hg9aP50jpp24rA");
        builder.setParameter("timestamp", String.valueOf(Instant.now().getEpochSecond()));
        int result;

        try {
            HttpGet request = new HttpGet(builder.build());
            String apiresult = AgentCaller.executeGet(request);
            JSONObject jo = new JSONObject(apiresult);
            result = (int) jo.get("rawOffset"); // GMT in seconds
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
        return result;
    }

    /** 
     * Convert scope to targetCRS
     */
    public void transform(String targetCRS) {
        // convert lower corner
    	this.lowerCorner.transform(targetCRS);
        // convert upper corner
    	this.upperCorner.transform(targetCRS);
    	this.srsname = targetCRS;
    }
    
    /**
     * Checks whether the given point is located within the scope
     * @param xy
     * @return
     */
    public boolean isWithinScope(Point p) {
    	boolean within = false;
    	
    	Point p_copy = new Point(p);
    	Scope sc_copy = new Scope(this);
    	
    	p_copy.transform(CRSTransformer.EPSG_4326);
    	sc_copy.transform(CRSTransformer.EPSG_4326);

    	if ((sc_copy.upperCorner.getX() >= p_copy.getX()) && (sc_copy.lowerCorner.getX() <= p_copy.getX()) 
    			&& (sc_copy.upperCorner.getY() >= p_copy.getY()) && (sc_copy.lowerCorner.getY() <= p_copy.getY())) {
    		within = true;
    	}
    	return within;
    }
}
