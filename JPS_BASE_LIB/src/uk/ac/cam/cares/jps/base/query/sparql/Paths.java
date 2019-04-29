package uk.ac.cam.cares.jps.base.query.sparql;

public interface Paths extends Prefixes {
	
	String CLASS = "CLASS";

	String[] PVALNUMVAL = new String[] {OCPSYST, "hasValue", OCPSYST, "numericalValue"};
	
	String[] PGISCOORDX = {OCPSPAC, "hasGISCoordinateSystem", OCPSPAC, "hasProjectedCoordinate_x", OCPSYST, "hasValue", OCPSYST, "numericalValue"};
	String[] PGISCOORDY = {OCPSPAC, "hasGISCoordinateSystem", OCPSPAC, "hasProjectedCoordinate_y", OCPSYST, "hasValue", OCPSYST, "numericalValue"};
}
