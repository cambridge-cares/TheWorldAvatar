package uk.ac.cam.cares.jps.agent.heat.objects;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;

public class HeatSource {

    public String iri = null;
    // Carbon emissions in units of tons per year
    public double carbonEmission = 0.0;
    // Heat emissions in units of megawatts.
    public double heatEmission = 0.0;
    public static final Integer numberOfSecondsPerYear = 365 * 24 * 60 * 60;

    // Type of heat source
    public HeatSourceType sourceType = null;

    // Geospatial properties
    public String ontoCityGmlIri = null;
    public Point location = null;
    public Geometry footPrint = null;

    public HeatSource(String Iri, Double carbonEmission) {
        this.iri = Iri;
        this.carbonEmission = carbonEmission;
    }

    public HeatSource(String Iri, String ontoCityGmlIri, Double carbonEmission) {
        this.iri = Iri;
        this.ontoCityGmlIri = ontoCityGmlIri;
        this.carbonEmission = carbonEmission;
    }

    public void calculateHeat() {
        double efficiency = 0.5;
        double carbonEmissionIndex = 63.0;
        heatEmission = carbonEmission * (1.0 - efficiency) * (1e3) * (1e9)
                / (carbonEmissionIndex * numberOfSecondsPerYear * (1e6));
        heatEmission = Math.min(heatEmission, 510.0);
    }

}
