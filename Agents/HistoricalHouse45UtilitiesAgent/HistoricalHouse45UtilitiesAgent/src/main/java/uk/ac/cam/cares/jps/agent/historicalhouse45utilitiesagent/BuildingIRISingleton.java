package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

/**
 * A singleton that gets and sets the required IRIs if they exist in the knowledge graph.
 *
 * @author qhouyee
 */
public class BuildingIRISingleton {
    // Key fields
    private String ONTOCITYGML_BUILDING_IRI;
    private String BUILDING_IRI;
    private String GROUND_FLOOR_IRI;
    private String FIRST_FLOOR_IRI;
    private String ATTIC_IRI;
    private String ELEC_METER_IRI;
    private String WATER_METER_IRI;
    private String OIL_METER_IRI;
    // Static variable reference to ensure single instance of type BuildingIRISingleton
    private static BuildingIRISingleton single_instance = null;

    // Private Constructor
    private BuildingIRISingleton() {
        ONTOCITYGML_BUILDING_IRI = "";
        BUILDING_IRI = "";
        GROUND_FLOOR_IRI = "";
        FIRST_FLOOR_IRI = "";
        ATTIC_IRI = "";
        ELEC_METER_IRI = "";
        WATER_METER_IRI = "";
        OIL_METER_IRI = "";
    }

    /**
     * Retrieves the singleton if it exists, else it will create a new instance.
     *
     * @return the singleton instance.
     */
    public static BuildingIRISingleton getInstance() {
        if (single_instance == null)
            single_instance = new BuildingIRISingleton();
        return single_instance;
    }

    public String getOntoCityGmlBuildingIri() {
        return ONTOCITYGML_BUILDING_IRI;
    }

    public void setOntoCityGmlBuildingIri(String iri) {
        this.ONTOCITYGML_BUILDING_IRI = iri;
    }

    public String getBuildingIri() {
        return BUILDING_IRI;
    }

    public void setBuildingIri(String iri) {
        this.BUILDING_IRI = iri;
    }

    public String getGroundFloorIri() {
        return GROUND_FLOOR_IRI;
    }

    public void setGroundFloorIri(String iri) {
        this.GROUND_FLOOR_IRI = iri;
    }

    public String getFirstFloorIri() {
        return FIRST_FLOOR_IRI;
    }

    public void setFirstFloorIri(String iri) {
        this.FIRST_FLOOR_IRI = iri;
    }

    public String getAtticIri() {
        return ATTIC_IRI;
    }

    public void setAtticIri(String iri) {
        this.ATTIC_IRI = iri;
    }

    public String getElecMeterIri() {
        return ELEC_METER_IRI;
    }

    public void setElecMeterIri(String iri) {
        this.ELEC_METER_IRI = iri;
    }

    public String getWaterMeterIri() {
        return WATER_METER_IRI;
    }

    public void setWaterMeterIri(String iri) {
        this.WATER_METER_IRI = iri;
    }

    public String getOilMeterIri() {
        return OIL_METER_IRI;
    }

    public void setOilMeterIri(String iri) {
        this.OIL_METER_IRI = iri;
    }
}
