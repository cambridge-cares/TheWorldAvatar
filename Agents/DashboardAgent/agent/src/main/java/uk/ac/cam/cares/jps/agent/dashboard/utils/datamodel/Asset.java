package uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;

/**
 * A class holding the required information to support the enforcement of the Facility data model.
 * This class cannot be accessed outside the subpackage, and is intended to be a data model for holding asset information.
 *
 * @author qhouyee
 */
public class Asset {
    private final String ASSET_NAME;
    private final String ASSET_TYPE;
    private final Map<String, String[]> MEASURES = new HashMap<>();

    /**
     * Standard Constructor. This will store the metadata retrieved from the SPARQL query.
     *
     * @param assetType     The asset type.
     * @param measureName   Name of the measure associated with the asset.
     * @param measureIri    Corresponding dataIRI of the measure associated with the asset.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    protected Asset(String assetName, String assetType, String measureName, String measureIri, String timeSeriesIri) {
        this.ASSET_NAME = assetName;
        this.ASSET_TYPE = assetType;
        this.addMeasure(measureName, measureIri, timeSeriesIri);
    }

    /**
     * Adds the measure to be stored within this instance.
     *
     * @param measureName   Name of the measure associated with the asset.
     * @param measureIri    Corresponding dataIRI of the measure associated with the asset.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    protected void addMeasure(String measureName, String measureIri, String timeSeriesIri) {
        String[] iris = new String[3];
        iris[0] = measureName;
        iris[1] = measureIri;
        iris[2] = timeSeriesIri;
        this.MEASURES.put(measureName, iris);
    }

    /**
     * A getter method for asset name.
     */
    protected String getAssetName() {return this.ASSET_NAME;}

    /**
     * A getter method for asset type.
     */
    protected String getAssetType() {return this.ASSET_TYPE;}

    /**
     * A getter method to retrieve all measures and their information.
     *
     * @returns A queue containing all measure information. Within the array, first position is measure name; Second position is the dataIRI; Third position is time series IRI.
     */
    protected Queue<String[]> getMeasureInfo() {
        Queue<String[]> measureInfo = new ArrayDeque<>();
        for (String measure : this.MEASURES.keySet()) {
            measureInfo.offer(this.MEASURES.get(measure));
        }
        return measureInfo;
    }
}
