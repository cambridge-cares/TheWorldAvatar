package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;

/**
 * A class holding the required information to support the enforcement of the Organisation data model.
 * This class cannot be accessed outside the subpackage, and is intended to be a data model for holding system information.
 *
 * @author qhouyee
 */
public class TechnicalSystem {
    private final String systemName;
    private final Map<String, String[]> measures = new HashMap<>();

    /**
     * Standard Constructor. This will store the metadata retrieved from the SPARQL query.
     *
     * @param name          Name of the system.
     * @param measureName   Name of the measure associated with the system.
     * @param unit          Measure unit symbol
     * @param measureIri    Corresponding dataIRI of the measure associated with the system.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    protected TechnicalSystem(String name, String measureName, String unit, String measureIri, String timeSeriesIri) {
        this.systemName = name;
        this.addMeasure(measureName, unit, measureIri, timeSeriesIri);
    }

    /**
     * Adds the measure to be stored within this instance.
     *
     * @param measureName   Name of the measure associated with the system.
     * @param unit          Measure unit symbol.
     * @param measureIri    Corresponding dataIRI of the measure associated with the system.
     * @param timeSeriesIri Corresponding time series IRI of the measure.
     */
    protected void addMeasure(String measureName, String unit, String measureIri, String timeSeriesIri) {
        String[] iris = new String[5];
        iris[0] = measureName;
        iris[1] = measureIri;
        iris[2] = timeSeriesIri;
        // Only append a unit if the inserted value is not null
        if (unit != null) iris[3] = unit;
        iris[4] = StringHelper.SYSTEM_KEY;
        this.measures.put(measureName, iris);
    }

    /**
     * A getter method for system name.
     */
    protected String getName() {return this.systemName;}

    /**
     * A getter method to retrieve all measure metadata associated with this system.
     *
     * @returns A queue containing all system information. Within the array, first position is measure name; Second position is the dataIRI; Third position is time series IRI; Fourth position is unit if available; Fifth position is system type.
     */
    protected Queue<String[]> getData() {
        Queue<String[]> measureInfo = new ArrayDeque<>();
        for (Map.Entry<String, String[]> measure : this.measures.entrySet()) {
            measureInfo.offer(measure.getValue());
        }
        return measureInfo;
    }
}
