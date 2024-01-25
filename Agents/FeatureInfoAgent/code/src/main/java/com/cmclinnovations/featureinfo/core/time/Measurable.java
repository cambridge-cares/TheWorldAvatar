package com.cmclinnovations.featureinfo.core.time;

import com.cmclinnovations.featureinfo.config.ConfigEntry;

/**
 * Represents a single entity that has a linked time series.
 * 
 * The IRI for this measurable should correspond to a single column of
 * dependent (Y) values in the relational database, whilst its parent
 * time series IRI corresponds to a single column of independent (X)
 * values in the same RDB.
 * 
 * This current representation supports multiple measurables sharing a
 * single parent time series, but not a single measurable with multiple
 * parent time series.
 */
public class Measurable {
    
    /**
     * IRI of the measurable entity.
     */
    private final String entityIRI;

    /**
     * User-facing name.
     */
    private final String name;

    /**
     * Unit, can be null.
     */
    private final String unit;

    /**
     * Configuration entry responsible for this instance.
     */
    private final ConfigEntry configEntry;

    /**
     * IRI for parent time series.
     */
    private String timeseriesIRI;

    /**
     * Create a new Measureable instance.
     * 
     * @param entityIRI IRI of measurable entity.
     * @param name User facing name of entity.
     * @param configEntry Configuration entry responsible for this instance.
     * @param unit Optional unit (can be null).
     */
    public Measurable(String entityIRI, String name, String unit, ConfigEntry configEntry) {
        this.entityIRI = entityIRI;
        this.name = name;
        this.configEntry = configEntry;
        this.unit = unit;
    }

    /**
     * Sets the IRI of the parent time series.
     * 
     * @param timeseriesIRI IRI of parent time series
     */
    public void setParentTimeSeries(String timeseriesIRI) {
        this.timeseriesIRI = timeseriesIRI;
    }

    /**
     * Get parent timeseries IRI.
     * 
     * @return parent timeseries IRI.
     */
    public String getTimeSeriesIRI() {
        return this.timeseriesIRI;
    }

    /**
     * Get the IRI of the measureable entity.
     * 
     * @return measureable entity IRI.
     */
    public String getEntityIRI() {
        return this.entityIRI;
    }

    /**
     * Get user facing name.
     * 
     * @return name.
     */
    public String getName() {
        return this.name;
    }

    /**
     * Returns configuration entry that resulted in this instance.
     * 
     * @return configuration entry.
     */
    public ConfigEntry getConfigEntry() {
        return this.configEntry;
    }

    /**
     * Get unit.
     * 
     * @return unit.
     */
    public String getUnit() {
        return this.unit;
    }

}    
// End of class.