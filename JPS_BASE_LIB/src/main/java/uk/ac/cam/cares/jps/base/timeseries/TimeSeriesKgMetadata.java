package uk.ac.cam.cares.jps.base.timeseries;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.EnumMap;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

public class TimeSeriesKgMetadata {
    private String timeSeriesIri;
    private List<String> dataIriList;
    private List<Class<?>> dataClassList;
    private String timeUnit;
    private Iri timeSeriesType;
    private Duration duration;
    private ChronoUnit durationUnit;

    // used to reinitialise deleted time series
    private double durationValue;
    private String durationUnitIri;
    private String durationIri;

    static final EnumMap<Type, Iri> typeToIriMap;
    static {
        typeToIriMap = new EnumMap<>(Type.class);
        typeToIriMap.put(Type.AVERAGE, TimeSeriesSparql.AVERAGE_TIMESERIES);
        typeToIriMap.put(Type.STEPWISECUMULATIVE, TimeSeriesSparql.STEPWISE_CUMULATIVE_TIMESERIES);
        typeToIriMap.put(Type.CUMULATIVETOTAL, TimeSeriesSparql.CUMULATIVE_TOTAL_TIMESERIES);
        typeToIriMap.put(Type.INSTANTANEOUS, TimeSeriesSparql.INSTANTANEOUS_TIMESERIES);
        typeToIriMap.put(Type.GENERAL, TimeSeriesSparql.TIMESERIES);
    }

    TimeSeriesKgMetadata(String iri) {
        timeSeriesIri = iri;
    }

    String getTimeSeriesIri() {
        return timeSeriesIri;
    }

    void setDataIriList(List<String> dataIriList) {
        this.dataIriList = dataIriList;
    }

    List<String> getDataIriList() {
        return dataIriList;
    }

    void setDataClassList(List<Class<?>> dataClassList) {
        this.dataClassList = dataClassList;
    }

    List<Class<?>> getDataClassList() {
        return dataClassList;
    }

    void setTimeUnit(String timeUnit) {
        this.timeUnit = timeUnit;
    }

    String getTimeUnit() {
        return timeUnit;
    }

    void setTimeSeriesType(Type timeSeriesType) {
        this.timeSeriesType = typeToIriMap.get(timeSeriesType);
    }

    void setTimeSeriesType(Iri timeSeriesType) {
        this.timeSeriesType = timeSeriesType;
    }

    Iri getTimeSeriesType() {
        return timeSeriesType;
    }

    void setDuration(Duration duration) {
        this.duration = duration;
    }

    Duration getDuration() {
        return duration;
    }

    void setDurationUnit(ChronoUnit unit) {
        durationUnit = unit;
    }

    ChronoUnit getDurationUnit() {
        return durationUnit;
    }

    void setDurationValue(double durationValue) {
        this.durationValue = durationValue;
    }

    double getDurationValue() {
        return durationValue;
    }

    void setDurationUnitIri(String durationUnitIri) {
        this.durationUnitIri = durationUnitIri;
    }

    String getDurationUnitIri() {
        return durationUnitIri;
    }

    void setDurationIri(String durationIri) {
        this.durationIri = durationIri;
    }

    String getDurationIri() {
        return durationIri;
    }
}
