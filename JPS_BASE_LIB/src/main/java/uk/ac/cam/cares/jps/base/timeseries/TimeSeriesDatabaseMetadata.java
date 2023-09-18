package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.jooq.impl.DefaultDataType;
import org.postgis.Geometry;

/**
 * used internally in TimeSeriesRDBClient to keep track of the data type for
 * each column
 */
class TimeSeriesDatabaseMetadata {
    // column name set within each table is unique
    // one data type can have multiple columns, e.g. 2 columns in a table may be of
    // the same type
    private Map<String, String> columnDataType;
    private Map<String, String> columnUdtName;
    private Map<String, String> columnGeometryType;
    private Map<String, Integer> columnSrid;
    private Map<String, List<String>> dataTypeColumn;

    TimeSeriesDatabaseMetadata() {
        columnDataType = new HashMap<>();
        columnUdtName = new HashMap<>();
        columnGeometryType = new HashMap<>();
        columnSrid = new HashMap<>();
        dataTypeColumn = new HashMap<>();
    }

    void addDataType(String column, String dataType) {
        columnDataType.put(column, dataType);
    }

    void addUdtName(String column, String udtName) {
        columnUdtName.put(column, udtName);
    }

    void addGeometryType(String column, String geometryType) {
        columnGeometryType.put(column, geometryType);
    }

    void addSrid(String column, int srid) {
        columnSrid.put(column, srid);
    }

    /**
     * returns list of columns with udt_type = 'geometry'
     */
    List<String> getGeometryColumns() {
        List<String> geometryColumns = new ArrayList<>();
        columnUdtName.entrySet().forEach(entry -> {
            if (entry.getValue().contentEquals("geometry")) {
                geometryColumns.add(entry.getKey());
            }
        });
        return geometryColumns;
    }

    /**
     * returns true if existing time_series_data has all the columns needed for this
     * new time series
     * 
     * @param classes
     * @param srid
     * @return
     */
    List<Boolean> hasMatchingColumn(List<Class<?>> classes, Integer srid) {
        List<List<String>> existingClassSets = getExistingClassSets();
        if (existingClassSets.isEmpty()) {
            return classes.stream().map(v -> false).collect(Collectors.toList());
        }

        if (srid == null) {
            srid = 0;
        }
        List<List<String>> classSetsToAdd = getSqlClassSetsToAdd(classes, srid);

        List<Boolean> hasMatchingColumn = new ArrayList<>();
        for (List<String> classSetToAdd : classSetsToAdd) {
            boolean match = false;
            for (List<String> existingClassSet : existingClassSets) {
                if (match) {
                    break;
                }
                for (String existingClass : existingClassSet) {
                    if (classSetToAdd.contains(existingClass)) {
                        match = true;
                        break;
                    }
                }
            }
            hasMatchingColumn.add(match);
        }

        return hasMatchingColumn;
    }

    String getExistingSuitableColumn(Class<?> clas, Integer srid) {
        String sqlDataType;
        if (Geometry.class.isAssignableFrom(clas)) {
            if (srid == null) {
                srid = 0;
            }
            sqlDataType = String.format("%s,%d", clas.getSimpleName(), srid).toUpperCase();
        } else {
            sqlDataType = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, clas).getTypeName();
            if (!dataTypeColumn.containsKey(sqlDataType)) {
                // alternative, one of these is guaranteed to match due to the way class sets
                // were matched
                sqlDataType = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, clas).getSQLDataType()
                        .getName();
            }
        }
        return dataTypeColumn.get(sqlDataType).remove(0);
    }

    List<List<String>> getSqlClassSetsToAdd(List<Class<?>> classes, int srid) {
        List<List<String>> classSets = new ArrayList<>();
        classes.stream().forEach(newClassToAdd -> {
            List<String> classSet = new ArrayList<>();
            if (!Geometry.class.isAssignableFrom(newClassToAdd)) {
                String typeName = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, newClassToAdd).getTypeName();
                String sqlDataType = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, newClassToAdd)
                        .getSQLDataType().getName();

                if (!typeName.contentEquals(sqlDataType)) {
                    classSet.add(typeName);
                    classSet.add(sqlDataType);
                } else {
                    classSet.add(typeName);
                }

            } else {
                classSet.add(String.format("%s,%d", newClassToAdd.getSimpleName(), srid).toUpperCase());
            }
            classSets.add(classSet);
        });

        return classSets;
    }

    /**
     * and initialise tableDataTypeColumn
     * 
     * @return
     */
    List<List<String>> getExistingClassSets() {
        List<List<String>> classSets = new ArrayList<>();

        columnUdtName.keySet().stream().forEach(column -> { // column loop
            List<String> classSet = new ArrayList<>();
            if (columnUdtName.get(column).contentEquals("geometry")) {
                // e.g. Point,4326
                String sqlType = String.format("%s,%d", columnGeometryType.get(column),
                        columnSrid.get(column));
                classSet.add(sqlType);

                dataTypeColumn.computeIfAbsent(sqlType, k -> new ArrayList<>());
                dataTypeColumn.get(sqlType).add(column);
            } else {
                String dataType = columnDataType.get(column);
                String udtName = columnUdtName.get(column);

                dataTypeColumn.computeIfAbsent(dataType, k -> new ArrayList<>());
                dataTypeColumn.get(dataType).add(column);
                dataTypeColumn.computeIfAbsent(udtName, k -> new ArrayList<>());
                dataTypeColumn.get(udtName).add(column);

                classSet.add(dataType);
                classSet.add(udtName);
            }

            classSets.add(classSet);
        });

        return classSets;
    }
}
