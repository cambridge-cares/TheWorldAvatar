package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

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
    private Map<String, Map<String, String>> tableColumnDataType;
    private Map<String, Map<String, String>> tableColumnUdtType;
    private Map<String, Map<String, String>> tableColumnGeometryType;
    private Map<String, Map<String, Integer>> tableColumnSrid;
    private Map<String, Map<String, List<String>>> tableDataTypeColumn;

    static Map<Class<?>, String> javaClassToPgDataType = new HashMap<>();
    static {
        javaClassToPgDataType.put(Double.class, "double precision");
        javaClassToPgDataType.put(Integer.class, "integer");
    }

    TimeSeriesDatabaseMetadata() {
        tableColumnDataType = new HashMap<>();
        tableColumnUdtType = new HashMap<>();
        tableColumnGeometryType = new HashMap<>();
        tableColumnSrid = new HashMap<>();
        tableDataTypeColumn = new HashMap<>();
    }

    void addDataType(String table, String column, String dataType) {
        tableColumnDataType.computeIfAbsent(table, k -> new HashMap<>());
        tableColumnDataType.get(table).put(column, dataType);
    }

    void addUdtType(String table, String column, String udtType) {
        tableColumnUdtType.computeIfAbsent(table, k -> new HashMap<>());
        tableColumnUdtType.get(table).put(column, udtType);
    }

    void addGeometryType(String table, String column, String geometryType) {
        tableColumnGeometryType.computeIfAbsent(table, k -> new HashMap<>());
        tableColumnGeometryType.get(table).put(column, geometryType);
    }

    void addSrid(String table, String column, int srid) {
        tableColumnSrid.computeIfAbsent(table, k -> new HashMap<>());
        tableColumnSrid.get(table).put(column, srid);
    }

    /**
     * returns table and column with udt_type = 'geometry'
     */
    List<List<String>> getGeometryRows() {
        List<List<String>> rows = new ArrayList<>();
        tableColumnUdtType.entrySet().forEach(tableEntry -> {
            tableEntry.getValue().entrySet().forEach(columnEntry -> {
                if (columnEntry.getValue().contentEquals("geometry")) {
                    List<String> row = new ArrayList<>();
                    row.add(tableEntry.getKey());
                    row.add(columnEntry.getKey());
                    rows.add(row);
                }
            });
        });
        return rows;
    }

    /**
     * the complexity of this operation can scale very badly if there are many
     * different combinations to match because for each class, there are always
     * two possible choices to match. The appropriate choice can be different for
     * each class, i.e. Java choice 1 matches Database choice 1 in a case, Java
     * choice 1 matches Database choice 2 for a different class, and vice versa
     * 
     * On the Java side, they are:
     * 1) DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT,
     * newClassToAdd).getTypeName();
     * 2) DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT,
     * newClassToAdd).getSQLDataType().getName();
     * 
     * on the database side, they can be either (for non geometry types)
     * 1) udt_name from information_schema.columns
     * 2) data_type from information_schema.columns
     * 
     * As you can see, the number of combinations is N squared from both sides.
     * 
     * @param classes
     * @param srid
     * @return
     */
    String getExistingSuitableTable(List<Class<?>> classes, Integer srid) {
        String suitableTable = null;
        Map<String, List<List<String>>> existingClassSetsMap = getExistingClassSetsMap();
        if (existingClassSetsMap.isEmpty()) {
            return suitableTable;
        }

        if (srid == null) {
            srid = 0;
        }
        List<List<String>> classSetsToAdd = getSqlClassSetsToAdd(classes, srid);

        boolean breakLoop = false;
        for (List<String> classSetToAdd : classSetsToAdd) {
            if (breakLoop) {
                break;
            }
            for (Entry<String, List<List<String>>> entry : existingClassSetsMap.entrySet()) {
                if (breakLoop) {
                    break;
                }

                List<List<String>> existingClassSets = entry.getValue();

                for (List<String> existingClassSet : existingClassSets) {
                    if (existingClassSet.containsAll(classSetToAdd)) {
                        suitableTable = entry.getKey();
                        breakLoop = true;
                        break;
                    }
                }
            }
        }

        return suitableTable;
    }

    String getExistingSuitableColumn(String table, Class<?> clas, Integer srid) {
        String sqlDataType;
        if (Geometry.class.isAssignableFrom(clas)) {
            if (srid == null) {
                srid = 0;
            }
            sqlDataType = String.format("%s,%d", clas.getSimpleName(), srid).toUpperCase();
        } else {
            sqlDataType = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, clas).getTypeName();
            if (!tableDataTypeColumn.get(table).containsKey(sqlDataType)) {
                // alternative, one of these is guaranteed to match due to the way class sets
                // were matched
                sqlDataType = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, clas).getSQLDataType()
                        .getName();
            }
        }
        return tableDataTypeColumn.get(table).get(sqlDataType).remove(0);
    }

    List<List<String>> getSqlClassSetsToAdd(List<Class<?>> classes, int srid) {
        List<List<String>> overallList = new ArrayList<>();
        overallList.add(new ArrayList<>());
        return getSqlClassSetsToAddRecursively(classes, srid, overallList);
    }

    List<List<String>> getSqlClassSetsToAddRecursively(List<Class<?>> classes, int srid,
            List<List<String>> overallList) {
        Class<?> newClassToAdd = classes.get(0);

        if (!Geometry.class.isAssignableFrom(newClassToAdd)) {
            String typeName = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, newClassToAdd).getTypeName();
            String sqlDataType = DefaultDataType.getDataType(TimeSeriesRDBClient.DIALECT, newClassToAdd)
                    .getSQLDataType().getName();

            if (!typeName.contentEquals(sqlDataType)) {
                List<List<String>> overallListCopy = new ArrayList<>();
                overallList.stream().forEach(list -> {
                    List<String> newList = new ArrayList<>();
                    list.stream().forEach(newList::add);
                    overallListCopy.add(newList);
                });

                overallList.stream().forEach(list -> {
                    list.add(typeName);
                });

                overallListCopy.stream().forEach(list -> {
                    list.add(sqlDataType);
                });

                overallList.addAll(overallListCopy);
            } else {
                overallList.stream().forEach(list -> list.add(typeName));
            }

        } else {
            overallList.stream().forEach(list -> {
                list.add(String.format("%s,%d", newClassToAdd.getSimpleName(), srid).toUpperCase());
            });
        }

        if (classes.size() == 1) {
            return overallList;
        } else {
            return getSqlClassSetsToAddRecursively(classes.subList(1, classes.size()), srid, overallList);
        }
    }

    /**
     * and initialise tableDataTypeColumn
     * 
     * @return
     */
    Map<String, List<List<String>>> getExistingClassSetsMap() {
        Map<String, List<List<String>>> tableToClassSetsMap = new HashMap<>();

        // table loop
        tableColumnUdtType.keySet().stream().forEach(table -> { // table loop
            List<List<String>> overallList = new ArrayList<>();
            overallList.add(new ArrayList<>());
            tableToClassSetsMap.put(table, overallList);

            tableDataTypeColumn.computeIfAbsent(table, k -> new HashMap<>());

            tableColumnUdtType.get(table).keySet().stream().forEach(column -> { // column loop
                if (tableColumnUdtType.get(table).get(column).contentEquals("geometry")) {
                    // e.g. Point,4326
                    String sqlType = String.format("%s,%d", tableColumnGeometryType.get(table).get(column),
                            tableColumnSrid.get(table).get(column));
                    overallList.stream().forEach(list -> list.add(sqlType));

                    tableDataTypeColumn.get(table).computeIfAbsent(sqlType, k -> new ArrayList<>());
                    tableDataTypeColumn.get(table).get(sqlType).add(column);
                } else {
                    List<List<String>> overallListCopy = new ArrayList<>();
                    overallList.stream().forEach(list -> {
                        List<String> newList = new ArrayList<>();
                        list.stream().forEach(newList::add);
                        overallListCopy.add(newList);
                    });

                    String dataType = tableColumnDataType.get(table).get(column);
                    overallList.stream().forEach(list -> list.add(dataType));

                    String udtType = tableColumnUdtType.get(table).get(column);
                    overallListCopy.stream().forEach(list -> list.add(udtType));

                    overallList.addAll(overallListCopy);

                    tableDataTypeColumn.get(table).computeIfAbsent(dataType, k -> new ArrayList<>());
                    tableDataTypeColumn.get(table).get(dataType).add(column);
                    tableDataTypeColumn.get(table).computeIfAbsent(udtType, k -> new ArrayList<>());
                    tableDataTypeColumn.get(table).get(udtType).add(column);
                }
            });
        });

        return tableToClassSetsMap;
    }
}
