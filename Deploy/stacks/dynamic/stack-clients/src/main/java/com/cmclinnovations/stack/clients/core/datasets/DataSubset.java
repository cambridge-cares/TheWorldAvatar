package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(Tabular.class), @Type(Vector.class), @Type(Raster.class), @Type(CityDB.class) })
public abstract class DataSubset {

    private String name;
    private String subdirectory;

    @JsonProperty(defaultValue = "public")
    private String schema;
    private String table;
    @JsonProperty
    private String sql;

    @JsonProperty
    private boolean skip;

    public String getName() {
        return (null != name) ? name : table;
    }

    public String getSubdirectory() {
        return null != subdirectory ? subdirectory : "";
    }

    public String getSchema() {
        return schema;
    }

    public String getTable() {
        return (null != table) ? table : name;
    }

    public boolean getSkip() {
        return skip;
    }

    public abstract void loadData(String dataSubsetDir, String database);

    public abstract void createLayer(String dataSubsetDir, String workspaceName,
            String database);

    public void runSQLPostProcess(String database) {
        if (null != sql) {

            if (sql.startsWith("@")) {
                String sqlFile = sql.substring(1);
                try {
                    sql = Files.readString(Path.of(sqlFile));
                } catch (IOException ex) {
                    throw new RuntimeException(
                            "Failed to read SQL file '" + sqlFile + "' for data subset '" + getName() + "'.", ex);
                }
            }

            PostGISClient.getInstance().executeUpdate(database, sql);
        }
    }
}
