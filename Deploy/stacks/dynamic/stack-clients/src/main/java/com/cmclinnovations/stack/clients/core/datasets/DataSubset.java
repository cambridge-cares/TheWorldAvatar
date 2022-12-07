package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({ @Type(value = Tabular.class, names = { "Tabular", "tabular" }),
        @Type(value = Vector.class, names = { "Vector", "vector" }),
        @Type(value = Raster.class, names = { "Raster", "raster" }) })
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

    public abstract void loadData(GDALClient gdalClient, String dataSubsetDir, String database);

    public abstract void createLayer(GeoServerClient geoServerClient, String dataSubsetDir, String workspaceName,
            String database);

    public void runSQLPostProcess(PostGISClient postGISClient, String database) {
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

            postGISClient.executeUpdate(database, sql);
        }
    }
}
