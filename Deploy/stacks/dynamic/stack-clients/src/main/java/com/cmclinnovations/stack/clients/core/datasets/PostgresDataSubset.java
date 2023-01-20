package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class PostgresDataSubset extends DataSubset {

    @JsonProperty(defaultValue = "public")
    private String schema;
    private String table;
    @JsonProperty
    private String sql;

    @Override
    public String getName() {
        String name = super.getName();
        return (null != name) ? name : table;
    }

    public String getSchema() {
        return schema;
    }

    public String getTable() {
        return (null != table) ? table : super.getName();
    }

    @Override
    public boolean usesPostGIS() {
        return !isSkip();
    }

    @Override
    void loadInternal(Dataset parent) {
        String database = parent.getDatabase();
        Path dataSubsetDirectory = parent.getDirectory().resolve(this.getSubdirectory());
        loadData(dataSubsetDirectory, database);
        runSQLPostProcess(database);
    }

    public abstract void loadData(Path dataSubsetDir, String database);

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

            PostGISClient.getInstance().getRemoteStoreClient(database).executeUpdate(sql);
        }
    }
}
