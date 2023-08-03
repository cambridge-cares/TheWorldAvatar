package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class PostgresDataSubset extends DataSubset {

    @JsonProperty(defaultValue = PostGISClient.DEFAULT_SCHEMA_NAME)
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
        loadData(dataSubsetDirectory, database, parent.baseIRI());
        runSQLPostProcess(database);
    }

    public abstract void loadData(Path dataSubsetDir, String database, String baseIRI);

    public void runSQLPostProcess(String database) {
        if (null != sql) {
            sql = handleFileValues(sql);
            PostGISClient.getInstance().getRemoteStoreClient(database).executeUpdate(sql);
        }
    }

    public String handleFileValues(String sql) {
        if (null != sql && sql.startsWith("@")) {
            String sqlFile = sql.substring(1);
            try {
                sql = Files.readString(Path.of(sqlFile));
            } catch (IOException ex) {
                throw new RuntimeException(
                        "Failed to read SQL file '" + Path.of(sqlFile).toAbsolutePath().toString()
                                + "' for data subset '"
                                + getName() + "'.",
                        ex);
            }
        }
        return sql;
    }
}
