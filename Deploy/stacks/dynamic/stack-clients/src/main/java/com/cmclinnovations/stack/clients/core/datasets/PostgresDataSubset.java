package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class PostgresDataSubset extends DataSubset {
    
    private static final Logger logger = LoggerFactory.getLogger(PostgresDataSubset.class);

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
        Path subdirectory = this.getSubdirectory();
        if (null != subdirectory) {
            Path dataSubsetDirectory = parent.getDirectory().resolve(subdirectory);
            PostGISClient.getInstance().resetSchema(database);
            loadData(dataSubsetDirectory, database, parent.baseIRI());
        } else {
            logger.warn("No Subdirectory specified, Continuing with SQL process and creation without data upload");
        }
        runSQLPostProcess(database);
        PostGISClient.getInstance().resetSchema(database);
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
