package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.Optional;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class PostgresDataSubset extends DataSubset {

    private static final Logger logger = LoggerFactory.getLogger(PostgresDataSubset.class);

    @JsonProperty(defaultValue = PostGISClient.DEFAULT_SCHEMA_NAME)
    private final Optional<String> schema = Optional.empty();
    private final Optional<String> table = Optional.empty();
    @JsonProperty
    private final Optional<String> sql = Optional.empty();

    @Override
    public String getName() {
        return Stream.of(name, table)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .findFirst()
                .orElseGet(super::getName);
    }

    public String getSchema() {
        return schema.orElse(PostGISClient.DEFAULT_SCHEMA_NAME);
    }

    public String getTable() {
        return Stream.of(table, name)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .findFirst()
                .orElse(null);
    }

    @Override
    public boolean usesPostGIS() {
        return !isSkip();
    }

    @Override
    void loadInternal(Dataset parent) {
        String database = parent.getDatabase();
        Optional<Path> subdirectory = this.getSubdirectory();
        if (subdirectory.isPresent()) {
            Path dataSubsetDirectory = parent.getDirectory().resolve(subdirectory.get());
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
        sql.map(JsonHelper::handleFileValues)
                .ifPresent(query -> PostGISClient.getInstance().getRemoteStoreClient(database).executeUpdate(query));
    }

}
