package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Dataset {

    private String name;
    private Path datasetDirectory;

    @JsonProperty(defaultValue = "postgres")
    private String database;

    private List<DataSubset> dataSubsets = new ArrayList<>();

    public String getName() {
        return name;
    }

    public Path getDatasetDirectory() {
        return datasetDirectory;
    }

    public String getDatabase() {
        return database;
    }

    public List<DataSubset> getDataSubsets() {
        return dataSubsets;
    }

    public void loadData() {
        PostGISClient postGISClient = new PostGISClient();
        postGISClient.createDatabase(database);
        dataSubsets.stream().filter(Predicate.not(DataSubset::getSkip)).forEach(
                subset -> subset.loadData(Path.of("/inputs", "data").resolve(datasetDirectory).toString(), database));
    }

}
