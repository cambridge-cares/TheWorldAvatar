package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.core.QueryElement;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class CopyDatasetQueryTest {
    @Test
    void testGetConstructQuery() throws IOException, URISyntaxException {
        checkQuery("constructQuery.sparql",
                CopyDatasetQuery.getConstructQuery(List.of("datasetA", "datasetB")));
    }

    @Test
    void testGetConstructQueryWithCatalogService() throws IOException, URISyntaxException {
        checkQuery("constructQueryWithCatalogService.sparql",
                CopyDatasetQuery.getConstructQuery(List.of("datasetA", "datasetB"), Rdf.iri("http://catalog1")));
    }

    @Test
    void testGetInsertQuery() throws IOException, URISyntaxException {
        checkQuery("insertQuery.sparql",
                CopyDatasetQuery.getInsertQuery(List.of("datasetA", "datasetB")));
    }

    @Test
    void testGetInsertQueryWithCatalogService() throws IOException, URISyntaxException {
        checkQuery("insertQueryWithCatalogService.sparql",
                CopyDatasetQuery.getInsertQuery(List.of("datasetA", "datasetB"), Rdf.iri("http://catalog1")));
    }

    private void checkQuery(String filename, QueryElement query) throws IOException, URISyntaxException {
        Assertions.assertEquals(
                Files.readString(
                        Path.of(CopyDatasetQueryTest.class.getResource("copydatasetquery/" + filename).toURI())),
                query.getQueryString());
    }

}
