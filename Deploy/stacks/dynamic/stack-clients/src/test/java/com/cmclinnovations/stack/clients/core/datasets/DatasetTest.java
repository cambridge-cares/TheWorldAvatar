package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.Collections;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;

import com.cmclinnovations.stack.clients.utils.BlazegraphContainer;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;

class DatasetTest {

    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

    @Test
    void testDeserialisingInjected() {
        ObjectMapper mapper = getObjectMapper();

        @SuppressWarnings("null")
        Dataset dataset = Assertions.assertDoesNotThrow(() -> mapper.readValue("{}", Dataset.class));

        Assertions.assertEquals("injectedName", dataset.getName());
    }

    @Test
    void testDeserialisingTrivial() {
        ObjectMapper mapper = getObjectMapper();

        String name = "test";

        @SuppressWarnings("null")
        Dataset dataset = Assertions.assertDoesNotThrow(() -> mapper.readValue("{\"name\":\"test\"}", Dataset.class));

        Assertions.assertEquals(name, dataset.getName());
        Assertions.assertEquals(name, dataset.getDescription());
        Assertions.assertEquals(Path.of("/inputs", "data", name), dataset.getDirectory());
        Assertions.assertEquals(name, dataset.getNamespace());
        Assertions.assertEquals(name, dataset.getDatabase());
        Assertions.assertEquals(name, dataset.getWorkspaceName());
        Assertions.assertFalse(dataset.isSkip());
        Assertions.assertEquals(SparqlConstants.DEFAULT_BASE_IRI, dataset.baseIRI());
        Assertions.assertEquals(Collections.emptyList(), dataset.getExternalDatasetNames());
        Assertions.assertEquals(Collections.emptyList(), dataset.getOntologyDatasetNames());
        Assertions.assertEquals(Collections.emptyList(), dataset.getReferencedDatasets());
        Assertions.assertEquals(Collections.emptyList(), dataset.getOntopMappings());
        Assertions.assertEquals(Collections.emptyList(), dataset.getGeoserverStyles());
        Assertions.assertFalse(dataset.hasStaticGeoServerData());
    }

    @Test
    void testDeserialisingNames() {
        ObjectMapper mapper = getObjectMapper();

        @SuppressWarnings("null")
        Dataset dataset = Assertions.assertDoesNotThrow(() -> mapper.readValue(
                "{\"name\":\"test\",\"description\":\"Test description\",\"datasetDirectory\":\"datasetDirectory\",\"namespace\":\"namespace\",\"database\":\"database\",\"workspace\":\"workspace\",\"skip\":true}",
                Dataset.class));

        Assertions.assertEquals("test", dataset.getName());
        Assertions.assertEquals("Test description", dataset.getDescription());
        Assertions.assertEquals(Path.of("/inputs", "data", "datasetDirectory"), dataset.getDirectory());
        Assertions.assertEquals("namespace", dataset.getNamespace());
        Assertions.assertEquals("database", dataset.getDatabase());
        Assertions.assertEquals("workspace", dataset.getWorkspaceName());
        Assertions.assertTrue(dataset.isSkip());
    }

    private ObjectMapper getObjectMapper() {
        ObjectMapper mapper = JsonHelper.getMapper();

        InjectableValues.Std iv = new InjectableValues.Std();
        iv.addValue(Dataset.NAME_KEY, "injectedName");
        mapper.setInjectableValues(iv);
        return mapper;
    }

}
