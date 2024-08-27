package com.cmclinnovations.stack.clients.ontop;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.jena.rdf.model.Model;
import org.eclipse.rdf4j.model.vocabulary.DCAT;
import org.eclipse.rdf4j.model.vocabulary.DCTERMS;
import org.eclipse.rdf4j.sparqlbuilder.constraint.propertypath.builder.PropertyPathBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ConstructQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.datasets.SparqlConstants;
import com.cmclinnovations.stack.clients.utils.ServiceEndpoint;
import com.cmclinnovations.stack.clients.utils.TempFile;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;

public class OntopClient extends ClientWithEndpoint<OntopEndpointConfig> {

    protected static final Logger LOGGER = LoggerFactory.getLogger(OntopClient.class);

    public static final String ONTOP_MAPPING_FILE = "ONTOP_MAPPING_FILE";
    public static final String ONTOP_ONTOLOGY_FILE = "ONTOP_ONTOLOGY_FILE";

    private static Map<String, OntopClient> instances = new HashMap<>();

    public static OntopClient getInstance() {
        return getInstance(EndpointNames.ONTOP);
    }

    public static OntopClient getInstance(String containerName) {
        return instances.computeIfAbsent(containerName, OntopClient::new);
    }

    private OntopClient(String containerName) {
        super(containerName, OntopEndpointConfig.class);
    }

    public void uploadOntology(String catalogNamespace, List<String> ontologyDatasets) {
        ConstructQuery query = Queries.CONSTRUCT();

        Variable subjectVar = SparqlBuilder.var("subject");
        Variable predicateVar = SparqlBuilder.var("predicate");
        Variable objectVar = SparqlBuilder.var("object");

        Variable serviceVar = SparqlBuilder.var("service");
        Variable serviceUrlVar = SparqlBuilder.var("serviceUrl");
        Variable ontologyDatasetVar = SparqlBuilder.var("ontologyDataset");

        ValuesPattern datasetValues = new ValuesPattern(ontologyDatasetVar,
                ontologyDatasets.stream().map(RdfObject.class::cast).collect(Collectors.toList()));

        query.construct(subjectVar.has(predicateVar, objectVar))
                .where(serviceVar
                        .has(PropertyPathBuilder.of(DCAT.SERVES_DATASET).then(DCTERMS.TITLE).build(),
                                ontologyDatasetVar)
                        .andIsA(SparqlConstants.BLAZEGRAPH_SERVICE)
                        .andHas(DCAT.ENDPOINT_URL, serviceUrlVar)
                        .and(new ServiceEndpoint(serviceUrlVar, subjectVar.has(predicateVar, objectVar))),
                        datasetValues);

        LOGGER.info("uploadOntology query: {}", query.getQueryString());

        Model model = BlazegraphClient.getInstance().getRemoteStoreClient(catalogNamespace)
                .executeConstruct(query.getQueryString());

        writeTurtleToFile(model);
    }

    public void updateOBDA(Path newMappingFilePath) {
        String containerId = getContainerId(getContainerName());
        Path ontopMappingFilePath = getEnvironmentVariable(containerId, ONTOP_MAPPING_FILE)
                .map(Path::of)
                .orElseThrow(() -> new RuntimeException("Environment variable '" + ONTOP_MAPPING_FILE
                        + " not set through Docker for '" + "ontop" + "' container."));

        try {
            SQLPPMappingImplementation mapping = new SQLPPMappingImplementation();

            if (fileExists(containerId, ontopMappingFilePath.toString())) {

                if (null == newMappingFilePath) {
                    // A mapping file already exists and no new one has been passed to be added.
                    return;
                }
                try (TempFile localTempOntopMappingFilePath = SQLPPMappingImplementation
                        .createTempOBDAFile(ontopMappingFilePath);
                        OutputStream outputStream = Files.newOutputStream(localTempOntopMappingFilePath.getPath())) {
                    outputStream.write(retrieveFile(containerId, ontopMappingFilePath.toString()));
                    mapping.addMappings(localTempOntopMappingFilePath.getPath());
                }
            }

            if (null != newMappingFilePath) {
                mapping.addMappings(newMappingFilePath);
            }
            try (TempFile localTempOntopMappingFilePath = SQLPPMappingImplementation
                    .createTempOBDAFile(ontopMappingFilePath)) {
                mapping.serialize(localTempOntopMappingFilePath.getPath());

                sendFilesContent(containerId,
                        Map.of(ontopMappingFilePath.getFileName().toString(),
                                Files.readAllBytes(localTempOntopMappingFilePath.getPath())),
                        ontopMappingFilePath.getParent().toString());
            }
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to write out combined Ontop mapping file '" + ontopMappingFilePath + "'.", ex);
        }
    }

    private void writeTurtleToFile(Model model) {
        String containerId = getContainerId(getContainerName());
        Path ontopOntologyFilePath = getEnvironmentVariable(containerId, ONTOP_ONTOLOGY_FILE)
                .map(Path::of)
                .orElseThrow(() -> new RuntimeException("Environment variable '" + ONTOP_ONTOLOGY_FILE
                        + " not set through Docker for '" + "ontop" + "' container."));

        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            model.write(outputStream, "TURTLE");
            sendFilesContent(containerId,
                    Map.of(ontopOntologyFilePath.getFileName().toString(), outputStream.toByteArray()),
                    ontopOntologyFilePath.getParent().toString());
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

    }
}
