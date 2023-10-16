package com.cmclinnovations.filteragent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.filteragent.objects.IriObject;
import com.cmclinnovations.filteragent.utils.ReplacementUtils;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import jakarta.annotation.PostConstruct;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@SpringBootApplication
@RestController
public class FilterAgentApplication {
	BlazegraphClient blazegraphClient;
	RemoteStoreClient remoteStoreClient;

	private static final Logger LOGGER = LogManager.getLogger(FilterAgentApplication.class);
	private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
	private static final String QUERY_DIR = "/inputs/queries";

	public static void main(String[] args) {
		SpringApplication.run(FilterAgentApplication.class, args);
	}

	@PostConstruct
	private void init() {
		blazegraphClient = BlazegraphClient.getInstance();
	}

	@GetMapping(value = "/filter")
	public String filter(@RequestParam("subs") String substitutionString, @RequestParam("query") String queryFile,
			@RequestParam("namespace") String namespace) throws IOException {
		LOGGER.info("config: {}", substitutionString);
		Map<String, String> subsMap = OBJECT_MAPPER.readValue(substitutionString,
				new TypeReference<Map<String, String>>() {
				});
		LOGGER.info("substitutionRequest: {}", subsMap);

		String query = Files.readString(Path.of(QUERY_DIR).resolve(queryFile + ".sparql"));

		query = ReplacementUtils.userReplacements(subsMap, query);
		query = blazegraphClient.filterQuery(query);
		LOGGER.info("query: {}", query);

		remoteStoreClient = blazegraphClient.getRemoteStoreClient(namespace);
		List<IriObject> iriObjects = OBJECT_MAPPER.readValue(
				QueryClient.executeQuery(remoteStoreClient, query).toString(), new TypeReference<List<IriObject>>() {
				});
		List<String> iriList = iriObjects.stream().map(IriObject::iri).collect(Collectors.toList());
		return iriList.toString();
	}
}
