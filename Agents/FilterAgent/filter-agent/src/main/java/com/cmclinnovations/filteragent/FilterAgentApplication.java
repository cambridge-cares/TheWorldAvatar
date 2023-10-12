package com.cmclinnovations.filteragent;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.cmclinnovations.filteragent.objects.IriObject;
import com.cmclinnovations.filteragent.objects.Substitution;
import com.cmclinnovations.filteragent.utils.FileUtils;
import com.cmclinnovations.filteragent.utils.ReplacementUtils;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.fasterxml.jackson.core.JsonProcessingException;
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

	public static void main(String[] args) {
		SpringApplication.run(FilterAgentApplication.class, args);
	}

	@PostConstruct
	private void init() {
		blazegraphClient = BlazegraphClient.getInstance();
		remoteStoreClient = blazegraphClient.getRemoteStoreClient(EnvConfig.DEFAULT_NAMESPACE);
	}

	@GetMapping(value = "/filter")
	public String filter(@RequestParam("subs") String config) throws JsonProcessingException {
		LOGGER.info("config: {}", config);
		List<Substitution> substitutionRequest = OBJECT_MAPPER.readValue(config,
				new TypeReference<List<Substitution>>() {
				});
		LOGGER.info("substitutionRequest: {}", substitutionRequest);

		String query = FileUtils.readStringFromResources("queries/query.sparql");
		query = ReplacementUtils.userReplacements(substitutionRequest, query);
		query = blazegraphClient.filterQuery(query);
		LOGGER.info("query: {}", query);

		List<IriObject> iriObjects = OBJECT_MAPPER.readValue(
				QueryClient.executeQuery(remoteStoreClient, query).toString(), new TypeReference<List<IriObject>>() {
				});
		List<String> iriList = iriObjects.stream().map(IriObject::iri).collect(Collectors.toList());
		return iriList.toString();
	}
}
