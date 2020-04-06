package com.cmclinnovations.jps.agent.caller.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:dft-agent-caller.properties")
public class DFTAgentCallerProperty {
	@Value("${sparql.endpoint.ontospecies}")
	private String endpointOntoSpecies;

	@Value("${sparql.endpoint.ontocompchem}")
	private String endpointOntoCompChem;

	@Value("${job.property.label}")
	private String jobPropertyLabel;

	@Value("${job.levelOfTheory.property.label}")
	private String jobLevelOfTheoryPropertyLabel;

	@Value("${job.levelOfTheory}")
	private String jobLevelOfTheory;

	@Value("${job.keyword.property.label}")
	private String jobKeywordPropertyLabel;

	@Value("${job.keyword}")
	private String jobKeyword;

	@Value("${job.algorithmChoice.property.label}")
	private String jobAlgorithmChoicePropertyLabel;

	@Value("${job.algorithmChoice}")
	private String jobAlgorithmChoice;

	@Value("${job.speciesIRI.property.label}")
	private String jobSpeciesIRIPropertyLabel;

	@Value("${http.request.first.part}")
	private String httpRequestFirstPart;
	
	public String getEndpointOntoSpecies() {
		return endpointOntoSpecies;
	}

	public String getEndpointOntoCompChem() {
		return endpointOntoCompChem;
	}

	public String getJobPropertyLabel() {
		return jobPropertyLabel;
	}

	public String getJobLevelOfTheoryPropertyLabel() {
		return jobLevelOfTheoryPropertyLabel;
	}

	public String getJobLevelOfTheory() {
		return jobLevelOfTheory;
	}

	public String getJobKeywordPropertyLabel() {
		return jobKeywordPropertyLabel;
	}

	public String getJobKeyword() {
		return jobKeyword;
	}

	public String getJobAlgorithmChoicePropertyLabel() {
		return jobAlgorithmChoicePropertyLabel;
	}

	public String getJobAlgorithmChoice() {
		return jobAlgorithmChoice;
	}

	public String getJobSpeciesIRIPropertyLabel() {
		return jobSpeciesIRIPropertyLabel;
	}

	public String getHttpRequestFirstPart() {
		return httpRequestFirstPart;
	}
}
