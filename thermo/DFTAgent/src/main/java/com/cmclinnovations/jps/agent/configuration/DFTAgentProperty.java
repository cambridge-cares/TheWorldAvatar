package com.cmclinnovations.jps.agent.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:dft-agent.properties")
public class DFTAgentProperty {
	@Value("${hpc.server.login.user.name}")
	private String hpcServerLoginUserName;

	@Value("${hpc.server.login.user.password}")
	private String hpcServerLoginUserPassword;

	@Value("${agent.class}")
	private String agentClass;
	
	@Value("${agent.class}")
	private String agentWorkspacePrefix;

	@Value("${agent.completed.job.space.prefix}")
	private String agentCompletedJobsSpacePrefix;

	@Value("${agent.failed.job.space.prefix}")
	private String agentFailedJobsSpacePrefix;
	
	@Value("${hpc.address}")
	private String hpcAddress;

	@Value("${input.file.name}")
	private String inputFileName;

	@Value("${input.file.extension}")
	private String inputFileExtension;

	@Value("${chk.point.file.extension}")
	private String checkPointFileExtension;

	@Value("${json.input.file.name}")
	private String jsonInputFileName;
	
	@Value("${json.file.extension}")
	private String jsonFileExtension;

	@Value("${slurm.script.file.name}")
	private String slurmScriptFileName;
	
	@Value("${executable.file}")
	private String executableFile;

	@Value("${job.preprint.directive}")
	private String jobPreprintDirective;

	@Value("${output.file.name}")
	private String outputFileName;

	@Value("${output.file.extension}")
	private String outputFileExtension;

	@Value("${kg.url.to.upload.result.via.json.input}")
	private String kgURLToUploadResultViaJsonInput;

	@Value("${rdf4j.repository.ontospecies}")
	private String rdf4jRepositoryOntoSpecies;
	
	@Value("${max.number.of.hpc.jobs}")
	private int maxNumberOfHPCJobs;

	@Value("${agent.initial.delay.to.start}")
	private int agentInitialDelayToStartJobMonitoring;
	
	@Value("${agent.periodic.action.interval}")
	private int agentPeriodicActionInterval;
	
	@Value("${thermo.agent.http.request.first.part}")
	private String thermoAgentHttpRequestFirstPart;

	@Value("${rdf4j.ontokin.repository.query.iri}")
	private String ontoKinRepositoryIRI;
	
	@Value("${rdf4j.server.url}")
	private String rdf4jServerURL;

	
	@Value("${rdf4j.ontokin.repository.id}")
	private String ontoKinRepositoryID;

	public String getThermoAgentHttpRequestFirstPart() {
		return thermoAgentHttpRequestFirstPart;
	}

	public String getOntoKinRepositoryIRI() {
		return ontoKinRepositoryIRI;
	}

	public String getRdf4jServerURL() {
		return rdf4jServerURL;
	}

	public String getOntoKinRepositoryID() {
		return ontoKinRepositoryID;
	}

	public String getHpcServerLoginUserName() {
		return hpcServerLoginUserName;
	}

	public String getHpcServerLoginUserPassword() {
		return hpcServerLoginUserPassword;
	}

	public String getAgentClass() {
		return agentClass;
	}

	public String getAgentWorkspacePrefix() {
		return agentWorkspacePrefix;
	}

	public String getAgentCompletedJobsSpacePrefix() {
		return agentCompletedJobsSpacePrefix;
	}

	public String getAgentFailedJobsSpacePrefix() {
		return agentFailedJobsSpacePrefix;
	}

	public String getHpcAddress() {
		return hpcAddress;
	}

	public String getInputFileName() {
		return inputFileName;
	}

	public String getInputFileExtension() {
		return inputFileExtension;
	}

	public String getCheckPointFileExtension() {
		return checkPointFileExtension;
	}

	public String getJsonInputFileName() {
		return jsonInputFileName;
	}

	public String getJsonFileExtension() {
		return jsonFileExtension;
	}

	public String getSlurmScriptFileName() {
		return slurmScriptFileName;
	}

	public String getExecutableFile() {
		return executableFile;
	}

	public String getJobPreprintDirective() {
		return jobPreprintDirective;
	}

	public String getOutputFileName() {
		return outputFileName;
	}

	public String getOutputFileExtension() {
		return outputFileExtension;
	}

	public String getKgURLToUploadResultViaJsonInput() {
		return kgURLToUploadResultViaJsonInput;
	}

	public String getRdf4jRepositoryOntoSpecies() {
		return rdf4jRepositoryOntoSpecies;
	}

	public int getMaxNumberOfHPCJobs() {
		return maxNumberOfHPCJobs;
	}

	public int getAgentInitialDelayToStartJobMonitoring() {
		return agentInitialDelayToStartJobMonitoring;
	}

	public int getAgentPeriodicActionInterval() {
		return agentPeriodicActionInterval;
	}
}

