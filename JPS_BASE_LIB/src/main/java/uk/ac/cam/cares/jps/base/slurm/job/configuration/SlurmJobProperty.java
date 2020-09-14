package uk.ac.cam.cares.jps.base.slurm.job.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all properties of DFT Agent provided in the dft-agent.properties file.
 * 
 * This will empower users to use DFT Agent, if some properties, e.g.<br>
 * HPC address and knowledge-graph repository addresses,<br>
 * change at a later stage, without changing the source code.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:slurm-job.properties")
public class SlurmJobProperty {
	@Value("${agent.class}")
	private String agentClass;
	
	@Value("${agent.work.space.prefix}")
	private String agentWorkspacePrefix;

	@Value("${agent.completed.job.space.prefix}")
	private String agentCompletedJobsSpacePrefix;
	
	@Value("${hpc.address}")
	private String hpcAddress;

	@Value("${agent.host.address}")
	private String agentHostAddress;

	@Value("${input.file.name}")
	private String inputFileName;

	@Value("${input.file.extension}")
	private String inputFileExtension;

	@Value("${chk.point.file.extension}")
	private String checkPointFileExtension;

	@Value("${json.file.extension}")
	private String jsonFileExtension;

	@Value("${json.input.file.name}")
	private String jsonInputFileName;

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

	@Value("${agent.hosting.server.operating.system}")
	private String agentHostingServerOperatingSystem;

	@Value("${kg.url.to.upload.result.via.json.input}")
	private String kgURLToUploadResultViaJsonInput;

	@Value("${agent.rdf4j.server.url}")
	private String rdf4jServerUrl;

	@Value("${rdf4j.repository.ontospecies}")
	private String rdf4jRepositoryOntoSpecies;
	
	@Value("${max.number.of.hpc.jobs}")
	private int maxNumberOfHPCJobs;

	@Value("${agent.initial.delay.to.start}")
	private int agentInitialDelayToStartJobMonitoring;
	
	@Value("${agent.periodic.action.interval}")
	private int agentPeriodicActionInterval;
	
	public String getAgentClass() {
		return agentClass;
	}

	public String getAgentWorkspacePrefix() {
		return agentWorkspacePrefix;
	}
	
	public String getAgentCompletedJobsSpacePrefix() {
		return agentCompletedJobsSpacePrefix;
	}

	public String getHpcAddress() {
		return hpcAddress;
	}

	public String getAgentHostAddress() {
		return agentHostAddress;
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

	public String getJsonFileExtension() {
		return jsonFileExtension;
	}

	public String getJsonInputFileName() {
		return jsonInputFileName;
	}

	public String getSlurmScriptFileName() {
		return slurmScriptFileName;
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

	public String getAgentHostingServerOperatingSystem() {
		return agentHostingServerOperatingSystem;
	}

	public String getKgURLToUploadResultViaJsonInput() {
		return kgURLToUploadResultViaJsonInput;
	}

	public String getRdf4jServerUrl() {
		return rdf4jServerUrl;
	}

	public String getRdf4jRepositoryOntoSpecies() {
		return rdf4jRepositoryOntoSpecies;
	}

	public String getExecutableFile() {
		return executableFile;
	}

	public void setExecutableFile(String executableFile) {
		this.executableFile = executableFile;
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

	public void setAgentClass(String agentClass) {
		this.agentClass = agentClass;
	}

	public void setAgentWorkspacePrefix(String agentWorkspacePrefix) {
		this.agentWorkspacePrefix = agentWorkspacePrefix;
	}

	public void setAgentCompletedJobsSpacePrefix(String agentCompletedJobsSpacePrefix) {
		this.agentCompletedJobsSpacePrefix = agentCompletedJobsSpacePrefix;
	}

	public void setHpcAddress(String hpcAddress) {
		this.hpcAddress = hpcAddress;
	}

	public void setAgentHostAddress(String agentHostAddress) {
		this.agentHostAddress = agentHostAddress;
	}

	public void setInputFileName(String inputFileName) {
		this.inputFileName = inputFileName;
	}

	public void setInputFileExtension(String inputFileExtension) {
		this.inputFileExtension = inputFileExtension;
	}

	public void setCheckPointFileExtension(String checkPointFileExtension) {
		this.checkPointFileExtension = checkPointFileExtension;
	}

	public void setJsonFileExtension(String jsonFileExtension) {
		this.jsonFileExtension = jsonFileExtension;
	}

	public void setJsonInputFileName(String jsonInputFileName) {
		this.jsonInputFileName = jsonInputFileName;
	}

	public void setSlurmScriptFileName(String slurmScriptFileName) {
		this.slurmScriptFileName = slurmScriptFileName;
	}

	public void setJobPreprintDirective(String jobPreprintDirective) {
		this.jobPreprintDirective = jobPreprintDirective;
	}

	public void setOutputFileName(String outputFileName) {
		this.outputFileName = outputFileName;
	}

	public void setOutputFileExtension(String outputFileExtension) {
		this.outputFileExtension = outputFileExtension;
	}

	public void setAgentHostingServerOperatingSystem(String agentHostingServerOperatingSystem) {
		this.agentHostingServerOperatingSystem = agentHostingServerOperatingSystem;
	}

	public void setKgURLToUploadResultViaJsonInput(String kgURLToUploadResultViaJsonInput) {
		this.kgURLToUploadResultViaJsonInput = kgURLToUploadResultViaJsonInput;
	}

	public void setRdf4jServerUrl(String rdf4jServerUrl) {
		this.rdf4jServerUrl = rdf4jServerUrl;
	}

	public void setRdf4jRepositoryOntoSpecies(String rdf4jRepositoryOntoSpecies) {
		this.rdf4jRepositoryOntoSpecies = rdf4jRepositoryOntoSpecies;
	}

	public void setMaxNumberOfHPCJobs(int maxNumberOfHPCJobs) {
		this.maxNumberOfHPCJobs = maxNumberOfHPCJobs;
	}

	public void setAgentInitialDelayToStartJobMonitoring(int agentInitialDelayToStartJobMonitoring) {
		this.agentInitialDelayToStartJobMonitoring = agentInitialDelayToStartJobMonitoring;
	}

	public void setAgentPeriodicActionInterval(int agentPeriodicActionInterval) {
		this.agentPeriodicActionInterval = agentPeriodicActionInterval;
	}
}
