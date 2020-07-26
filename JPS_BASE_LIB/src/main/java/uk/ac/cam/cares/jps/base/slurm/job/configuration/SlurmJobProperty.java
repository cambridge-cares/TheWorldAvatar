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
	
	public String getAgentClass() {
		return agentClass;
	}

	public String getAgentWorkspacePrefix() {
		return agentWorkspacePrefix;
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
}
