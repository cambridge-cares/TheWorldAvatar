package com.cmclinnovations.slurm.job.configuration;

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

	@Value("${status.file.extension}")
	private String statusFileExtension;

	@Value("${json.file.extension}")
	private String jsonFileExtension;

	@Value("${status.file.name}")
	private String statusFileName;

	@Value("${json.input.file.name}")
	private String jsonInputFileName;

	@Value("${slurm.script.file.name}")
	private String slurmScriptFileName;

	@Value("${job.preprint.directive}")
	private String jobPreprintDirective;

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

	public String getStatusFileExtension() {
		return statusFileExtension;
	}

	public String getJsonFileExtension() {
		return jsonFileExtension;
	}

	public String getStatusFileName() {
		return statusFileName;
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
}
