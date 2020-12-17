package uk.ac.cam.cares.jps.agent.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:kinetics-agent.properties")
public class KineticsAgentProperty {

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

	@Value("${json.input.file.name}")
	private String jsonInputFileName;

	@Value("${json.file.extension}")
	private String jsonFileExtension;

	@Value("${slurm.script.file.name}")
	private String slurmScriptFileName;

	@Value("${output.file.name}")
	private String outputFileName;

	@Value("${output.file.extension}")
	private String outputFileExtension;

	@Value("${max.number.of.hpc.jobs}")
	private int maxNumberOfHPCJobs;

	@Value("${agent.initial.delay.to.start}")
	private int agentInitialDelayToStartJobMonitoring;

	@Value("${agent.periodic.action.interval}")
	private int agentPeriodicActionInterval;

	/**
	 * Location of python scripts (should contain 'agkin', 'simulation_templates', and 'venv' directories).
	 */
	@Value("${agent.scripts.location}")
	private String agentScriptsLocation;

	/**
	 * File that contains the results of a job.
	 */
	@Value("${reference.output.json.file}")
	private String referenceOutputJsonFile;

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

	public String getJsonInputFileName() {
		return jsonInputFileName;
	}

	public String getJsonFileExtension() {
		return jsonFileExtension;
	}

	public String getSlurmScriptFileName() {
		return slurmScriptFileName;
	}

	public String getOutputFileName() {
		return outputFileName;
	}

	public String getOutputFileExtension() {
		return outputFileExtension;
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

	/**
	 * Returns the Location of python scripts.
	 *
	 * @return python scripts directory
	 */
	public String getAgentScriptsLocation() {
		String userHome = System.getProperty("user.home");
		if (userHome.contains("\\")) {
			userHome = userHome.replace("\\", "/");
		}

		if (!userHome.endsWith("/") && !agentScriptsLocation.startsWith("/")) {
			return userHome.concat("/" + agentScriptsLocation);
		} else {
			return userHome.concat(agentScriptsLocation);
		}
	}

	/**
	 * Returns the name of reference output JSON file.
	 *
	 * @return
	 */
	public String getReferenceOutputJsonFile() {
		return referenceOutputJsonFile;
	}
}
