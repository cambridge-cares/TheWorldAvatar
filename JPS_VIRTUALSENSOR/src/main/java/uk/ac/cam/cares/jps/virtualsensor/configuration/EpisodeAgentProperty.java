package uk.ac.cam.cares.jps.virtualsensor.configuration;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Properties;

import org.springframework.core.io.ClassPathResource;

public class EpisodeAgentProperty {
	private String hpcServerLoginUserName;
	private String hpcServerLoginUserPassword;
	private String agentClass;
	private String agentWorkspacePrefix;
	private String agentCompletedJobsSpacePrefix;
	private String agentFailedJobsSpacePrefix;
	private String hpcAddress;
	private String inputFileName;
	private String inputFileExtension;
	private String checkPointFileExtension;
	private String jsonInputFileName;
	private String jsonFileExtension;
	private String slurmScriptFileName;
	private String executableFile;
	private String outputFileName;
	private String outputFileExtension;
	private int maxNumberOfHPCJobs;
	private int agentInitialDelayToStartJobMonitoring;
	private int agentPeriodicActionInterval;

    private Properties props = null;

    public void initProperties() {
		if (props == null) {
			try {
				String propfile = Paths.get("dispersion-agent.properties").toString();
				InputStream inputStream = new ClassPathResource(propfile).getInputStream();

				props = new Properties();
				props.load(inputStream);

				hpcServerLoginUserName = props.getProperty("hpc.server.login.user.name");
				hpcServerLoginUserPassword = props.getProperty("hpc.server.login.user.password");
				agentClass = props.getProperty("agent.class");
				agentWorkspacePrefix = agentClass;
				agentCompletedJobsSpacePrefix = props.getProperty("agent.completed.job.space.prefix");
				agentFailedJobsSpacePrefix = props.getProperty("agent.failed.job.space.prefix");
				hpcAddress = props.getProperty("hpc.address");
				inputFileName = props.getProperty("input.file.name");
				inputFileExtension = props.getProperty("input.file.extension");
				checkPointFileExtension = props.getProperty("chk.point.file.extension");
				jsonInputFileName = props.getProperty("json.input.file.name");
				jsonFileExtension = props.getProperty("json.file.extension");
				slurmScriptFileName = props.getProperty("slurm.script.file.name");
				executableFile = props.getProperty("executable.file");
				outputFileName = props.getProperty("output.file.name");
				outputFileExtension = props.getProperty("output.file.extension");
				maxNumberOfHPCJobs = Integer.parseInt(props.getProperty("max.number.of.hpc.jobs"));
				agentInitialDelayToStartJobMonitoring = Integer.parseInt(props.getProperty("agent.initial.delay.to.start"));
				agentPeriodicActionInterval = Integer.parseInt(props.getProperty("agent.periodic.action.interval"));
			} catch (IOException e) {
				
			}
		}
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
}

