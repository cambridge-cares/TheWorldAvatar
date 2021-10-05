package uk.ac.cam.cares.jps.agent.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * This class reads all inputs from the modsmoo-agent.properties file. 
 * 
 * @author jb2197
 *
 */
@Configuration
@PropertySource("classpath:modsdatadriven-agent.properties")
public class MoDSMooAgentProperty {
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
	
	@Value("${output.file.name}")
	private String outputFileName;

	@Value("${output.file.extension}")
	private String outputFileExtension;

	@Value("${json.input.file.name}")
	private String jsonInputFileName;
	
	@Value("${json.file.extension}")
	private String jsonFileExtension;

	@Value("${slurm.script.file.name}")
	private String slurmScriptFileName;

	@Value("${rdf4j.server.url}")
	private String rdf4jServerURL;

	@Value("${rdf4j.repository.ontospecies}")
	private String rdf4jRepositoryOntoSpecies;
	
	@Value("${rdf4j.repository.ontokin}")
	private String rdf4jRepositoryOntoKin;
	
	@Value("${rdf4j.repository.ontochemexp}")
	private String rdf4jRepositoryOntoChemExp;
	
	@Value("${max.number.of.hpc.jobs}")
	private int maxNumberOfHPCJobs;

	@Value("${agent.initial.delay.to.start}")
	private int agentInitialDelayToStartJobMonitoring;
	
	@Value("${agent.periodic.action.interval}")
	private int agentPeriodicActionInterval;
	
	@Value("${kinetics.folder.path}")
	private String kineticsFolderPath;
	
	@Value("${kinetics.executable.name}")
	private String kineticsExecutableName;

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

	public String getOutputFileName() {
		return outputFileName;
	}

	public String getOutputFileExtension() {
		return outputFileExtension;
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

	public String getRdf4jServerURL() {
		return rdf4jServerURL;
	}

	public String getRdf4jRepositoryOntoSpecies() {
		return rdf4jRepositoryOntoSpecies;
	}

	public String getRdf4jRepositoryOntoKin() {
		return rdf4jRepositoryOntoKin;
	}

	public String getRdf4jRepositoryOntoChemExp() {
		return rdf4jRepositoryOntoChemExp;
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

	public String getKineticsFolderPath() {
		return kineticsFolderPath;
	}

	public String getKineticsExecutableName() {
		return kineticsExecutableName;
	}
	
}
