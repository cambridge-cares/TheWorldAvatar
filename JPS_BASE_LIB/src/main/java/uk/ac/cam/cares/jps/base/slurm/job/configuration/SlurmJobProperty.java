package uk.ac.cam.cares.jps.base.slurm.job.configuration;

/**
 * These properties must be set by all agents that will run Slurm jobs<br>
 * with this API.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class SlurmJobProperty {
	private String hpcServerLoginUserName;
	private String hpcServerLoginUserPassword;
        private String hpcServerPrivateKey;
	private String agentClass;
	private String agentWorkspacePrefix;
	private String agentCompletedJobsSpacePrefix;
	private String agentFailedJobsSpacePrefix;
	private String hpcAddress;
	private String inputFileName;
	private String inputFileExtension;
	private String jsonInputFileName;
	private String jsonFileExtension;
	private String slurmScriptFileName;
	private String executableFile;
	private String outputFileName;
	private String outputFileExtension;
	private int maxNumberOfHPCJobs;
	private int agentInitialDelayToStartJobMonitoring;
	private int agentPeriodicActionInterval;
	
	public String getHpcServerLoginUserName() {
		return hpcServerLoginUserName;
	}

	public void setHpcServerLoginUserName(String hpcServerLoginUserName) {
		this.hpcServerLoginUserName = hpcServerLoginUserName;
	}

	public String getHpcServerLoginUserPassword() {
		return hpcServerLoginUserPassword;
	}

	public void setHpcServerLoginUserPassword(String hpcServerLoginUserPassword) {
		this.hpcServerLoginUserPassword = hpcServerLoginUserPassword;
	}
        
        public String getHpcServerPrivateKey() {
            return this.hpcServerPrivateKey;
        }
        public void setHpcServerPrivateKey(String hpcServerPrivateKey) {
            this.hpcServerPrivateKey = hpcServerPrivateKey;
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

	public String getHpcAddress() {
		return hpcAddress;
	}

	public String getInputFileName() {
		return inputFileName;
	}

	public String getInputFileExtension() {
		return inputFileExtension;
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

	public String getOutputFileName() {
		return outputFileName;
	}

	public String getOutputFileExtension() {
		return outputFileExtension;
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

	public void setInputFileName(String inputFileName) {
		this.inputFileName = inputFileName;
	}

	public void setInputFileExtension(String inputFileExtension) {
		this.inputFileExtension = inputFileExtension;
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

	public void setOutputFileName(String outputFileName) {
		this.outputFileName = outputFileName;
	}

	public void setOutputFileExtension(String outputFileExtension) {
		this.outputFileExtension = outputFileExtension;
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

	public String getAgentFailedJobsSpacePrefix() {
		return agentFailedJobsSpacePrefix;
	}

	public void setAgentFailedJobsSpacePrefix(String agentFailedJobsSpacePrefix) {
		this.agentFailedJobsSpacePrefix = agentFailedJobsSpacePrefix;
	}
}
