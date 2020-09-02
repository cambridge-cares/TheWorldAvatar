package uk.ac.cam.cares.jps.base.slurm.job;

/**
 * Starting from the submission to the completion a Slurm job goes through<br>
 * different statuses, e.g. pending, running and completed. This java class<br>
 * contains all possible statuses and corresponding messages as constants.   
 * 
 * @author msff2
 *
 */
public enum Status {
	STATUS_FILE("status.txt"),
	ATTRIBUTE_JOB_STATUS("JobStatus:"),
	STATUS_JOB_NOT_STARTED("not started"),
	STATUS_JOB_COMPLETED("completed"),
	STATUS_JOB_COMPLETING("completing"),
	STATUS_JOB_FAILED("failed"),
	STATUS_JOB_PENDING("pending"),
	STATUS_JOB_PREEMPTED("preempted"),
	STATUS_JOB_RUNNING("running"),
	STATUS_JOB_SUSPENDED("suspended"),
	STATUS_JOB_STOPPED("stopped"),
	STATUS_JOB_ERROR_TERMINATED("error termination"),
	ATTRIBUTE_JOB_ID("JobId:"),
	ATTRIBUTE_AGENT_ID("AgentId:"),
	ATTRIBUTE_HPC_ADDRESS("HPCAddress:"),
	ATTRIBUTE_JOB_OUTPUT("JobOutput:"),
	OUTPUT_PROCESSED("processed"),
	EXTENSION_SLURM_FILE(".sh"),
	EXTENSION_LOG_FILE(".log"),
	EXTENSION_JSON_FILE(".json"),
	JOB_SETUP_SUCCESS_MSG("The job request has been processed successfully."),
	JOB_SETUP_JSON_ERROR("The JSON request is ill formatted."),
	JOB_SETUP_SLURM_SCRIPT_ERROR("The Slurm script could not be downloaded."),
	JOB_SETUP_INPUT_FILE_ERROR("The input file for the job could not be created."),
	JOB_SETUP_ERROR("The requested job could not be set up, therefore, resubmit the job."),
	JOB_LOG_MSG_ERROR_TERMINATION("error termination"),
	JOB_LOG_MSG_NORMAL_TERMINATION("normal termination"),
	JOB_SQUEUE_STATUS_COMPLETED("CD"),
	JOB_SQUEUE_STATUS_COMPLETING("CG"),
	JOB_SQUEUE_STATUS_FAILED("F"),
	JOB_SQUEUE_STATUS_PENDING("PD"),
	JOB_SQUEUE_STATUS_PREEMPTED("PR"),
	JOB_SQUEUE_STATUS_RUNNING("R"),
	JOB_SQUEUE_STATUS_SUSPENDED("S"),
	JOB_SQUEUE_STATUS_STOPPED("ST");
	
	private String property;
	private Status(String property){
		this.property = property;
	}
	
	public String getName(){
		return property;
	}
}
