package uk.ac.cam.cares.jps.base.slurm.job;

import junit.framework.TestCase;

public class StatusTest extends TestCase{
	
	
	public void testStatus() {
		
		
		assertEquals(Status.STATUS_FILE.getName(),"status.txt");
		assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName(),"JobStatus:");
		assertEquals(Status.STATUS_JOB_NOT_STARTED.getName(),"not started");
		assertEquals(Status.STATUS_JOB_COMPLETED.getName(),"completed");
		assertEquals(Status.STATUS_JOB_COMPLETING.getName(),"completing");
		assertEquals(Status.STATUS_JOB_FAILED.getName(),"failed");
		assertEquals(Status.STATUS_JOB_PENDING.getName(),"pending");
		assertEquals(Status.STATUS_JOB_PREEMPTED.getName(),"preempted");
		assertEquals(Status.STATUS_JOB_RUNNING.getName(),"running");
		assertEquals(Status.STATUS_JOB_SUSPENDED.getName(),"suspended");
		assertEquals(Status.STATUS_JOB_STOPPED.getName(),"stopped");
		assertEquals(Status.STATUS_JOB_ERROR_TERMINATED.getName(),"error termination");
		assertEquals(Status.ATTRIBUTE_JOB_ID.getName(),"JobId:");
		assertEquals(Status.ATTRIBUTE_AGENT_ID.getName(),"AgentId:");
		assertEquals(Status.ATTRIBUTE_HPC_ADDRESS.getName(),"HPCAddress:");
		assertEquals(Status.ATTRIBUTE_JOB_OUTPUT.getName(),"JobOutput:");
		assertEquals(Status.OUTPUT_PROCESSED.getName(),"processed");
		assertEquals(Status.EXTENSION_SLURM_FILE.getName(),".sh");
		assertEquals(Status.EXTENSION_LOG_FILE.getName(),".log");
		assertEquals(Status.EXTENSION_JSON_FILE.getName(),".json");
		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(),"The job request has been processed successfully.");
		assertEquals(Status.JOB_SETUP_JSON_ERROR.getName(),"The JSON request is ill formatted.");
		assertEquals(Status.JOB_SETUP_SLURM_SCRIPT_ERROR.getName(),"The Slurm script could not be downloaded.");
		assertEquals(Status.JOB_SETUP_INPUT_FILE_ERROR.getName(),"The input file for the job could not be created.");
		assertEquals(Status.JOB_SETUP_ERROR.getName(),"The requested job could not be set up, therefore, resubmit the job.");
		assertEquals(Status.JOB_LOG_MSG_ERROR_TERMINATION.getName(),"error termination");
		assertEquals(Status.JOB_LOG_MSG_NORMAL_TERMINATION.getName(),"normal termination");
		assertEquals(Status.JOB_OUTPUT_FILE_EXIST_MESSAGE.getName(),"The file exist.");
		assertEquals(Status.JOB_SQUEUE_STATUS_COMPLETED.getName(),"CD");
		assertEquals(Status.JOB_SQUEUE_STATUS_COMPLETING.getName(),"CG");
		assertEquals(Status.JOB_SQUEUE_STATUS_FAILED.getName(),"F");
		assertEquals(Status.JOB_SQUEUE_STATUS_PENDING.getName(),"PD");
		assertEquals(Status.JOB_SQUEUE_STATUS_PREEMPTED.getName(),"PR");
		assertEquals(Status.JOB_SQUEUE_STATUS_RUNNING.getName(),"R");
		assertEquals(Status.JOB_SQUEUE_STATUS_SUSPENDED.getName(),"S");
		assertEquals(Status.JOB_SQUEUE_STATUS_STOPPED.getName(),"ST");
	}

}
