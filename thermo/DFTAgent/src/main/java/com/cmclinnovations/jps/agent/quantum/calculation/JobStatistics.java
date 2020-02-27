package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

public class JobStatistics {
	private int jobsSubmitted = 0;
	private int jobsRunning = 0;
	private int jobsCompleted = 0;
	private int jobsErrorTerminated = 0;
	private int jobsNotStarted = 0;
	
	private File jobSpace;
	
	public JobStatistics(File jobSpace) throws IOException{
		this.jobSpace = jobSpace;
		File[] jobs = jobSpace.listFiles();
		for(File job:jobs){
			calculateAllStatistics(job);
		}
		setJobsSubmitted(getJobsCompleted()+getJobsErrorTerminated()+getJobsNotStarted()+getJobsRunning());
	}

	private void calculateAllStatistics(File job) throws IOException{
		if(job!=null && job.isDirectory()){
			File[] jobFiles = job.listFiles();
			for(File jobFile:jobFiles){
				calculateStatistics(jobFile);
			}
		}
	}
	
	private void calculateStatistics(File jobFile) throws IOException{
		if(!jobFile.isDirectory() && jobFile.getAbsolutePath().endsWith(Jobs.STATUS_FILE.getName())){
			calculateStatistics(jobFile.getAbsolutePath());
		}
	}
	
	public void calculateStatistics(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Jobs.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Jobs.STATUS_JOB_RUNNING.getName())){
					setJobsRunning(getJobsRunning()+1);
				}
				if(line.contains(Jobs.STATUS_JOB_COMPLETED.getName())){
					setJobsCompleted(getJobsCompleted()+1);
				}
				if(line.contains(Jobs.STATUS_JOB_ERROR_TERMINATED.getName())){
					setJobsErrorTerminated(getJobsErrorTerminated()+1);
				}
				if(line.contains(Jobs.STATUS_JOB_NOT_STARTED.getName())){
					setJobsNotStarted(getJobsNotStarted()+1);
				}				
			}
		}
		statusFile.close();
	}
	
	public void setJobSpace(File jobSpace) {
		this.jobSpace = jobSpace;
	}

	public int getJobsSubmitted() {
		return jobsSubmitted;
	}
	public void setJobsSubmitted(int jobsSubmitted) {
		this.jobsSubmitted = jobsSubmitted;
	}
	public int getJobsRunning() {
		return jobsRunning;
	}
	public void setJobsRunning(int jobsRunning) {
		this.jobsRunning = jobsRunning;
	}
	public int getJobsCompleted() {
		return jobsCompleted;
	}
	public void setJobsCompleted(int jobsCompleted) {
		this.jobsCompleted = jobsCompleted;
	}
	public int getJobsErrorTerminated() {
		return jobsErrorTerminated;
	}
	public void setJobsErrorTerminated(int jobsErrorTerminated) {
		this.jobsErrorTerminated = jobsErrorTerminated;
	}
	public int getJobsNotStarted() {
		return jobsNotStarted;
	}
	public void setJobsNotStarted(int jobsNotStarted) {
		this.jobsNotStarted = jobsNotStarted;
	}
}
