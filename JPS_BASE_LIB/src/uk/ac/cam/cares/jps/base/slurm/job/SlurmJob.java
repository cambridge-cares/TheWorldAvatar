package uk.ac.cam.cares.jps.base.slurm.job;

public class SlurmJob {
	private String jobFolderName;
	
	public String getJobFolderName() {
		return jobFolderName;
	}

	public void setJobFolderName(String jobFolderName) {
		this.jobFolderName = jobFolderName;
	}
	/**
	 * To create a new job folder name, this method is used. 
	 * 
	 * @param hpcAddress
	 * @return
	 */
	public String getNewJobFolderName(String hpcAddress){
		setJobFolderName(hpcAddress.concat("_").concat("" + Utils.getTimeStamp()));
		return getJobFolderName();
	}
}
