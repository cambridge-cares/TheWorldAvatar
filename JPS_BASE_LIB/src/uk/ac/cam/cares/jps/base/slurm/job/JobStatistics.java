package uk.ac.cam.cares.jps.base.slurm.job;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

/**
 * This class implemented methods for the calculation and visualisation of<br>
 * statistics of jobs submitted to an HPC.  
 * 
 * @author msff2
 *
 */
public class JobStatistics {
	private int jobsSubmitted = 0;
	private int jobsCompleted = 0;
	private int jobsCompleting = 0;
	private int jobsFailed = 0;
	private int jobsPending = 0;
	private int jobsPreempted = 0;
	private int jobsRunning = 0;
	private int jobsSuspended = 0;
	private int jobsStopped = 0;
	private int jobsErrorTerminated = 0;
	private int jobsNotStarted = 0;
	
	private File jobSpace;
	
	/**
	 * Construct that takes the job workspace folder as the parameter and<br>
	 * calculates the statistics of jobs submitted to an HPC. 
	 * 
	 * @param jobSpace
	 * @throws IOException
	 */
	public JobStatistics(File jobSpace) throws IOException{
		this.jobSpace = jobSpace;
		File[] jobs = jobSpace.listFiles();
		for(File job:jobs){
			calculateAllStatistics(job);
		}
		setJobsSubmitted(getJobsCompleted() + getJobsCompleting() + getJobsFailed() + getJobsPending()
				+ getJobsPreempted() + getJobsRunning() + getJobsSuspended() + getJobsStopped()
				+ getJobsErrorTerminated() + getJobsNotStarted());
	}

	/**
	 * Calculates statistics of jobs grouped into multiple categories based<br>
	 * upon their status.
	 * 
	 * @param job
	 * @throws IOException
	 */
	private void calculateAllStatistics(File job) throws IOException{
		if(job!=null && job.isDirectory()){
			File[] jobFiles = job.listFiles();
			for(File jobFile:jobFiles){
				calculateStatistics(jobFile);
			}
		}
	}
	
	/**
	 * Reads the status of single job to calculate the statistics. 
	 * 
	 * @param jobFile
	 * @throws IOException
	 */
	private void calculateStatistics(File jobFile) throws IOException{
		if(!jobFile.isDirectory() && jobFile.getAbsolutePath().endsWith(Status.STATUS_FILE.getName())){
			calculateStatistics(jobFile.getAbsolutePath());
		}
	}
	/**
	 * Calculates statistics about all submitted jobs.
	 * 
	 * @param statusFilePath
	 * @throws IOException
	 */
	public void calculateStatistics(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_COMPLETED.getName())){
					setJobsCompleted(getJobsCompleted()+1);
				} else if(line.contains(Status.STATUS_JOB_COMPLETING.getName())){
					setJobsCompleting(getJobsCompleting()+1);
				} else if(line.contains(Status.STATUS_JOB_FAILED.getName())){
					setJobsFailed(getJobsFailed()+1);
				} else if(line.contains(Status.STATUS_JOB_PENDING.getName())){
					setJobsPending(getJobsPending()+1);
				} else if(line.contains(Status.STATUS_JOB_PREEMPTED.getName())){
					setJobsPreempted(getJobsPreempted()+1);
				} else if(line.contains(Status.STATUS_JOB_RUNNING.getName())){
					setJobsRunning(getJobsRunning()+1);
				} else if(line.contains(Status.STATUS_JOB_SUSPENDED.getName())){
					setJobsSuspended(getJobsSuspended()+1);
				} else if(line.contains(Status.STATUS_JOB_STOPPED.getName())){
					setJobsStopped(getJobsStopped()+1);
				} else if(line.contains(Status.STATUS_JOB_ERROR_TERMINATED.getName())){
					setJobsErrorTerminated(getJobsErrorTerminated()+1);
				} else if(line.contains(Status.STATUS_JOB_NOT_STARTED.getName())){
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
	
	public int getJobsCompleting() {
		return jobsCompleting;
	}

	public void setJobsCompleting(int jobsCompleting) {
		this.jobsCompleting = jobsCompleting;
	}

	public int getJobsFailed() {
		return jobsFailed;
	}

	public void setJobsFailed(int jobsFailed) {
		this.jobsFailed = jobsFailed;
	}

	public int getJobsPending() {
		return jobsPending;
	}

	public void setJobsPending(int jobsPending) {
		this.jobsPending = jobsPending;
	}

	public int getJobsPreempted() {
		return jobsPreempted;
	}

	public void setJobsPreempted(int jobsPreempted) {
		this.jobsPreempted = jobsPreempted;
	}

	public int getJobsSuspended() {
		return jobsSuspended;
	}

	public void setJobsSuspended(int jobsSuspended) {
		this.jobsSuspended = jobsSuspended;
	}

	public int getJobsStopped() {
		return jobsStopped;
	}

	public void setJobsStopped(int jobsStopped) {
		this.jobsStopped = jobsStopped;
	}

	/**
	 * Produces the header for showing the statistics table.
	 * 
	 * @return mainly the header of HTML.
	 */
	public String getHTMLHeader(){
		String htmlHead = "<!DOCTYPE html>";     
		htmlHead = htmlHead.concat("<html>");
		htmlHead = htmlHead.concat("<head>");
		htmlHead = htmlHead.concat("<style>");
		htmlHead = htmlHead.concat("table, th, td {");
		htmlHead = htmlHead.concat("border: 1px solid black;");
		htmlHead = htmlHead.concat("border-collapse: collapse;");
		htmlHead = htmlHead.concat("}");
		htmlHead = htmlHead.concat("th, td {");
		htmlHead = htmlHead.concat("padding: 15px;");
		htmlHead = htmlHead.concat("text-align: left;");
		htmlHead = htmlHead.concat("}");
		htmlHead = htmlHead.concat("table#t01 {");
		htmlHead = htmlHead.concat("width: 100%;");
		htmlHead = htmlHead.concat("background-color: #f1f1c1;");
		htmlHead = htmlHead.concat("}");
		htmlHead = htmlHead.concat("</style>");
		htmlHead = htmlHead.concat(getResources());
		htmlHead = htmlHead.concat("</head>");
		return htmlHead; 
	}
	
	/**
	 * Adds links to java script libraries and style sheets.
	 * 
	 * @return
	 */
	private String getResources(){
		String resourcePath = pathToResource();
		String headerResource = "<meta charset=\"UTF-8\">";
		headerResource = headerResource.concat("<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js\"></script>");
		headerResource = headerResource.concat(getLink("<link rel=\"stylesheet\" type=\"text/css\" href=\"", resourcePath, "css/static/group/bootstrap.min.css\">"));
		headerResource = headerResource.concat(getLink("<link rel=\"stylesheet\" type=\"text/css\" href=\"", resourcePath, "css/index.css\">"));
		headerResource = headerResource.concat(getLink("<link rel=\"icon\" href=\"", resourcePath, "css/static/group/favicon.ico\">"));
		headerResource = headerResource.concat(getLink("<link rel=\"stylesheet\" type=\"text/css\" href=\"", resourcePath, "css/static/group/dftagent.css\">"));
		headerResource = headerResource.concat("<title>J-Park Simulator</title>");
		return headerResource;
	}
	
	public String getBodydivStart(){
		String resourcePath = pathToResource();
		String startDiv = "<div class=\"jumbotron text-center\" id=\"topBanner\">";
		startDiv = startDiv.concat("<a id=\"cares-banner\" href=\"http://www.cares.cam.ac.uk\">");
		startDiv = startDiv.concat("<img id=\"care-banner-img\" src=\"");
		startDiv = startDiv.concat(resourcePath.concat("images/cam_lang_negativ1%20NEW_0.png\">"));
		startDiv = startDiv.concat("</a>");
		startDiv = startDiv.concat("<h1 id=\"title\">DFT Agent</h1>");
		startDiv = startDiv.concat("<p id=\"description\">");
		startDiv = startDiv.concat("An agent developed for performing quantum chemistry calculations at "
				+ "various levels of theories to create a living, continuously self-improving and growing "
				+ "Knowledge Graph of species serving the need for thermochemical data across disciplines.");
		startDiv = startDiv.concat("</p>");
		startDiv = startDiv.concat("</div>");
		return startDiv;
	}
	
	/**
	 * Converts path specific to a Windows or Linux server.
	 * 
	 * @param resourcePath
	 * @return
	 */
	private String pathToResource(){
		return "/DFTAgent/assets/";
	}
	
	/**
	 * Produces a link to a java script library or style sheet using the</br>
	 * link tag and the path where the resource is located.
	 *  
	 * @param link
	 * @param hrefStartPath
	 * @param hrefEndPath
	 * @return
	 */
	private String getLink(String link, String hrefStartPath, String hrefEndPath){
		String linkResource = link;
		linkResource = linkResource.concat(hrefStartPath);
		linkResource = linkResource.concat(hrefEndPath);
		return linkResource;
	}
	
	/**
	 * Prepares the header of table showing statistics about jobs submitted to DFT Agent.
	 * 
	 * @return mainly the header part of the HTML table showing job statistics.
	 */
	public String getStatisticsTableHeader(String headerText, String statisticsProperty, String statisticsValue, String tableWidth){
		String tableHeader = "<h2>"+headerText+"</h2><br>";
		tableHeader = tableHeader + "<table style=\"width:"+tableWidth+"\">";
		tableHeader = tableHeader + "<tr>";
		tableHeader = tableHeader + "<th>"+statisticsProperty+"</th>";
		tableHeader = tableHeader + "<th>"+statisticsValue+"</th>";
		tableHeader = tableHeader + "</tr>";
		return tableHeader;
	}
	
	/**
	 * Prepares the row of table showing statistics about jobs submitted to DFT Agent.
	 * 
	 * @param property
	 * @param value
	 * @return
	 */
	public String getStatisticsTableRow(String property, String value){
		String tableRow = "<tr>";
		tableRow = tableRow + "<td>"+property+"</td>";
		tableRow = tableRow + "<td>"+value+"</td>";
		tableRow = tableRow + "</tr>";
		tableRow = tableRow + "<tr>";
		return tableRow;
	}
}
