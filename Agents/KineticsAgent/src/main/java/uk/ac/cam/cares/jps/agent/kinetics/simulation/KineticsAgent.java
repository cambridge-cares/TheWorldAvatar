package uk.ac.cam.cares.jps.agent.kinetics.simulation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.commons.io.FileUtils;

import org.slf4j.LoggerFactory;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import uk.ac.cam.cares.jps.agent.configuration.KineticsAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.KineticsAgentProperty;
import uk.ac.cam.cares.jps.agent.utils.ZipUtility;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.PostProcessing;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * Kinetics Agent has implemented the processes to create input files for<p>
 * setting up Slurm jobs to perform kinetics simulations on any High-Per-<p>
 * formance Computing (HPC) clusters. This agent asynchronously monitors<p>
 * jobs, post-processes outputs of simulations and receives HTTP requests<p>
 * to respond with outputs of a submitted and successfully finished Slurm job.<p>
 *
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@Controller
@WebServlet(urlPatterns = {KineticsAgent.JOB_REQUEST_PATH, KineticsAgent.JOB_STATISTICS_PATH,
	KineticsAgent.JOB_OUTPUT_REQUEST_PATH})
public class KineticsAgent extends JPSAgent {

	private static final long serialVersionUID = -8669607645910441935L;
	private Logger logger = LoggerFactory.getLogger(KineticsAgent.class);
	private File workspace;
	static JobSubmission jobSubmission;
	public static ApplicationContext applicationContextKineticsAgent;
	public static KineticsAgentProperty kineticsAgentProperty;

	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to Kinetics Agent";

	public static final String JOB_REQUEST_PATH = "/job/request";
	public static final String JOB_OUTPUT_REQUEST_PATH = "/job/output/request";
	public static final String JOB_STATISTICS_PATH = "/job/statistics";
	public static final String JOB_SHOW_STATISTICS_PATH = "/job/show/statistics";
	

	/**
	 * Produces and returns the following statistics of Slurm jobs submitted<p>
	 * to Kinetics Agent in JSON format:<p>
	 * - Total number of jobs submitted<p> 
	 * - Total number of jobs currently running<p>
	 * - Total number of jobs successfully completed<p>
	 * - Total number of jobs terminated with an error<p>
	 * - Total number of jobs not started yet<p>
	 *
	 * @param input the JSON string specifying the return data format.
	 * @return the statistics in JSON format if requested.
	 */
	public JSONObject produceStatistics(String input) throws IOException, KineticsAgentException {
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		// Initialises all properties required for this agent to set-up<br>
		// and run jobs. It will also initialise the unique instance of<br>
		// Job Submission class.
		initAgentProperty();
		return jobSubmission.getStatistics(input);
	}

	/**
	 * Shows the following statistics of Slurm jobs submitted to Kinetics Agent.<br>
	 * This method responds against the HTTP request for showing statistics<p>
	 * which is not included in the list of URL patterns represented in the<p>
	 * annotation of this class.
	 * - Total number of jobs submitted<p>
	 * - Total number of jobs currently running<p>
	 * - Total number of jobs successfully completed<p>
	 * - Total number of jobs terminated with an error<p>
	 * - Total number of jobs not started yet<p>
	 *
	 * @return the statistics in HTML format.
	 */
	@RequestMapping(value = KineticsAgent.JOB_SHOW_STATISTICS_PATH, method = RequestMethod.GET)
	@ResponseBody
	public String showStatistics() throws IOException, KineticsAgentException {
		System.out.println("Received a request to show statistics.\n");
		logger.info("Received a request to show statistics.\n");
		initAgentProperty();
		return jobSubmission.getStatistics();
	}

	/**
	 * Starts the asynchronous scheduler to monitor Slurm jobs.
	 *
	 * @throws KineticsAgentException
	 */
	public void init() throws ServletException {
		logger.info("---------- Kinetics Simulation Agent has started ----------");
		System.out.println("---------- Kinetics Simulation Agent has started ----------");
		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		KineticsAgent kineticsAgent = new KineticsAgent();
		// initialising classes to read properties from the kinetics-agent.properites file
		initAgentProperty();
		// In the following method call, the parameter getAgentInitialDelay-<br>
		// ToStartJobMonitoring refers to the delay (in seconds) before<br>
		// the job scheduler starts and getAgentPeriodicActionInterval<br>
		// refers to the interval between two consecutive executions of<br>
		// the scheduler.
		executorService.scheduleAtFixedRate(() -> {
			try {
				kineticsAgent.monitorJobs();
			} catch (SlurmJobException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}, kineticsAgentProperty.getAgentInitialDelayToStartJobMonitoring(),
			kineticsAgentProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
		logger.info("---------- Kinetics Simulation jobs are being monitored  ----------");
		System.out.println("---------- Kinetics Simulation jobs are being monitored  ----------");

	}

	/**
	 * Initialises the unique instance of the KineticsAgentProperty class that<br>
	 * reads all properties of KineticsAgent from the kinetics-agent property file.<br>
	 *
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the kinetics-agent property file<br>
	 * through the KineticsAgent class.
	 */
	public void initAgentProperty() {
		// initialising classes to read properties from the kinetics-agent.properites
		// file
		if (applicationContextKineticsAgent == null) {
			applicationContextKineticsAgent = new AnnotationConfigApplicationContext(KineticsAgentConfiguration.class);
		}
		if (kineticsAgentProperty == null) {
			kineticsAgentProperty = applicationContextKineticsAgent.getBean(KineticsAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(kineticsAgentProperty.getAgentClass(), kineticsAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(kineticsAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty
				.setHpcServerLoginUserPassword(kineticsAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(kineticsAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty
				.setAgentCompletedJobsSpacePrefix(kineticsAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty
				.setAgentFailedJobsSpacePrefix(kineticsAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(kineticsAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(kineticsAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(kineticsAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(kineticsAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(kineticsAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(kineticsAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(kineticsAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setJsonFileExtension(kineticsAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(kineticsAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(kineticsAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(
				kineticsAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty
				.setAgentPeriodicActionInterval(kineticsAgentProperty.getAgentPeriodicActionInterval());
		}
	}

	/**
	 * Receives and processes HTTP requests that match with the URL patterns<br>
	 * listed in the annotations of this class.
	 *
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		System.out.println("A request has been received..............................");
		if (path.equals(KineticsAgent.JOB_REQUEST_PATH)) {
			try {
				return setUpJob(requestParams.toString());
			} catch (SlurmJobException | IOException | KineticsAgentException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else if (path.equals(KineticsAgent.JOB_OUTPUT_REQUEST_PATH)) {
			JSONObject result = getSimulationResults(requestParams);
			return result;
		} else if (path.equals(KineticsAgent.JOB_STATISTICS_PATH)) {
			try {
				return produceStatistics(requestParams.toString());
			} catch (IOException | KineticsAgentException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else {
			System.out.println("Unknown request");
			throw new JPSRuntimeException(UNKNOWN_REQUEST);
		}
	}

	/**
	 * Validates input parameters specific to Kinetics Agent to decide whether<br>
	 * the job set up request can be served.
	 */
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
			throw new BadRequestException();
		}
		return true;
	}

	/**
	 * Checks the status of a job and returns results if it is finished and<br>
	 * post-processing is successfully completed. If the job has terminated<br>
	 * with an error or failed, then error termination message is sent.
	 *
	 * The JSON input for this request has the following format: {"jobId":
	 * "login-skylake.hpc.cam.ac.uk_117804308649998"}
	 *
	 * @param requestParams
	 * @return
	 */
	private JSONObject getSimulationResults(JSONObject requestParams) {
		JSONObject json = new JSONObject();
		String jobId = getJobId(requestParams);
		if (jobId == null) {
			return json.put("message", "jobId is not present in the request parameters.");
		}
		initAgentProperty();
		JSONObject message = checkJobInWorkspace(jobId);
		if (message != null) {
			return message;
		}
		JSONObject result = checkJobInCompletedJobs(jobId);
		if (result != null) {
			return result;
		}
		message = checkJobInFailedJobs(jobId);
		if (message != null) {
			return message;
		}
		return json.put("message", "The job is not available in the system.");
	}

	/**
	 * Checks the presence of the requested job in the workspace.<br>
	 * If the job is available, it returns that the job is currently running.
	 *
	 * @param json
	 * @return
	 */
	private JSONObject checkJobInWorkspace(String jobId) {
		JSONObject json = new JSONObject();
		// The path to the set-up and running jobs folder.
		workspace = jobSubmission.getWorkspaceDirectory();
		if (workspace.isDirectory()) {
			File[] jobFolders = workspace.listFiles();
			for (File jobFolder : jobFolders) {
				if (jobFolder.getName().equals(jobId)) {
					return json.put("message", "The job is being executed.");
				}
			}
		}
		return null;
	}

	/**
	 * Checks the presence of the requested job in the completed jobs.<br>
	 * If the job is available, it returns the result.
	 *
	 * @param json
	 * @return
	 */
	private JSONObject checkJobInCompletedJobs(String jobId) {
		JSONObject json = new JSONObject();
		// The path to the completed jobs folder.
		String completedJobsPath = workspace.getParent().concat(File.separator)
			.concat(kineticsAgentProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace.getName());
		File completedJobsFolder = new File(completedJobsPath);
		if (completedJobsFolder.isDirectory()) {
			File[] jobFolders = completedJobsFolder.listFiles();
			for (File jobFolder : jobFolders) {
				if (jobFolder.getName().equals(jobId)) {
					try {
						String inputJsonPath = completedJobsPath.concat(File.separator).concat(jobFolder.getName()).concat(File.separator).concat(kineticsAgentProperty.getReferenceOutputJsonFile());
						InputStream inputStream = new FileInputStream(inputJsonPath);
						return new JSONObject(FileUtil.inputStreamToString(inputStream));
					} catch (FileNotFoundException e) {
						return json.put("message", "The job has been completed, but the file that contains results is not found.");
					}
				}
			}
		}
		return null;
	}

	/**
	 * Checks the presence of the requested job in the failed jobs.<br>
	 * If the job is available, it returns a message saying that<br>
	 * job has failed.
	 *
	 * @param json
	 * @param jobId
	 * @return
	 */
	private JSONObject checkJobInFailedJobs(String jobId) {
		JSONObject json = new JSONObject();
		// The path to the failed jobs folder.
		String failedJobsPath = workspace.getParent().concat(File.separator)
			.concat(kineticsAgentProperty.getAgentFailedJobsSpacePrefix()).concat(workspace.getName());
		File failedJobsFolder = new File(failedJobsPath);
		if (failedJobsFolder.isDirectory()) {
			File[] jobFolders = failedJobsFolder.listFiles();
			for (File jobFolder : jobFolders) {
				if (jobFolder.getName().equals(jobId)) {
					return json.put("message",
						"The job terminated with an error. Please check the failed jobs folder.");
				}
			}
		}
		return null;
	}

	/**
	 * Monitors already set up jobs.
	 *
	 * @throws SlurmJobException
	 */
	private void monitorJobs() throws SlurmJobException {
		//Configures all properties required for setting-up and running a Slurm job. 
		jobSubmission.monitorJobs();
		processOutputs();
	}

	/**
	 * Monitors the currently running Slurm jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.
	 *
	 */
	public void processOutputs() {
		workspace = jobSubmission.getWorkspaceDirectory();
		try {
			if (workspace.isDirectory()) {
				File[] jobFolders = workspace.listFiles();
				for (File jobFolder : jobFolders) {

					if (Utils.isJobCompleted(jobFolder) && !Utils.isJobOutputProcessed(jobFolder)) {

                        boolean outcome = false;
                        try {
                            outcome = postProcessing(Paths.get(jobFolder.getAbsolutePath()));
                        } catch(Exception exception) {
                            exception.printStackTrace(System.out);
                            outcome = false;
                        }

						if (outcome) {
							// Success
							PostProcessing.updateJobOutputStatus(jobFolder);
						} else {
							// Failure
							Utils.modifyStatus(
								Utils.getStatusFile(jobFolder).getAbsolutePath(),
								Status.JOB_LOG_MSG_ERROR_TERMINATION.getName()
							);
						}
					}
				}
			}
		} catch (IOException e) {
			logger.error("KineticsAgent: IOException.".concat(e.getMessage()));
			e.printStackTrace();
		}
	}

	/**
	 * Executes post-processing on the input job folder, returning true if the post-processing task returns
	 * successfully.
	 *
	 * @param jobFolder job folder
	 *
	 * @return true if post-processing is successful
	 */
	public boolean postProcessing(Path jobFolder) throws Exception {
        System.out.println("Running postProcessing() method...");
        
		// Find the job results ZIP
		Path archive = Paths.get(
			jobFolder.toString(),
			kineticsAgentProperty.getOutputFileName() + kineticsAgentProperty.getOutputFileExtension()
		);
		if (!Files.exists(archive)) throw new IOException("Cannot find expected archive at: " + archive);

		// Unzip
        System.out.println("Unzipping files...");

		Path outputsDir = Paths.get(jobFolder.toString(), "outputs");
		
		ZipUtility zipper = new ZipUtility();
		zipper.unzip(
			archive.toString(),
			outputsDir.toString()
		);

        // Artificial delay to wait for the unzipping (not an ideal fudge,
		// hence, needs to be modified in the future)
        Thread.sleep(1000);
        System.out.println("Unzipped files!");
        
		// Get the location of the python scripts directory
		Path scriptsDir = Paths.get(kineticsAgentProperty.getAgentScriptsLocation());
		if (!Files.exists(scriptsDir)) throw new IOException("Cannot find python scripts directory at: " + scriptsDir);

		// Build commands for running the Python script created for post-
		// processing the outputs of the kinetics simulation. 
		ProcessBuilder builder = new ProcessBuilder();
		builder.redirectErrorStream(true);
		
		Path execPath = Paths.get(scriptsDir.toString(), "venv", "bin", "agkin_post");
		builder.directory(Paths.get(scriptsDir.toString(), "venv", "bin").toFile());
		
		if (!Files.exists(execPath)) {
			builder.directory(Paths.get(scriptsDir.toString(), "venv", "Scripts").toFile());
			execPath = Paths.get(scriptsDir.toString(), "venv", "Scripts", "agkin_post.exe");
		}
		
		List<String> commands = new ArrayList<>();
		commands.add(execPath.toString());

		// Location of outputs directory directory
		commands.add("-d");
		commands.add(outputsDir.toString());
		
		builder.command(commands);
        System.out.println("Built command line arguments...");
        
		// Could redirect the script's output here, looks like a logging system is required first
		Process process = builder.start();

         try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line = null;
            
            while((line = reader.readLine()) != null) {
                System.out.println(line);
                if(line.toLowerCase().contains("failed") || line.toLowerCase().contains("error")) {
                    throw new KineticsAgentException("Error encountered when running pre-processing script!");
                }
            }
        } catch(Exception exception) {
            throw new KineticsAgentException("Error encountered when running pre-processing script!");
        }
        
		// Wait until the process is finished (should add a timeout here, expected duration?)
		while (process.isAlive()) {
			try {
				Thread.sleep(500);
			} catch (InterruptedException iException) {
				// Failure
				return false;
			}
		}
        
        System.out.println("Python script completed!");

		// Check the outputs JSON file
		String outputFilename = kineticsAgentProperty.getReferenceOutputJsonFile().trim();
		Path outputsJSON = Paths.get(outputsDir.toString(), outputFilename);

		if (!Files.exists(outputsJSON) || Files.readAllBytes(outputsJSON).length <= 0) {
			// Try looking in the job directory directly
			outputsJSON = Paths.get(jobFolder.toString(), outputFilename);

			if (!Files.exists(outputsJSON) || Files.readAllBytes(outputsJSON).length <= 0) {
				// No valid output.json, failure
				return false;
			}
		}
		
		// Copy the JSON up into the job folder just in case Feroz expects it there
        Path jsonCopy = Paths.get(jobFolder.toString(), outputFilename);
        
        if(!Files.exists(jsonCopy)) {
            Files.copy(outputsJSON, jsonCopy);
        }
		
		// Remove the temporary directory
        Path temporaryDirectory = Paths.get(System.getProperty("user.home"), "." + jobFolder.getFileName().toString());
        
        try {
            if(temporaryDirectory != null && Files.exists(temporaryDirectory)) {
                Thread.sleep(1000);
                FileUtils.deleteDirectory(temporaryDirectory.toFile());
                System.out.println("Deleted directory at: " + temporaryDirectory);
            }
        } catch(IOException ioException) {
            System.out.println("WARNING: Could not delete temporary directory at " + temporaryDirectory);
        }
		
		// Success!
		return true;
	}

	/**
	 * Sets up a Slurm job by creating the job folder and the following files</br>
	 * under this folder:</br>
	 * - the input file.</br>
	 * - the Slurm script file.</br. - the Status file.</br> - the JSON input file, which comes from the user
	 * request.</br>
	 *
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws KineticsAgentException
	 */
	public JSONObject setUpJob(String jsonString) throws IOException, KineticsAgentException, SlurmJobException {
		String message = setUpJobOnAgentMachine(jsonString);
		JSONObject obj = new JSONObject();
		obj.put("jobId", message);
		return obj;
	}

	/**
	 * Sets up the Slurm job for the current input.
	 *
	 * @param jsonInput
	 * @return
	 * @throws IOException
	 * @throws KineticsAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonInput) throws IOException, KineticsAgentException, SlurmJobException {
		initAgentProperty();
		long timeStamp = Utils.getTimeStamp();
		String jobFolderName = getNewJobFolderName(kineticsAgentProperty.getHpcAddress(), timeStamp);
		
		return jobSubmission.setUpJob(
			jsonInput, new File(getClass().getClassLoader()
				.getResource(kineticsAgentProperty.getSlurmScriptFileName()).getPath()),
			getInputFile(jsonInput, jobFolderName), timeStamp);
	}

	/**
	 * Prepares input files, bundles them in a zip file and returns the zip file to the calling method.
	 *
	 * @param jsonInput
	 * @param jobFolderName
	 * @return
	 * @throws IOException
	 * @throws KineticsAgentException
	 */
	private File getInputFile(String jsonInput, String jobFolderName) throws IOException, KineticsAgentException {
		// Get the location of the python scripts directory
		Path scriptsDir = Paths.get(kineticsAgentProperty.getAgentScriptsLocation());
		if (!Files.exists(scriptsDir)) throw new IOException("Cannot find python scripts directory at: " + scriptsDir);

		// Contains directories for each provided SRM simulation template
		Path templatesDir = Paths.get(scriptsDir.toString(), "simulation_templates");
		if (!Files.exists(templatesDir)) throw new IOException("Cannot find SRM templates directory at: " + templatesDir);

		// Create a temporary folder in the user's home location
		Path temporaryDirectory = Paths.get(System.getProperty("user.home"), "." + jobFolderName);
		try {
			Files.createDirectory(temporaryDirectory);

			// Save JSON raw input to file
			Path dstJSON = Paths.get(temporaryDirectory.toString(), "input.json");

			FileWriter fileWriter = new FileWriter(dstJSON.toFile());
			fileWriter.write(jsonInput);

			fileWriter.flush();
			fileWriter.close();

		} catch (IOException ioException) {
			throw new IOException("Could not create temporary directory with JSON file at: " + temporaryDirectory);
		}
		
		// Build commands for running the Python script developed for
		// producing input files for the current Slurm job
		ProcessBuilder builder = new ProcessBuilder();
		builder.redirectErrorStream(true);

		Path execPath = Paths.get(scriptsDir.toString(), "venv", "bin", "agkin_pre");
		builder.directory(Paths.get(scriptsDir.toString(), "venv", "bin").toFile());
		
		if (!Files.exists(execPath)) {
			execPath = Paths.get(scriptsDir.toString(), "venv", "Scripts", "agkin_pre.exe");
			builder.directory(Paths.get(scriptsDir.toString(), "venv", "Scripts").toFile());
		}
		
		List<String> commands = new ArrayList<>();
		commands.add(execPath.toString());

		// Location of SRM simulation templates folder
		commands.add("-s");
		commands.add("../../simulation_templates");

		// Location of temporary output folder
		commands.add("-d");
		commands.add(temporaryDirectory.toString());

		builder.command(commands);

		// Could redirect the script's output here, looks like a logging system is required first
		Process process = builder.start();

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line = null;
            
            while((line = reader.readLine()) != null) {
                System.out.println(line);
                if(line.toLowerCase().contains("failed") || line.toLowerCase().contains("error")) {
                    throw new KineticsAgentException("Error encountered when running pre-processing script!");
                }
            }
        } catch(Exception exception) {
            throw new KineticsAgentException("Error encountered when running pre-processing script!");
        }
            
		// Wait until the process is finished (should add a timeout here, expected duration?)
		while (process.isAlive()) {
			try {
				Thread.sleep(500);
			} catch (InterruptedException iException) {
				throw new KineticsAgentException("Python script process was interrupted!");
			}
		}

		// Compress all files in the temporary directory into a ZIP
		Path zipFile = Paths.get(System.getProperty("user.home"), temporaryDirectory.getFileName().toString() + ".zip");
		List<File> zipContents = new ArrayList<>();

		Files.walk(temporaryDirectory)
			.map(Path::toFile)
			.forEach((File f) -> zipContents.add(f));
		zipContents.remove(temporaryDirectory.toFile());

		// Will throw an IOException if something goes wrong
		new ZipUtility().zip(zipContents, zipFile.toString());

		// Return the final ZIP file
		return new File(zipFile.toString());
	}

	/**
	 * Produces a job folder name by following the schema hpcAddress_timestamp.
	 *
	 * @param hpcAddress
	 * @param timeStamp
	 * @return
	 */
	public String getNewJobFolderName(String hpcAddress, long timeStamp) {
		return hpcAddress.concat("_").concat("" + timeStamp);
	}

	/**
	 * Returns the job id.
	 *
	 * @param jsonObject
	 * @return
	 */
	public String getJobId(JSONObject jsonObject) {
		if (jsonObject.has("jobId")) {
			return jsonObject.get("jobId").toString();
		} else {
			return null;
		}
	}
}
