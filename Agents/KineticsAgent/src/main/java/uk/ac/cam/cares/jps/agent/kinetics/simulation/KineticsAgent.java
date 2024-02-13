package uk.ac.cam.cares.jps.agent.kinetics.simulation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;

import uk.ac.cam.cares.jps.agent.configuration.KineticsAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.KineticsAgentProperty;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
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
@WebServlet(urlPatterns = {
    KineticsAgent.JOB_REQUEST_PATH,
	KineticsAgent.JOB_OUTPUT_REQUEST_PATH
})
public class KineticsAgent extends JPSAgent {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(KineticsAgent.class);

	private static final long serialVersionUID = -8669607645910441935L;
	private static Path WORKSPACE;
	public static ApplicationContext applicationContextKineticsAgent;
	public static KineticsAgentProperty kineticsAgentProperty;

	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to Kinetics Agent";

	public static final String JOB_REQUEST_PATH = "/job/request";
	public static final String JOB_OUTPUT_REQUEST_PATH = "/job/output/request";
	public static final String JOB_STATISTICS_PATH = "/job/statistics";
	public static final String JOB_SHOW_STATISTICS_PATH = "/job/show/statistics";

    public volatile boolean postProcessing;
    
    static {
        try {
            WORKSPACE = Paths.get(System.getProperty("user.home"), "KineticsAgent");
            KineticsAgent.logToFile("Using workspace: " + WORKSPACE);

            if(!Files.exists(WORKSPACE)) {
                Files.createDirectories(WORKSPACE);
            }

            Path running = Paths.get(WORKSPACE.toString(), "running");
            if(!Files.exists(running)) {
                Files.createDirectories(running);
                KineticsAgent.logToFile("Created 'running' directory");
            }

            Path completed = Paths.get(WORKSPACE.toString(), "completed");
            if(!Files.exists(completed)) {
                Files.createDirectories(completed);
                KineticsAgent.logToFile("Created 'completed' directory");
            }

            Path failed = Paths.get(WORKSPACE.toString(), "failed");
            if(!Files.exists(failed)) {
                Files.createDirectories(failed);
                KineticsAgent.logToFile("Created 'failed' directory");
            }

        } catch(IOException exception) {
            KineticsAgent.logToFile("Could not create directories!", exception);
        }
        KineticsAgent.logToFile("--- @@@ ---");
    }

	/**
	 * Starts the asynchronous scheduler to monitor Slurm jobs.
	 *
	 * @throws KineticsAgentException
	 */
	public void init() throws ServletException {
		KineticsAgent.logToFile("----- Kinetics Agent has started -----");

        // Create new instance
		KineticsAgent kineticsAgent = new KineticsAgent();

		// initialising classes to read properties from the kinetics-agent.properites file
		initAgentProperty();

		// In the following method call, the parameter getAgentInitialDelay-<br>
		// ToStartJobMonitoring refers to the delay (in seconds) before<br>
		// the job scheduler starts and getAgentPeriodicActionInterval<br>
		// refers to the interval between two consecutive executions of<br>
		// the scheduler.
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		executorService.scheduleAtFixedRate(() -> {
                try {
                    kineticsAgent.monitorJobs();
                } catch (Exception exception) {
                    KineticsAgent.logToFile("Error in monitoring jobs! " + exception.getMessage());
                }
            },
            kineticsAgentProperty.getAgentInitialDelayToStartJobMonitoring(),
            kineticsAgentProperty.getAgentPeriodicActionInterval(),
            TimeUnit.SECONDS
        );

		KineticsAgent.logToFile("----- Kinetics Agent jobs are being monitored  -----");
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
        KineticsAgent.logToFile("Initialising the agent's properties...");

		// initialising classes to read properties from the kinetics-agent.properites file
		if (applicationContextKineticsAgent == null) {
			applicationContextKineticsAgent = new AnnotationConfigApplicationContext(KineticsAgentConfiguration.class);
		}
		if (kineticsAgentProperty == null) {
			kineticsAgentProperty = applicationContextKineticsAgent.getBean(KineticsAgentProperty.class);
		}

        KineticsAgent.logToFile("...properties have been initialised.");
	}

	/**
	 * Receives and processes HTTP requests that match with the URL patterns<br>
	 * listed in the annotations of this class.
	 *
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
        System.out.println("HELLO, CAN YOU SEE THIS LINE!");
        KineticsAgent.logToFile("A request has been receieved...");

		if (path.equals(KineticsAgent.JOB_REQUEST_PATH)) {
            KineticsAgent.logToFile("...it's a '" + KineticsAgent.JOB_REQUEST_PATH + " request.");

			try {
				return setUpJob(requestParams.toString());
			} catch (IOException | KineticsAgentException e) {
                KineticsAgent.logToFile("Exception! " + e.getMessage());
				throw new JPSRuntimeException(e.getMessage());
			}

		} else if (path.equals(KineticsAgent.JOB_OUTPUT_REQUEST_PATH)) {
            KineticsAgent.logToFile("...it's a '" + KineticsAgent.JOB_OUTPUT_REQUEST_PATH + " request.");

			JSONObject result = getSimulationResults(requestParams);
			return result;

		} else {
            KineticsAgent.logToFile("...it's an unknown request path.");
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
            KineticsAgent.logToFile("Received a request with no parameters!");
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
	 * Checks the presence of the requested job in the WORKSPACE.<br>
	 * If the job is available, it returns that the job is currently running.
	 *
	 * @param json
	 * @return
	 */
	private JSONObject checkJobInWorkspace(String jobId) {
        Path runningDir = Paths.get(WORKSPACE.toString(), "running");
        Path runningJobDir = Paths.get(runningDir.toString(), jobId);

        if(Files.exists(runningJobDir)) {
            JSONObject json = new JSONObject();
            return json.put("message", "The job is being executed.");
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
        Path completedDir = Paths.get(WORKSPACE.toString(), "completed");
        Path completedJobDir = Paths.get(completedDir.toString(), jobId);

        if(Files.exists(completedDir)) {
            JSONObject json = new JSONObject();
            String inputJsonPath = completedJobDir.toString().concat(File.separator).concat(kineticsAgentProperty.getReferenceOutputJsonFile());

            try {
                InputStream inputStream = new FileInputStream(inputJsonPath);
                return new JSONObject(FileUtil.inputStreamToString(inputStream));

            } catch (FileNotFoundException e) {
                KineticsAgent.logToFile("Could not find expected file at: " + inputJsonPath);
                return json.put("message", "The job has been completed, but the file that contains results is not found.");
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
        Path failedDir = Paths.get(WORKSPACE.toString(), "failed");
        Path failedJobDir = Paths.get(failedDir.toString(), jobId);

        if(Files.exists(failedJobDir)) {
            JSONObject json = new JSONObject();
            KineticsAgent.logToFile("Job with id '" + jobId + "' returned an error");
		    return json.put("message",	"The job terminated with an error. Please check the failed jobs folder.");
        }
        return null;
	}

	/**
	 * Monitors already set up jobs.
	 *
	 * @throws SlurmJobException
	 */
	private void monitorJobs() throws IOException {
		// Configures all properties required for setting-up and running a Slurm job. 
		//jobSubmission.monitorJobs();
		processOutputs();
	}

    private boolean isFinished(String jobFolder) throws IOException {
        Path logFile = Paths.get(jobFolder, "OutputCase00001Cyc0001.progress");

        if(Files.exists(logFile)) {
            KineticsAgent.logToFile("log exists");
            String logContents = FileUtils.readFileToString(logFile.toFile(), StandardCharsets.UTF_8);
            boolean contains = logContents.contains("progress_total=1");

            if(!contains) {
                // If not modified in 15 minutes, assume finished but failed 
                BasicFileAttributes attr = Files.readAttributes(logFile, BasicFileAttributes.class);

                LocalDateTime modified = LocalDateTime.ofInstant(attr.lastModifiedTime().toInstant(), ZoneId.systemDefault());
                LocalDateTime now = LocalDateTime.now();
                Duration duration = Duration.between(modified, now);
                
                boolean expired = duration.getSeconds() >= (15 * 60);
                if(expired) KineticsAgent.logToFile("Marking as failed due to expiration!");
                return expired;
            }
            return true;
        }
        return false;
    }

    private boolean wasSuccess(String jobFolder) throws IOException {
        Path logFile = Paths.get(jobFolder, "OutputCase00001Cyc0001.progress");

        if(Files.exists(logFile)) {
            KineticsAgent.logToFile("progress exists");
            String logContents = FileUtils.readFileToString(logFile.toFile(), StandardCharsets.UTF_8);
            boolean contains = logContents.contains("progress_cycle=1");
            KineticsAgent.logToFile("contains is " + contains);
            return contains;
        }
        return false;
    }

	/**
	 * Monitors the currently running Slurm jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.
	 *
	 */
	public void processOutputs() throws IOException {
        List<Path> forCompleted = new ArrayList<>();
        List<Path> forFailed = new ArrayList<>();
        
        Path runningDir = Paths.get(WORKSPACE.toString(), "running");
        if(!Files.exists(runningDir)) return;

        File[] list = runningDir.toFile().listFiles();

        for(File file : list) {
            Path path = Paths.get(file.getAbsolutePath());

            try {
                // Has the simulation finished
                if(isFinished(path.toString())) {
                    KineticsAgent.logToFile("Simulation finished at " + path);

                    // Was the simulation successful
                    boolean success = wasSuccess(path.toString());
                    KineticsAgent.logToFile("Success was " + success);

                    if(success) {
                        // Run post processing
                        boolean result = postProcessing(path);

                        // Wait for the result
                        long start = System.currentTimeMillis();
                        while(this.postProcessing && (System.currentTimeMillis() - start) < 30_000) {
                            Thread.sleep(3000);
                        }
                        KineticsAgent.logToFile("No longer post-processing.");

                        if(result) {
                            // Move to completed
                            KineticsAgent.logToFile("Moving to completed folder");
                            forCompleted.add(path);
                        } else {
                            // Move to failed
                            KineticsAgent.logToFile("Moving to failed folder");
                            forFailed.add(path);
                        }
                    } else {
                        // Move to failed
                        KineticsAgent.logToFile("Moving to failed folder");
                        forFailed.add(path);
                    }
                }
            } catch(Exception exception) {
                KineticsAgent.logToFile("Exception encountered when processing job outputs in folder: " + path, exception);
            }
        }

        forCompleted.forEach(path -> {
            if(Files.exists(path)) {
                try {
                    Path newPath = Paths.get(WORKSPACE.toString(), "completed", path.getFileName().toString());
                    Files.move(path, newPath);
                    KineticsAgent.logToFile("Moved to completed directory: " +  path.getFileName().toString());
                } catch(IOException excep) {
                    KineticsAgent.logToFile("Could not move to completed folder!", excep);
                }
            }
        });

        forFailed.forEach(path -> {
            if(Files.exists(path)) {
                try {
                    Path newPath = Paths.get(WORKSPACE.toString(), "failed", path.getFileName().toString());
                    Files.move(path, newPath);
                    KineticsAgent.logToFile("Moved to failed directory: " +  path.getFileName().toString());
                } catch(IOException excep) {
                    KineticsAgent.logToFile("Could not move to failed folder!", excep);
                }
            }
        });
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
        this.postProcessing = true;
        KineticsAgent.logToFile("Running postProcessing() method on folder " + jobFolder);
        
        Path outputsDir = Paths.get(jobFolder.toString(), "outputs");
        Files.createDirectories(outputsDir);

        try (Stream<Path> walker = Files.walk(jobFolder, 1)) {
            walker.forEach(path -> {
                if(!Files.isDirectory(path)) {
                    try {
                        Path dest = Paths.get(outputsDir.toString(), path.getFileName().toString());
                        Files.copy(path, dest);
                        
                        // KineticsAgent.logToFile("Copied following files:");
                        // KineticsAgent.logToFile("   FROM:  " + path);
                        // KineticsAgent.logToFile("   TO:    " + dest);

                    } catch (IOException innerExcep) {
                        KineticsAgent.logToFile("Exception when moving output files into new folder!", innerExcep);
                        KineticsAgent.logToFile(innerExcep.getMessage());
                        this.postProcessing = false;
                    }
                }
            });
        } catch(IOException outerExcep) {
            KineticsAgent.logToFile("Exception when moving output files into new folder!", outerExcep);
            this.postProcessing = false;
        }

        Thread.sleep(5000);
        KineticsAgent.logToFile("Moved files to output dir");

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
        KineticsAgent.logToFile("Generated command line arguments are:");
        KineticsAgent.logToFile(String.join(" ", commands));
        
		// Could redirect the script's output here, looks like a logging system is required first
        KineticsAgent.logToFile("Running python script...");
		Process process = builder.start();

         try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line = null;
            
            while((line = reader.readLine()) != null) {
                if(line.toLowerCase().contains("failed") || line.toLowerCase().contains("error")) {
                    KineticsAgent.logToFile("OFFENDING LINE IS...");
                    KineticsAgent.logToFile(line);
                    this.postProcessing = false;
                    throw new KineticsAgentException("Error encountered when running post-processing script!");
                }
            }
        } catch(Exception exception) {
            this.postProcessing = false;
            throw new KineticsAgentException("Error encountered when running post-processing script!");
        }
        
		// Wait until the process is finished (should add a timeout here, expected duration?)
		while (process.isAlive()) {
			try {
				Thread.sleep(500);
			} catch (InterruptedException iException) {
				// Failure
                this.postProcessing = false;
				return false;
			}
		}
        KineticsAgent.logToFile("...python script completed!");

		// Check the outputs JSON file
		String outputFilename = kineticsAgentProperty.getReferenceOutputJsonFile().trim();
		Path outputsJSON = Paths.get(outputsDir.toString(), outputFilename);

		if (!Files.exists(outputsJSON) || Files.readAllBytes(outputsJSON).length <= 0) {
			// Try looking in the job directory directly
			outputsJSON = Paths.get(jobFolder.toString(), outputFilename);

			if (!Files.exists(outputsJSON) || Files.readAllBytes(outputsJSON).length <= 0) {
				// No valid output.json, failure
                this.postProcessing = false;
				return false;
			}
		}
		
		// Copy the JSON up into the job folder just in case Feroz expects it there
        String fileName = (outputFilename.endsWith(".json")) ? outputFilename : outputFilename + ".json";
        Path jsonCopy = Paths.get(jobFolder.toString(), fileName);
        if(!Files.exists(jsonCopy)) {
            Files.copy(outputsJSON, jsonCopy);
            KineticsAgent.logToFile("   FROM:  " + outputsJSON);
            KineticsAgent.logToFile("   TO:    " + jsonCopy);
        }
		
		// Remove the temporary directory
        Path temporaryDirectory = Paths.get(System.getProperty("user.home"), "." + jobFolder.getFileName().toString());
        
        try {
            if(temporaryDirectory != null && Files.exists(temporaryDirectory)) {
                Thread.sleep(1000);
                FileUtils.deleteDirectory(temporaryDirectory.toFile());
                KineticsAgent.logToFile("Deleted directory at: " + temporaryDirectory);
            }
        } catch(IOException ioException) {
            this.postProcessing = false;
            KineticsAgent.logToFile("Could not delete directory at: " + temporaryDirectory, ioException);
        }
		
		// Success!
        KineticsAgent.logToFile("Finished running postProcessing() method.");
        this.postProcessing = false;
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
	public JSONObject setUpJob(String jsonString) throws IOException, KineticsAgentException {
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
	private String setUpJobOnAgentMachine(String jsonInput) throws IOException, KineticsAgentException {
        return executeSRM(jsonInput);
	}

    private String executeSRM(String jsonInput) throws IOException, KineticsAgentException {
        // Generate new job folder name.
        String timeStamp = new SimpleDateFormat("yyyyMMddHHmmss").format(new java.util.Date());
        String jobFolderName = "simdome-" + timeStamp;

        // Prepare inputs
        File workingDir = getInputFile(jsonInput, jobFolderName);
        KineticsAgent.logToFile("Simulation directory is at " + workingDir);

        Path srmDir = Paths.get(System.getProperty("user.home"), "srm-driver"); 
        KineticsAgent.logToFile("SRM directory is at " + srmDir);

        String command = "./driver -w " + workingDir + "/";
        KineticsAgent.logToFile("Using command " + command);

        Process proc = Runtime.getRuntime().exec(command, null, srmDir.toFile());
        try {
            BufferedReader lineReader = new BufferedReader(new InputStreamReader(proc.getInputStream()));
            lineReader.lines().forEach(line -> KineticsAgent.logToFile(line));

            BufferedReader errorReader = new BufferedReader(new InputStreamReader(proc.getErrorStream()));
            errorReader.lines().forEach(line -> KineticsAgent.logToFile("ERROR: " + line));

        } catch(Exception excep) {
            KineticsAgent.logToFile("Exception when running SRM ", excep);
        }

        return jobFolderName;
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
	private File getInputFile(String jsonInput, String jobId) throws IOException, KineticsAgentException {
        KineticsAgent.logToFile("Preparing input files for job " + jobId);

		// Get the location of the python scripts directory
		Path scriptsDir = Paths.get(kineticsAgentProperty.getAgentScriptsLocation());
        KineticsAgent.logToFile("Scripts location is " + scriptsDir);
		if (!Files.exists(scriptsDir)) throw new IOException("Cannot find python scripts directory at: " + scriptsDir);

		// Contains directories for each provided SRM simulation template
		Path templatesDir = Paths.get(scriptsDir.toString(), "simulation_templates");
        KineticsAgent.logToFile("Templates location is " + templatesDir);
		if (!Files.exists(templatesDir)) throw new IOException("Cannot find SRM templates directory at: " + templatesDir);

		// Create a temporary folder in the user's home location
		Path temporaryDirectory = Paths.get(System.getProperty("user.home"), "KineticsAgent", "running", jobId);
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
		commands.add(templatesDir.toString());

		// Location of temporary output folder
		commands.add("-d");
		commands.add(temporaryDirectory.toString());
		builder.command(commands);

        KineticsAgent.logToFile("Commands: " + String.join(" ", commands));

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
            KineticsAgent.logToFile("Could not run process!", exception);
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

        KineticsAgent.logToFile("Inputs processing and ready for running at: " + temporaryDirectory.toString());
        return temporaryDirectory.toFile();
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

    private static void logToFile(String message, Throwable exception) {
        KineticsAgent.logToFile(message + "\n" + exception.getMessage());
    }
            
    private static void logToFile(String message) {
        Path file = Paths.get("/opt/tomcat/logs/kinetics-agent.log");

        try {
            if(!Files.exists(file)) Files.createFile(file);

            Files.write(
                file,
                (message + "\n").getBytes(), 
                StandardOpenOption.APPEND
            );
        } catch(Exception exception) {
            System.err.println("EXECPTION WRITING TO FILE");
            exception.printStackTrace(System.err);
        }
    }
}
