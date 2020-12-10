package uk.ac.cam.cares.jps.agent.gPROMS;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentProperty;
import uk.ac.cam.cares.jps.agent.utils.ZipUtility;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.PostProcessing;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJob;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * gPROMS Agent developed for setting-up and running gPROMS chemical network on HPC. The input files
 * for gPROMS execution should be placed in user.home//input folder
 * 
 * @author Aravind Devanand (aravind@u.nus.edu)
 *
 */
@Controller
@WebServlet(urlPatterns = {gPROMSAgent.JOB_REQUEST_PATH, gPROMSAgent.JOB_STATISTICS_PATH,})
public class gPROMSAgent extends JPSAgent {
  private static final long serialVersionUID = 1L;
  private Logger logger = LoggerFactory.getLogger(gPROMSAgent.class);
  private File workspace;
  static JobSubmission jobSubmission;
  static SlurmJob slurmJob;
  public static ApplicationContext applicationContextgPROMSAgent;
  public static gPROMSAgentProperty gpROMSAgentProperty;
  public static final String UNKNOWN_REQUEST = "The request is unknown to the Agent";
  public static final String JOB_REQUEST_PATH = "/job/request";
  public static final String JOB_OUTPUT_REQUEST_PATH = "/job/output/request";
  public static final String JOB_STATISTICS_PATH = "/job/statistics";
  public static final String JOB_SHOW_STATISTICS_PATH = "/job/show/statistics";
  public static final String TEMP_SETTINGS_FILE = "/input/Settings.input";
  public static final String ENCRYPT_PATH = "/input/input";
  public static final String INPUT_PATH = "/input";
  public static final String DEBUTANISER_SECTION = "/input/debutaniser_section.owl";
  public static final String TEMP = "?Temp";
  public static final String VAR = "?x";
  public static final String GPROMS_AGENT_URL =
      "http://www.theworldavatar.com/kb/agents/Service__gPROMS.owl#Service";
  public static final String CHEMICAL_PROCESS_SYSTEM =
      "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#";
  public static final String UPPER_LEVEL =
      "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#";
  public static final String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
  public static String TEMP_DIRECTORY = null;

  public JSONObject produceStatistics(String input) throws IOException, gPROMSAgentException {
    logger.info("Received a request to send statistics.\n");
    // Initialises all properties required for this agent to set-up<br>
    // and run jobs. It will also initialise the unique instance of<br>
    // Job Submission class.
    initAgentProperty();
    return jobSubmission.getStatistics(input);
  }

  @RequestMapping(value = gPROMSAgent.JOB_SHOW_STATISTICS_PATH, method = RequestMethod.GET)
  @ResponseBody
  public String showStatistics() throws IOException, gPROMSAgentException {
    logger.info("Received a request to show statistics.\n");
    initAgentProperty();
    return jobSubmission.getStatistics();
  }

  /**
   * Starts the asynchronous scheduler to monitor quantum jobs.
   */
  public void init() throws ServletException {
    logger.info("---------- gPROMS Simulation Agent has started ----------");
    System.out.println(System.getProperty("user.dir"));
    ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
    gPROMSAgent gPROMSAgent = new gPROMSAgent();
    // initialising classes to read properties from the gPROMS-agent.properites file
    initAgentProperty();
    // In the following method call, the parameter getAgentInitialDelay-<br>
    // ToStartJobMonitoring refers to the delay (in seconds) before<br>
    // the job scheduler starts and getAgentPeriodicActionInterval<br>
    // refers to the interval between two consecutive executions of<br>
    // the scheduler.

    executorService.scheduleAtFixedRate(() -> {
      try {
        gPROMSAgent.monitorJobs();
      } catch (SlurmJobException e) {
        throw new JPSRuntimeException(e.getMessage());
      }
    }, gpROMSAgentProperty.getAgentInitialDelayToStartJobMonitoring(),
        gpROMSAgentProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
    logger.info("---------- gPROMS Simulation jobs are being monitored  ----------");
  }

  /**
   * Initialises the unique instance of the gpROMSAgentProperty class that<br>
   * reads all properties of gPROMSAgent from the kinetics-agent property file.<br>
   *
   * Initialises the unique instance of the SlurmJobProperty class and<br>
   * sets all properties by reading them from the kinetics-agent property file<br>
   * through the gPROMSAgent class.
   */
  public void initAgentProperty() {
    // initialising classes to read properties from the kinetics-agent.properites
    // file
    if (applicationContextgPROMSAgent == null) {
      applicationContextgPROMSAgent =
          new AnnotationConfigApplicationContext(gPROMSAgentConfiguration.class);
    }
    if (gpROMSAgentProperty == null) {
      gpROMSAgentProperty = applicationContextgPROMSAgent.getBean(gPROMSAgentProperty.class);
    }
    if (jobSubmission == null) {
      jobSubmission = new JobSubmission(gpROMSAgentProperty.getAgentClass(),
          gpROMSAgentProperty.getHpcAddress());
      jobSubmission.slurmJobProperty
          .setHpcServerLoginUserName(gpROMSAgentProperty.getHpcServerLoginUserName());
      jobSubmission.slurmJobProperty
          .setHpcServerLoginUserPassword(gpROMSAgentProperty.getHpcServerLoginUserPassword());
      jobSubmission.slurmJobProperty.setAgentClass(gpROMSAgentProperty.getAgentClass());
      jobSubmission.slurmJobProperty
          .setAgentCompletedJobsSpacePrefix(gpROMSAgentProperty.getAgentCompletedJobsSpacePrefix());
      jobSubmission.slurmJobProperty
          .setAgentFailedJobsSpacePrefix(gpROMSAgentProperty.getAgentFailedJobsSpacePrefix());
      jobSubmission.slurmJobProperty.setHpcAddress(gpROMSAgentProperty.getHpcAddress());
      jobSubmission.slurmJobProperty.setInputFileName(gpROMSAgentProperty.getInputFileName());
      jobSubmission.slurmJobProperty
          .setInputFileExtension(gpROMSAgentProperty.getInputFileExtension());
      jobSubmission.slurmJobProperty.setOutputFileName(gpROMSAgentProperty.getOutputFileName());
      jobSubmission.slurmJobProperty
          .setOutputFileExtension(gpROMSAgentProperty.getOutputFileExtension());
      jobSubmission.slurmJobProperty
          .setJsonInputFileName(gpROMSAgentProperty.getJsonInputFileName());
      jobSubmission.slurmJobProperty
          .setJsonFileExtension(gpROMSAgentProperty.getJsonFileExtension());
      jobSubmission.slurmJobProperty
          .setJsonFileExtension(gpROMSAgentProperty.getJsonFileExtension());
      jobSubmission.slurmJobProperty
          .setSlurmScriptFileName(gpROMSAgentProperty.getSlurmScriptFileName());
      jobSubmission.slurmJobProperty
          .setMaxNumberOfHPCJobs(gpROMSAgentProperty.getMaxNumberOfHPCJobs());
      jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(
          gpROMSAgentProperty.getAgentInitialDelayToStartJobMonitoring());
      jobSubmission.slurmJobProperty
          .setAgentPeriodicActionInterval(gpROMSAgentProperty.getAgentPeriodicActionInterval());
    }
  }

  /**
   * Receives and processes HTTP requests that match with the URL patterns listed in the annotations
   * of this class.
   */
  @Override
  public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    String path = request.getServletPath();
    System.out.println(
        "...........................A request has been received..........................");
    if (path.equals(gPROMSAgent.JOB_REQUEST_PATH)) {
      try {
        return setUpJob(requestParams.toString());
      } catch (SlurmJobException | IOException | gPROMSAgentException e) {
        throw new JPSRuntimeException(e.getMessage());
      }
    } else if (path.equals(gPROMSAgent.JOB_OUTPUT_REQUEST_PATH)) {
      JSONObject result = getSimulationResults(requestParams);
      return result;
    } else if (path.equals(gPROMSAgent.JOB_STATISTICS_PATH)) {
      try {
        return produceStatistics(requestParams.toString());
      } catch (IOException | gPROMSAgentException e) {
        throw new JPSRuntimeException(e.getMessage());
      }
    } else {
      System.out.println("Unknown request");
      throw new JPSRuntimeException(UNKNOWN_REQUEST);
    }
  }

  /**
   * Validates input parameters specific to gPROMS Agent to decide whether<br>
   * the job set up request can be served. The method checks wether the input files required for the
   * gPROMS agent execution are present at the required location
   */
  @Override
  public boolean validateInput(JSONObject requestParams) {
    boolean valid;
    try {
      File encrypt = new File(System.getProperty("user.home") + ENCRYPT_PATH);
      File inputFilegPROMS = new File(System.getProperty("user.home") + INPUT_PATH);
      FilenameFilter inputFilefilter = new FilenameFilter() {
        public boolean accept(File dir, String name) {
          String lowercaseName = name.toLowerCase();
          if (lowercaseName.endsWith(".input")) {
            return true;
          } else {
            return false;
          }
        }
      };
      FilenameFilter encryptFilefilter = new FilenameFilter() {
        public boolean accept(File dir, String name) {
          if (name.equals("Final_HPC.gENCRYPT")) {
            return true;
          } else {
            return false;
          }
        }
      };
      String inputfile[] = inputFilegPROMS.list(inputFilefilter);
      String encry[] = encrypt.list(encryptFilefilter);
      System.out.print(encry.length);
      if (inputfile.length >= 1 && encry.length >= 1) {
        valid = true;
      } else {
        valid = false;
      }
    } catch (Exception e) {
      throw new JPSRuntimeException(e.getMessage());
    }
    return valid;
  }

  /**
   * Checks the status of a job and returns results if it is finished and<br>
   * post-processing is successfully completed. If the job has terminated<br>
   * with an error or failed, then error termination message is sent. The JSON input for this
   * request has the following format: {"jobId": "login-skylake.hpc.cam.ac.uk_117804308649998"}
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
   */
  private JSONObject checkJobInCompletedJobs(String jobId) {
    JSONObject json = new JSONObject();
    // The path to the completed jobs folder.
    String completedJobsPath = workspace.getParent().concat(File.separator)
        .concat(gpROMSAgentProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace.getName());
    File completedJobsFolder = new File(completedJobsPath);
    if (completedJobsFolder.isDirectory()) {
      File[] jobFolders = completedJobsFolder.listFiles();
      for (File jobFolder : jobFolders) {
        if (jobFolder.getName().equals(jobId)) {
          try {
            String inputJsonPath = completedJobsPath.concat(File.separator)
                .concat(jobFolder.getName()).concat(File.separator)
                .concat(gpROMSAgentProperty.getReferenceOutputJsonFile());
            InputStream inputStream = new FileInputStream(inputJsonPath);
            return new JSONObject(FileUtil.inputStreamToString(inputStream));
          } catch (FileNotFoundException e) {
            return json.put("message",
                "The job has been completed, but the file that contains results is not found.");
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
   */
  private JSONObject checkJobInFailedJobs(String jobId) {
    JSONObject json = new JSONObject();
    // The path to the failed jobs folder.
    String failedJobsPath = workspace.getParent().concat(File.separator)
        .concat(gpROMSAgentProperty.getAgentFailedJobsSpacePrefix()).concat(workspace.getName());
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
   */
  private void monitorJobs() throws SlurmJobException {
    // Configures all properties required for setting-up and running a Slurm job.
    jobSubmission.monitorJobs();
    processOutputs();
  }

  /**
   * Monitors the currently running quantum jobs to allow new jobs to start.</br>
   * In doing so, it checks if the number of running jobs is less than the</br>
   * maximum number of jobs allowed to run at a time.
   */
  public void processOutputs() {
    workspace = jobSubmission.getWorkspaceDirectory();
    try {
      if (workspace.isDirectory()) {
        File[] jobFolders = workspace.listFiles();
        for (File jobFolder : jobFolders) {
          if (Utils.isJobCompleted(jobFolder) && !Utils.isJobOutputProcessed(jobFolder)) {
            boolean outcome = postProcessing(Paths.get(jobFolder.getAbsolutePath()));
            if (outcome) {
              // Success
              PostProcessing.updateJobOutputStatus(jobFolder);
            } else {
              // Failure
              Utils.modifyStatus(Utils.getStatusFile(jobFolder).getAbsolutePath(),
                  Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
            }
          }
        }
      }
    } catch (IOException e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }

  /**
   * Executes post-processing on the input job folder, returning true if the post-processing task
   * returns successfully.
   */
  public boolean postProcessing(Path jobFolder) throws IOException {
    // Find the job results ZIP
    Path archive = Paths.get(jobFolder.toString(),
        gpROMSAgentProperty.getOutputFileName() + gpROMSAgentProperty.getOutputFileExtension());
    if (!Files.exists(archive))
      throw new IOException("Cannot find expected archive at: " + archive);
    if (!Files.exists(archive) || Files.readAllBytes(archive).length <= 0) {
      return false;
    }
    ArrayList<String> result = new ArrayList<>();
    try (BufferedReader br = new BufferedReader(new FileReader(archive.toString()))) {
      while (br.ready()) {
        result.add(br.readLine());
      }
      br.close();
    }
    // Integer r is the line number of the required variable minus 1
    int r = 16038;
    int vars = Integer.parseInt(result.get(0));
    int n = result.size() / (vars + 1);
    // The required variable and the time index is stored into a float array
    float table[][] = new float[n][2];
    System.out.println(n);
    for (int i = 1; i < n; i++) {
      table[i][0] = Float.parseFloat(result.get((vars + 1) * i));
      table[i][1] = Float.parseFloat(result.get(r + (vars + 1) * i));
      System.out.println(table[i][0]);
    }
    exportDataToExcel(jobFolder.toString() + "/matlab.csv", table);
    JSONObject jo = new JSONObject();
    String resultStart = AgentCaller
        .executeGetWithJsonParameter("ElChemoAgent/JPSMatlabAgent/startSimulation", jo.toString());
    System.out.println(resultStart);
    return true;
  }

  /**
   * Extract values to csv file
   */
  public static void exportDataToExcel(String fileName, float[][] data)
      throws FileNotFoundException, IOException {
    File file = new File(fileName);
    if (!file.isFile())
      file.createNewFile();
    FileWriter csvWriter = new FileWriter(file);
    int rowCount = data.length;
    for (int i = 0; i < rowCount; i++) {
      int columnCount = data[i].length;
      float[][] values = new float[rowCount][columnCount];
      for (int j = 0; j < columnCount; j++) {
        values[i][j] = data[i][j];
        csvWriter.append(String.valueOf(values[i][j]));
        csvWriter.append(",");
      }
      csvWriter.append("\n");
    }
    csvWriter.flush();
    csvWriter.close();
    File dest = new File(System.getProperty("user.home") + "\\matlab\\matlab.csv");
    Files.copy(file.toPath(), dest.toPath());
    // Adding the matlab.csv file to the metadata repo
    String destURI = dest.getAbsolutePath();
    destURI = destURI.replace("\\", "/");
    TEMP_DIRECTORY = destURI;
    MetaDataAnnotator.annotateWithTimeAndAgent(destURI, gettingFilecreationtime(dest),
        GPROMS_AGENT_URL);
  }

  /**
   * Getting the time when file was modified for storing in the metadata annoattator
   */
  public static String gettingFilecreationtime(File file) {
    Path filePath = file.toPath();
    BasicFileAttributes attributes = null;
    try {
      attributes = Files.readAttributes(filePath, BasicFileAttributes.class);
    } catch (IOException exception) {
      throw new JPSRuntimeException(exception.getMessage());
    }
    String creationDate = new String(attributes.creationTime().toString());
    return (creationDate);
  }

  /**
   * Sets up a quantum job by creating the job folder and the following files</br>
   * under this folder:</br>
   * - the input file.</br>
   * - the Slurm script file.</br. - the Status file.</br>
   * - the JSON input file, which comes from the user request.</br>
   */
  public JSONObject setUpJob(String jsonString)
      throws IOException, gPROMSAgentException, SlurmJobException {
    String message = setUpJobOnAgentMachine(jsonString);
    JSONObject obj = new JSONObject();
    obj.put("jobId", message);
    return obj;
  }

  /**
   * Sets up the quantum job for the current input.
   */
  private String setUpJobOnAgentMachine(String jsonInput)
      throws IOException, gPROMSAgentException, SlurmJobException {
    initAgentProperty();
    long timeStamp = Utils.getTimeStamp();
    String jobFolderName = getNewJobFolderName(gpROMSAgentProperty.getHpcAddress(), timeStamp);
    System.out.println("Jobfolder is" + jobFolderName);
    Path temporaryDirectory1 = Paths.get(System.getProperty("user.home"), "." + jobFolderName);
    System.out.println("tempdir is" + temporaryDirectory1.toString());
    System.out.println("tempdir1 is" + temporaryDirectory1.toString());
    System.out.println("userdir is" + System.getProperty("user.dir"));
    System.out.println("scrptdir is" + gpROMSAgentProperty.getAgentScriptsLocation().toString());
    return jobSubmission.setUpJob(jsonInput,
        new File(URLDecoder.decode(getClass().getClassLoader()
            .getResource(gpROMSAgentProperty.getSlurmScriptFileName()).getPath(), "utf-8")),
        getInputFile(jsonInput, jobFolderName), timeStamp);
  }


  /**
   * Prepares input files, bundle them in a zip file and return the zip file to the calling method
   */
  private File getInputFile(String jsonInput, String jobFolderName)
      throws IOException, gPROMSAgentException {
    // Preparation of settings.input file
    // Extracting required variables from owl files
    String filePath = System.getProperty("user.home") + DEBUTANISER_SECTION;
    String outputFilePath = System.getProperty("user.home") + TEMP_SETTINGS_FILE;
    gPROMSAgent.queryBuilder(filePath, outputFilePath);
    // Compress all files in the temporary directory into a ZIP
    Path zipFile = Paths.get(System.getProperty("user.home") + "\\input.zip");
    // Create a temporary folder in the user's home location
    Path temporaryDirectory = Paths.get(System.getProperty("user.home") + "\\input");
    List<File> zipContents = new ArrayList<>();
    Files.walk(temporaryDirectory).map(Path::toFile).forEach((File f) -> zipContents.add(f));
    zipContents.remove(temporaryDirectory.toFile());
    new ZipUtility().zip(zipContents, zipFile.toString());
    // Return the final ZIP file
    return new File(zipFile.toString());
  }

  static void queryBuilder(String filePath, String outputFilePath) {
    SelectBuilder sb = new SelectBuilder().addPrefix("process", CHEMICAL_PROCESS_SYSTEM)
        .addPrefix("system", UPPER_LEVEL).addPrefix("rdf", RDF).addVar(TEMP)
        .addWhere(VAR, "rdf:type", "system:ScalarValue").addWhere(VAR, "system:value", TEMP);
    System.out.println(sb.toString());
    OntModel model = ModelFactory.createOntologyModel(OntModelSpec.RDFS_MEM);
    InputStream is;
    try {
      is = new FileInputStream(filePath);
      model.read(is, null);
      ResultSet resultSet = JenaHelper.query(model, sb.buildString());
      List<Float> resultList = new ArrayList<Float>();
      for (; resultSet.hasNext();) {
        QuerySolution solution = resultSet.nextSolution();
        System.out.println(solution.getLiteral(TEMP).getFloat());
        resultList.add(solution.getLiteral(TEMP).getFloat());
        try {
          FileWriter fw = new FileWriter(outputFilePath, true);
          // the true will append the new data
          for (int k = 0; k < resultList.size(); k++) {
            if (k == 0) {
              fw.write("Feed__T\n");
              fw.write(resultList.get(k).toString());
            }
            if (k == 1) {
              fw.write("\nFeed__P\n");
              fw.write(resultList.get(k).toString());
            }
          }
          fw.close();
        } catch (IOException ioe) {
          throw new JPSRuntimeException(ioe.getMessage());
        }
      }
    } catch (FileNotFoundException e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }

  /**
   * Produces a job folder name by following the schema hpcAddress_timestamp
   */
  public String getNewJobFolderName(String hpcAddress, long timeStamp) {
    return hpcAddress.concat("_").concat("" + timeStamp);
  }

  /**
   * Returns the job id.
   */
  public String getJobId(JSONObject jsonObject) {
    if (jsonObject.has("jobId")) {
      return jsonObject.get("jobId").toString();
    } else {
      return null;
    }
  }
}
