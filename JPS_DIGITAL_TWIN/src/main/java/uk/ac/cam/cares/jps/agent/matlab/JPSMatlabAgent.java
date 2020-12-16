package uk.ac.cam.cares.jps.agent.matlab;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.apache.http.client.methods.HttpPost;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

/**
 * Matlab Agent developed for setting-up and running electrical network The files for Matlab
 * execution should be placed in user.home//matlab folder
 * 
 * @author Gourab Karmakar (gourab.karmakar@cares.cam.ac.uk)
 */
@WebServlet(urlPatterns = {"/JPSMatlabAgent/startSimulation", "/JPSMatlabAgent/processResult"})
public class JPSMatlabAgent extends JPSAgent {
  public static final String SIM_START_PATH = "/JPSMatlabAgent/startSimulation";
  public static final String SIM_PROCESS_PATH = "/JPSMatlabAgent/processResult";
  public static final String KEY_WATCH = "watch";
  public static final String KEY_CALLBACK_URL = "callback";
  private static final long serialVersionUID = 1L;
  public static final String MESSAGE_KEY = "File generated and located at:";
  public static final String SUCCESS_MESSAGE_KEY = "Completed and executed";
  public static final String TEMP_INPUT_FILE = "/matlab/matInput.dat";
  public static final String TEMP_BATCH_FILE = "/matlab/call_matlab.bat";
  public static final String TEMP_SCRIPT_FILE = "/matlab/Run_Script.m";
  public static final String PATH_FREQUENCY_FILE = "/matlab/freq.csv";
  public static final String MATLAB_AGENT_URL =
      "http://www.theworldavatar.com/kb/agents/Service__matlab.owl#Service";
  public static final String ELECTRICAL_SYSTEM_IRI = "\\matlab\\electrical_system.owl";
  public static final String PARAMETER = "\\matlab\\param.mat";
  public static String ELECTRICAL_SYSTEM =
      "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl";
  public static final int STARTLINE = 69;
  public static final int NUMLINES = 5;
  public static int LINENUMBER = 1;

  /**
   * Read the gPROMS output file from RDF4J repository. Validates input file. Create input file for
   * Matlab. Run the script using a batch file. Deletes the temporary files.
   */
  @Override
  public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    JSONObject responseParams = requestParams;
    String path = request.getServletPath();
    System.out.println("path= " + path);
    String current = System.getProperty("user.home");
    String activePowerFilePath = null;
    String agentiri = gPROMSAgent.GPROMS_AGENT_URL;
    List<String> lst = null;
    JPSMatlabAgent iri = new JPSMatlabAgent();
    activePowerFilePath = iri.queryRDF4J(agentiri, lst);
    if (SIM_START_PATH.equals(path)) {
      JSONObject jofornuc = requestParams;
      if (validateInput(activePowerFilePath)) {
        JPSMatlabAgent app = new JPSMatlabAgent();
        String str = current + ELECTRICAL_SYSTEM_IRI;
        String outFile = current + PARAMETER;
        app.appendFile(activePowerFilePath);
        ELECTRICAL_SYSTEM = gPROMSAgent.CHEMICAL_PROCESS_SYSTEM;
        // Delete the temporary file
        File tempFile = new File(activePowerFilePath);
        tempFile.delete();
        JPSMatlabAgent.queryBuilder(str, outFile);
        // JPSMatlabAgent now = new JPSMatlabAgent();
        // now.delete(pathToSettingFile, STARTLINE, NUMLINES);
        // Create file path for batch file
        String batchFile = current + TEMP_BATCH_FILE;
        batchFile = batchFile.replace("\\", "/");
        System.out.printf(MESSAGE_KEY + batchFile + "\n");
        // File path for Matlab script file
        String scriptFile = current + TEMP_SCRIPT_FILE;
        scriptFile = scriptFile.replace("\\", "/");
        System.out.println(MESSAGE_KEY + scriptFile + "\n");
        // Command string for Matlab
        String cmd =
            "matlab -nodisplay -nosplash -nodesktop -r \"run('" + scriptFile + "');exit;\"";
        JPSMatlabAgent exe = new JPSMatlabAgent();
        exe.batchFile(batchFile, scriptFile, cmd);
        // watcher to look for output files.
        File dest = new File(System.getProperty("user.home") + PATH_FREQUENCY_FILE);
        String pathToFrequency = dest.getAbsolutePath();
        pathToFrequency = pathToFrequency.replace("\\", "/");
        notifyWatcher(jofornuc, pathToFrequency,
            request.getRequestURL().toString().replace(SIM_START_PATH, SIM_PROCESS_PATH));
      } else {
        System.out.println(gPROMSAgent.UNKNOWN_REQUEST);
      }
    } else if (SIM_PROCESS_PATH.equals(path)) {
      System.out.println("Testing whether the else if lopp is executed");
      File dest = new File(System.getProperty("user.home") + PATH_FREQUENCY_FILE);
      String pathToFrequency = dest.getAbsolutePath();
      pathToFrequency = pathToFrequency.replace("\\", "/");
      MetaDataAnnotator.annotateWithTimeAndAgent(pathToFrequency, gettingFilecreationtime(dest),
          MATLAB_AGENT_URL);
      JSONObject jObject = new JSONObject();
      String resultStart = AgentCaller.executeGetWithJsonParameter("ElChemoAgent/SpinElectrical",
          jObject.toString());
      System.out.println(resultStart);
    }
    return responseParams;
  }

  /**
   * notifies the watcher to return with the callback.
   */
  private void notifyWatcher(JSONObject agentArgs, String filePath, String callbackIRI) {
    agentArgs.put(KEY_WATCH, filePath);
    agentArgs.put(KEY_CALLBACK_URL, callbackIRI);
    System.out.println("The keyvalueamp for AWS watcher that is an argument:"
        + KeyValueMap.getInstance().get("url.jps_aws"));
    System.out.println("The agent arguments for execute are :" + agentArgs.toString());
    execute(KeyValueMap.getInstance().get("url.jps_aws"), agentArgs.toString(),
        HttpPost.METHOD_NAME);
  }

  /**
   * Validates input parameters specific to the Agent to decide whether<br>
   * the execution request can be served. The method checks whether the input files required for the
   * agent execution are present at the required location
   */
  public boolean validateInput(String requestparam) {
    try {
      String str = requestparam;
      if (new String(str).equals(gPROMSAgent.TEMP_DIRECTORY)) {
        return true;
      } else {
        return false;
      }
    } catch (Exception e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }

  /**
   * Query RDF4J for pump IRI.
   */
  public String queryRDF4J(String agentiri, List<String> lst) {
    String csvFilePath = null;
    String resultFromRDF4J =
        MetaDataQuery.queryResources(null, null, null, agentiri, null, null, null, lst);
    String[] keys = JenaResultSetFormatter.getKeys(resultFromRDF4J);
    List<String[]> listmap =
        JenaResultSetFormatter.convertToListofStringArrays(resultFromRDF4J, keys);
    for (String[] str : listmap) {
      for (String s : str) {
        if (isFile(s)) {
          csvFilePath = s;
          break;
        }
      }
      break;
    }
    return (csvFilePath);
  }

  /**
   * Validate file path.
   */
  private boolean isFile(String path) {
    if (path == null) {
      return true;
    }
    return new File(path).isFile();
  }

  /**
   * Get the values starting from row 2 and store it in array Loop the array till end and multiply
   * ActivePower values with 0.5 in a new array key to get the reactive power Create a new CSV file
   * and write it into the output directory user.home/matlab Matlab input filename: matInput.dat
   * Create a batch file to execute MATLAB from command line
   */
  void appendFile(String pathToInputFile) {
    // Appending reactive power value on the Pump_power CSV file
    BufferedReader csvReader = null;
    try {
      csvReader = new BufferedReader(new FileReader(pathToInputFile));
    } catch (FileNotFoundException e) {
      throw new JPSRuntimeException(e.getMessage());
    }
    String row;
    try {
      ArrayList<ArrayList<String>> output = new ArrayList<ArrayList<String>>();
      row = csvReader.readLine();
      while ((row = csvReader.readLine()) != null) {
        String[] input = row.split(",");
        double input2 = Double.parseDouble(input[1]) * 0.5;
        ArrayList<String> inner = new ArrayList<String>();
        inner.add(input[0]);
        inner.add(input[1]);
        inner.add(Double.toString(input2));
        output.add(inner);
      }
      // close the reader
      csvReader.close();
      // Write the ArrayList into CSV into the path specified
      String matInputFile = System.getProperty("user.home") + TEMP_INPUT_FILE;
      matInputFile = matInputFile.replace("\\", "/");
      System.out.printf(MESSAGE_KEY + matInputFile + "\n");
      FileWriter csvWriter = new FileWriter(matInputFile);
      for (List<String> rowData : output) {
        csvWriter.append(String.join(",", rowData));
        csvWriter.append("\n");
      }
      // close the writer
      csvWriter.flush();
      csvWriter.close();
    } catch (IOException e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }

  /**
   * Create a batch file to execute MATLAB from command line
   */
  void batchFile(String batchFile, String scriptFile, String cmd) {
    // Creating batch file
    try {
      File file = new File(batchFile);
      FileWriter writer = new FileWriter(batchFile, true);
      writer.write(cmd);
      writer.close();
      // Execute batch file
      Process pb = Runtime.getRuntime().exec(batchFile);
      BufferedReader reader = new BufferedReader(new InputStreamReader(pb.getInputStream()));
      while ((reader.readLine()) != null) {
      }
      try {
        pb.waitFor();
      } catch (InterruptedException e) {
        throw new JPSRuntimeException(e.getMessage());
      }
      System.out.printf(SUCCESS_MESSAGE_KEY);
      file.delete();
    } catch (IOException e) {
      throw new JPSRuntimeException(e.getMessage());
    }
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
   * Delete the temporarily created values
   */
  void delete(String filename, int startline, int numlines) {
    try {
      BufferedReader br = new BufferedReader(new FileReader(filename));
      // String buffer to store contents of the file
      StringBuffer sb = new StringBuffer("");
      String line;
      while ((line = br.readLine()) != null) {
        // Store each valid line in the string buffer
        if (LINENUMBER < startline || LINENUMBER >= startline + numlines)
          sb.append(line + "\n");
        LINENUMBER++;
      }
      if (startline + numlines > LINENUMBER)
        System.out.println(gPROMSAgent.UNKNOWN_REQUEST);
      br.close();
      FileWriter fw = new FileWriter(new File(filename));
      // Write entire string buffer into the file
      fw.write(sb.toString());
      fw.close();
    } catch (Exception e) {
      throw new JPSRuntimeException(e.getMessage());
    }
  }

  /**
   * Query Builder
   */
  static void queryBuilder(String filePath, String outputFilePath) {
    SelectBuilder sb = new SelectBuilder().addPrefix("electrical", ELECTRICAL_SYSTEM)
        .addPrefix("system", gPROMSAgent.UPPER_LEVEL).addPrefix("rdf", gPROMSAgent.RDF)
        .addVar(gPROMSAgent.TEMP).addWhere(gPROMSAgent.VAR, "rdf:type", "system:ScalarValue")
        .addWhere(gPROMSAgent.VAR, "system:value", gPROMSAgent.TEMP);
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
        System.out.println(solution.getLiteral(gPROMSAgent.TEMP).getFloat());
        resultList.add(solution.getLiteral(gPROMSAgent.TEMP).getFloat());
        try {
          FileWriter fw = new FileWriter(outputFilePath, true);
          // the true will append the new data
          for (int k = 0; k < resultList.size(); k++) {
            if (k == 0) {
              fw.write("Transformer_Rating\n");
              fw.write(resultList.get(k).toString());
            }
            if (k == 1) {
              fw.write("\nTurns__Ratio\n");
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
}


