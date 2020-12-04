package uk.ac.cam.cares.jps.agent.matlab;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

/**
 * Matlab Agent developed for setting-up and running electrical network The files for Matlab
 * execution should be placed in user.home//matlab folder
 * 
 * @author Gourab Karmakar (gourab.karmakar@cares.cam.ac.uk)
 */
@WebServlet("/JPSMatlabAgent")
public class JPSMatlabAgent extends JPSAgent {
  private static final long serialVersionUID = 1L;
  public static final String MESSAGE_KEY = "File generated and located at:";
  public static final String SUCCESS_MESSAGE_KEY = "Completed and executed";
  public static final String TEMP_INPUT_FILE = "/matlab/matInput.dat";
  public static final String TEMP_BATCH_FILE = "/matlab/call_matlab.bat";
  public static final String TEMP_SCRIPT_FILE = "/matlab/Run_Script.m";
  public static final int STARTLINE = 69;
  public static final int NUMLINES = 5;
  public static int LINENUMBER = 1;

  /**
   * Read the gPROMS output file from RDF4J repository. Validates input file. Create input file for
   * Matlab. Run the script using a batch file. Deletes the temporary files.
   */
  @Override
  public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    JSONObject jo = AgentCaller.readJsonParameter(request);
    String current = System.getProperty("user.home");
    String activePowerFilePath = null;
    String agentiri = gPROMSAgent.GPROMS_AGENT_URL;
    List<String> lst = null;
    JPSMatlabAgent iri = new JPSMatlabAgent();
    activePowerFilePath = iri.queryRDF4J(agentiri, lst);
    // JSONObject param = new JSONObject().put("key", activePowerFilePath);
    if (validateInput(activePowerFilePath)) {
      JPSMatlabAgent app = new JPSMatlabAgent();
      app.appendFile(activePowerFilePath);
      // Create file path for batch file
      String batchFile = current + TEMP_BATCH_FILE;
      System.out.printf(MESSAGE_KEY + batchFile + "\n");
      // File path for Matlab script file
      String scriptFile = current + TEMP_SCRIPT_FILE;
      System.out.println(MESSAGE_KEY + scriptFile + "\n");
      // Command string for Matlab
      String cmd = "matlab -nodisplay -nosplash -nodesktop -r \"run('" + scriptFile + "');exit;\"";
      JPSMatlabAgent exe = new JPSMatlabAgent();
      exe.batchFile(batchFile, scriptFile, cmd);
      // Delete the temporary file
      File tempFile = new File(activePowerFilePath);
      tempFile.delete();
      String pathToSettingFile = current + gPROMSAgent.TEMP_SETTINGS_FILE;
      JPSMatlabAgent now = new JPSMatlabAgent();
      now.delete(pathToSettingFile, STARTLINE, NUMLINES);
    } else {
      System.out.println(gPROMSAgent.UNKNOWN_REQUEST);
    }
    return jo;
  }

  /**
   * Validates input parameters specific to the Agent to decide whether<br>
   * the execution request can be served. The method checks whether the input files required for the
   * agent execution are present at the required location
   */
  public boolean validateInput(String requestparam) {
    try {
      // String str = requestparam.getString("key");
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
      Runtime rs = Runtime.getRuntime();
      try {
        rs.exec(batchFile).waitFor();
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
}


