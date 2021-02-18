package uk.ac.cam.cares.jps.powsys.util;

import java.io.IOException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

public class Util {

	public static String getResourceDir(Object thisObject) {
		return AgentLocator.getCurrentJpsAppDirectory(thisObject) + "/res";
	}
	/** feeds a query and gets a result
	   * 
	   * @param model
	   * @param query
	   * @return
	   */
	  public static List<String[]> queryResult(OntModel model, String query) {
	    
	    ResultSet resultSet = JenaHelper.query(model, query);
	    String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	    String[] keys = JenaResultSetFormatter.getKeys(result);
	    List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
	    return resultListfromquery;
	  }
	  /** After realizing that you may not have GAMS as a system variable (Or don't have the permission rights)
	   * You can choose to change here
	   * 
	   * 
	   */
	  public static String getGAMSLocation() {
		  try {
			String gamsLocation = System.getenv("GAMSDIR").split(";")[0];
			gamsLocation =gamsLocation.replace("\\", "/");
			gamsLocation =gamsLocation.replace("//", "/");
			String executablelocation = gamsLocation+"/gams.exe";
			return executablelocation;
		  }catch (NullPointerException ex) {
			  //if gamsLocation was empty, select and change your native build
			   return  "C:/GAMS/win64/26.1/gams.exe"; //depends where is in claudius, or whatever machine this is installed in
		  }catch (Exception ex) {
			  //Not quite sure what else could trigger a 'not found' if System.getenv doesn't throw
			  // but you'll return it empty anyway. 
			  ex.printStackTrace();
			  return "";
			  
		  }
	  }
}
