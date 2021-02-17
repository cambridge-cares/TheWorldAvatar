package uk.ac.cam.cares.jps.powsys.util;

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
}
