package uk.ac.cam.cares.jps.powsys.util;

import java.io.IOException;
import java.util.List;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;

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
	  /** get emission data from each generator. 
	   * 
	   * @return
	   */
	  public static SelectBuilder getGenEmission() {
		  SelectBuilder genInfo = new SelectBuilder()
			        .addPrefix("j1", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
			        .addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
			        .addPrefix("j9", "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#")
			        .addPrefix("technical_system","http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#")
			        .addVar("?entity").addVar("?V_Actual_CO2_Emission").addVar("?V_Design_CO2_Emission")
			        .addWhere("?entity", "a", "j1:PowerGenerator")
			        .addWhere("?entity", "technical_system:realizes", "?generation")
			        .addWhere("?generation", "j9:hasEmission", "?emission")
			        .addWhere("?emission", "a", "j9:Actual_CO2_Emission")
			        .addWhere("?emission","j2:hasValue", "?valueemission")
			        .addWhere("?valueemission" ,"j2:numericalValue", "?V_Actual_CO2_Emission")
			        .addWhere("?generation", "j9:hasEmission", "?v_emission")
			        .addWhere("?v_emission", "a", "j9:CO2_emission")
			        .addWhere("?v_emission","j2:hasValue", "?valueemission_d")
			        .addWhere("?valueemission_d" ,"j2:numericalValue", "?V_Design_CO2_Emission");
		  return genInfo;
	  }
	  
	  /**
	   * Used in CarbonTaxAgent and AggregationEmissionAgent
	   * @param iriofnetwork
	   * @return
	   */
	    public static List<String[]> provideGenlist(String iriofnetwork) {
	        String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
	                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
	                + "SELECT ?entity "
	                + "WHERE {?entity  a  j1:PowerGenerator  ."
	                + "FILTER EXISTS {?entity j2:isSubsystemOf ?plant } " //filtering gen 001 as it is slackbus
	                + "}";


	        OntModel model = Util.readModelGreedy(iriofnetwork);
	        List<String[]> resultListfromquery = queryResult(model, gennodeInfo);

	        return resultListfromquery;
	    }
	    /** reads the topnode into an OntModel of all its subsystems. 
	     * @param iriofnetwork
	     * @return
	     */
	    public static OntModel readModelGreedy(String iriofnetwork) { //model will get all the offsite wtf, transportation and food court
	      SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
	          .addWhere("?entity" ,"a", "j2:CompositeSystem").addWhere("?entity" ,"j2:hasSubsystem", "?component");
	      String wasteInfo = sb.build().toString();

	      QueryBroker broker = new QueryBroker();
	      return broker.readModelGreedy(iriofnetwork, wasteInfo);
	    }
}
