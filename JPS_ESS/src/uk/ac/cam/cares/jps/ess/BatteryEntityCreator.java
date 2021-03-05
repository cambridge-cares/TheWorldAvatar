package uk.ac.cam.cares.jps.ess;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.InputValidator;


@WebServlet(urlPatterns = { "/CreateBattery" })
/** creates appropriate battery OWL based on LocateBattery
 * 
 * @author Laura Ong
 *
 */
public class BatteryEntityCreator extends JPSAgent {
	
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private OntClass coordinateclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass valueclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass powerbalanceclass= null;
	private ObjectProperty hasActivePowerInjection= null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasdimension = null;
	private ObjectProperty referto = null;
	
	private DatatypeProperty numval = null;
	static Individual degree;
	static Individual MW;
	static Individual xaxis;
	static Individual yaxis;
	static Individual length;
	
	/** assign property values to variables
	 * 
	 * @param jenaOwlModel
	 */
	protected void initOWLClasses(OntModel jenaOwlModel) {
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		valueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		powerbalanceclass= jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#ActivePowerBalance");
		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		hasActivePowerInjection=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerInjection");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		referto = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#refersToAxis");
		hasdimension = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDimension");
		
		length=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#length");
		xaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#x-axis");
		yaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#y-axis");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		MW=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");
	}
	
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {

		if (!validateInput(requestParams)) {
			throw new JSONException("ESSOptimizationAgent: Input parameters not found.\n");
		}
		String ENIRI=requestParams.getString("electricalnetwork");
		String storagetype=requestParams.getString("storage");
		
		Double valueboundary=0.3; //later is extracted from the battery type
		OntModel model = EnergyStorageSystem.readModelGreedy(ENIRI);
		
		JSONArray listbat;
		try {
			listbat = createBatteryOwlFile(model, storagetype,valueboundary);

			
			requestParams.put("batterylist", listbat);
		} catch (IOException e) {
			e.printStackTrace();
		}catch (Exception e) {
			e.printStackTrace();
		}
		return requestParams;
		
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String storageFormat = requestParams.getString("storage");
	        boolean q = InputValidator.checkIfValidIRI(storageFormat);
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean v = InputValidator.checkIfValidIRI(ENIRI);
	        
	        return q&v;
        }catch (Exception ex) {
        	return false;
        }
	}	
	/** queries for Electric cable, and returns electric line, loss, and connected buses
	 * 
	 * @param model OntModel of electrical network
	 * @param valueboundary mark at which to query. If larger, add to list
	 * @return List<String[]>
	 */
	public List<String[]> prepareSelectedBranch(OntModel model, double valueboundary){
		String branchoutputInfo  = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j4", "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")				
				.addVar("?entity").addVar("?vplossvalue").addVar("?bus1").addVar("?bus2")
				.addWhere("?entity" ,"a", "j1:UndergroundCable")
				.addWhere("?entity" ,"j2:isModeledBy", "?model")
				.addWhere("?model" ,"j5:hasModelVariable", "?ploss")
				
				.addWhere("?ploss" ,"a", "j3:PLoss")
				.addWhere("?ploss" ,"j2:hasValue", "?vploss")
				.addWhere("?vploss" ,"j2:numericalValue", "?vplossvalue")
				
				.addWhere("?entity" ,"j4:hasInput", "?bus1")
				.addWhere("?entity" ,"j4:hasOutput", "?bus2").buildString();
		
		
		
		List<String[]> newresult= new ArrayList<String[]>();
		List<String[]> resultList= EnergyStorageSystem.queryResult(model,branchoutputInfo  );
		for(int d=0;d<resultList.size();d++) {
			if(Double.valueOf(resultList.get(d)[1])>=valueboundary){
				newresult.add(resultList.get(d));
			}
		}
		
		return newresult;
		
	}
	/** Selects the respective buses, and return their midpoint locations
	 * 
	 * @param model
	 * @param busirichosen
	 * @param busirichosen2
	 * @return
	 */
	public double[] prepareStorageLocation(OntModel model,String busirichosen,String busirichosen2){
		
		List<double[]>group= new ArrayList<double[]>();
		
		String iri=busirichosen;
		for(int x=0;x<2;x++) {
			if(x==1) {
				iri=busirichosen2;
			}
			//requires iri to be allocated first as a Node, rather than copying and pasting in iri
			SelectBuilder sb = new SelectBuilder();
			
			String buscoordinate  = sb.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
					.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
					.addVar("?valueofx").addVar("?valueofy")
					.addWhere("<"+iri+">" ,"j7:hasGISCoordinateSystem", "?coorsys")
					.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_x", "?x")
					.addWhere("?x" ,"j2:hasValue", "?xval").addWhere("?xval" ,"j2:numericalValue", "?valueofx")
					.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_y", "?y")
					.addWhere("?y" ,"j2:hasValue", "?yval").addWhere("?yval" ,"j2:numericalValue", "?valueofy")
					.buildString();
			List<String[]> resultList = EnergyStorageSystem.queryResult(model, buscoordinate);
			double[]coordinatebusresult= new double[2];
			coordinatebusresult[0]=Double.valueOf(resultList.get(0)[0]);
			coordinatebusresult[1]=Double.valueOf(resultList.get(0)[1]);
			group.add(coordinatebusresult);
			
		}
		
		double xbat = (group.get(0)[0] + group.get(1)[0]) / 2;
		double ybat = (group.get(0)[1] + group.get(1)[1]) / 2;	
		

		double[]coordinateresult= new double[2];
		
		coordinateresult[0]=xbat;
		coordinateresult[1]=ybat;
		
		
		return coordinateresult;
		
		
	}
	
	/** generates the OWL files for the batteries
	 * 
	 * @param model
	 * @param resultofbattery
	 * @param valueboundary
	 * @return
	 * @throws IOException
	 */
	public JSONArray createBatteryOwlFile(OntModel model, String resultofbattery,double valueboundary) throws IOException {
		//ArrayList<String[]> resultfrommodelbranch = readResultfromtxt(dir + "/outputBranch" + "OPF" + ".txt", 6);
		List<String[]> resultfrommodelbranch = prepareSelectedBranch(model, valueboundary);
		int size=resultfrommodelbranch.size();
		int d=0;

		QueryBroker broker=new QueryBroker();
		JSONArray listofbat= new JSONArray();
			while(d<size) {
					double[]coordinate=prepareStorageLocation(model,resultfrommodelbranch.get(d)[2],resultfrommodelbranch.get(d)[3]);
					double x=coordinate[0];
					double y=coordinate[1];
					double capacity=Double.valueOf(resultfrommodelbranch.get(d)[1]);
					String typebat=resultofbattery.split("#")[1];
					OntModel bat= JenaHelper.createModel(resultofbattery);
					String indexline=resultfrommodelbranch.get(d)[0].split("#ELine-")[1];

					initOWLClasses(bat);
					String iriprefix="http://www.jparksimulator.com/kb/batterycatalog/";
					Individual battery=bat.getIndividual(iriprefix+typebat+".owl#"+typebat);
					
					Individual gencoordinate = coordinatesystemclass.createIndividual(iriprefix + typebat+".owl#CoordinateSystem_of_"+typebat+"-"+indexline);
					Individual xgencoordinate = coordinateclass.createIndividual(iriprefix + typebat+".owl#x_coordinate_of_"+typebat+"-"+indexline);
					Individual ygencoordinate = coordinateclass.createIndividual(iriprefix + typebat+".owl#y_coordinate_of_"+typebat+"-"+indexline);
					Individual xgencoordinatevalue = valueclass.createIndividual(iriprefix + typebat+".owl#v_x_coordinate_of_"+typebat+"-"+indexline);
					Individual ygencoordinatevalue = valueclass.createIndividual(iriprefix + typebat+".owl#v_y_coordinate_of_"+typebat+"-"+indexline);
					Individual activepowerbalance=powerbalanceclass.createIndividual(iriprefix + typebat+".owl#ActivePowerInjection_of_"+typebat+"-"+indexline);
					Individual activepowerbalancevalue=scalarvalueclass.createIndividual(iriprefix + typebat+".owl#V_ActivePowerInjection_of_"+typebat+"-"+indexline);
					
//					System.out.println("relation= "+hascoordinatesystem.getURI());
					battery.addProperty(hascoordinatesystem,gencoordinate);
					gencoordinate.addProperty(hasx,xgencoordinate);
					gencoordinate.addProperty(hasy,ygencoordinate);
					xgencoordinate.addProperty(hasvalue, xgencoordinatevalue);
					xgencoordinate.addProperty(referto, xaxis);
					xgencoordinate.addProperty(hasdimension, length);
					ygencoordinate.addProperty(hasvalue, ygencoordinatevalue);
					ygencoordinate.addProperty(referto, yaxis);
					ygencoordinate.addProperty(hasdimension, length);
					xgencoordinatevalue.setPropertyValue(numval, bat.createTypedLiteral(new Double (x)));
					xgencoordinatevalue.addProperty(hasunit, degree);
					ygencoordinatevalue.setPropertyValue(numval, bat.createTypedLiteral(new Double (y)));
					ygencoordinatevalue.addProperty(hasunit, degree);
					
					battery.addProperty(hasActivePowerInjection,activepowerbalance);
					activepowerbalance.addProperty(hasvalue, activepowerbalancevalue);
					activepowerbalancevalue.setPropertyValue(numval, bat.createTypedLiteral(new Double (capacity)));
					activepowerbalancevalue.addProperty(hasunit, MW);
					
					String finalcontent=JenaHelper.writeToString(bat);
					
					String newiri = QueryBroker.getIriPrefix() + "/sgp/jurongisland/jurongislandpowernetwork/"+typebat+"-"+indexline+".owl";
					//String newiri="http://www.jparksimulator.com/kb/
					finalcontent=finalcontent.replace(iriprefix+typebat+".owl",newiri); //individual file name changed
					finalcontent=finalcontent.replace("#"+typebat, "#"+typebat+"-"+indexline);//main instance name changed
					
					broker.putOld(newiri,finalcontent);
					listofbat.put(newiri+"#"+typebat+"-"+indexline);
//					indbat.put(x);
//					indbat.put(y);
					//listofbat.put(indbat);
				
				
				d++;
			}
			return listofbat;
	}

}
