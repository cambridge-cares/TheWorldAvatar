package uk.ac.cam.cares.jps.ess;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;


@WebServlet(urlPatterns = { "/LocateEnergyStorage" })
public class EnergyStorageLocator extends JPSHttpServlet {
	
	
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
	
	public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject joforess = AgentCaller.readJsonParameter(request);
		String ENIRI=joforess.getString("electricalnetwork");
		String storagetype=joforess.getString("storage");
		
		Double valueboundary=0.3;
		OntModel model = readModelGreedy(ENIRI);
		
	 JSONArray listbat=createBatteryOwlFile(model, storagetype,valueboundary);
		
		JSONObject batterylist=new JSONObject ();
		batterylist.put("batterylist", listbat);
		AgentCaller.printToResponse(batterylist, response);
	}
	
	public List<String[]> prepareSelectedBranch(OntModel model, double valueboundary){
			
		
		String branchoutputInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+"PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
				+ "SELECT ?entity ?vplossvalue ?bus1 ?bus2 "

				+ "WHERE {?entity  a  j1:UndergroundCable  ." 
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?ploss ." 
				+ "?ploss  a  j3:PLoss  ." 
				+ "?ploss  j2:hasValue ?vploss ." // ploss
				+ "?vploss  j2:numericalValue ?vplossvalue ."
				+ "?entity   j4:hasInput ?bus1 ."
				+ "?entity   j4:hasOutput ?bus2 ."
				+ "}";
		
		List<String[]> newresult= new ArrayList<String[]>();
		ResultSet resultSet = JenaHelper.query(model, branchoutputInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

		for(int d=0;d<resultList.size();d++) {
			if(Double.valueOf(resultList.get(d)[1])>=valueboundary){
				newresult.add(resultList.get(d));
			}
		}
		
		return newresult;
		
	}
	
	public double[] prepareStorageLocation(OntModel model,String busirichosen,String busirichosen2){
		
		List<double[]>group= new ArrayList<double[]>();
		
		String iri=busirichosen;
		for(int x=0;x<2;x++) {
			if(x==1) {
				iri=busirichosen2;
			}
			
			String buscoordinate = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
					+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
					+ "SELECT ?valueofx ?valueofy "
					+ "WHERE {"
					//+ "?entity  a  j1:BusNode ."
					+ "<"+iri+">   j7:hasGISCoordinateSystem ?coorsys ."
					+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
					+ "?y  j2:hasValue ?vy ." 
					+ "?vy  j2:numericalValue ?valueofy ."

					+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
					+ "?x  j2:hasValue ?vx ." 
					+ "?vx  j2:numericalValue ?valueofx ."
					+ "}";
			ResultSet resultSet = JenaHelper.query(model, buscoordinate);
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
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
	
	
	public JSONArray createBatteryOwlFile(OntModel model, String resultofbattery,double valueboundary) throws IOException {
		//ArrayList<String[]> resultfrommodelbranch = readResultfromtxt(dir + "/outputBranch" + "OPF" + ".txt", 6);
		List<String[]> resultfrommodelbranch = prepareSelectedBranch(model, valueboundary);
		int size=resultfrommodelbranch.size();
		int d=0;

		QueryBroker broker=new QueryBroker();
		JSONArray listofbat= new JSONArray();
			while(d<size) {
					JSONArray indbat= new JSONArray();
					//double[]coordinate=prepareBatteryLocationData(resultfrommodelbranch.get(d)[0],dir,model);
					double[]coordinate=prepareStorageLocation(model,resultfrommodelbranch.get(d)[2],resultfrommodelbranch.get(d)[3]);
					double x=coordinate[0];
					double y=coordinate[1];
					double capacity=Double.valueOf(resultfrommodelbranch.get(d)[1]);
					String typebat=resultofbattery.split("#")[1];
					OntModel bat= JenaHelper.createModel(resultofbattery);

					initOWLClasses(bat);
					String iriprefix="http://www.jparksimulator.com/kb/batterycatalog/";
					Individual battery=bat.getIndividual(iriprefix+typebat+".owl#"+typebat);
					
					Individual gencoordinate = coordinatesystemclass.createIndividual(iriprefix + typebat+".owl#CoordinateSystem_of_"+typebat);
					Individual xgencoordinate = coordinateclass.createIndividual(iriprefix + typebat+".owl#x_coordinate_of_"+typebat);
					Individual ygencoordinate = coordinateclass.createIndividual(iriprefix + typebat+".owl#y_coordinate_of_"+typebat);
					Individual xgencoordinatevalue = valueclass.createIndividual(iriprefix + typebat+".owl#v_x_coordinate_of_"+typebat);
					Individual ygencoordinatevalue = valueclass.createIndividual(iriprefix + typebat+".owl#v_y_coordinate_of_"+typebat);
					Individual activepowerbalance=powerbalanceclass.createIndividual(iriprefix + typebat+".owl#ActivePowerInjection_of_"+typebat);
					Individual activepowerbalancevalue=scalarvalueclass.createIndividual(iriprefix + typebat+".owl#V_ActivePowerInjection_of_"+typebat);
					
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
					String indexline=resultfrommodelbranch.get(d)[0].split("#ELine-")[1];
					String newiri = QueryBroker.getIriPrefix() + "/sgp/jurongisland/jurongislandpowernetwork/"+typebat+"-"+indexline+".owl";
					//String newiri="http://www.jparksimulator.com/kb/
					finalcontent=finalcontent.replace(iriprefix+typebat+".owl",newiri); //individual file name changed
					
					broker.putOld(newiri,finalcontent);
					indbat.put(newiri+"#"+typebat+"-"+indexline);
//					indbat.put(x);
//					indbat.put(y);
					listofbat.put(indbat);
				
				
				d++;
			}
			return listofbat;
	}

}
