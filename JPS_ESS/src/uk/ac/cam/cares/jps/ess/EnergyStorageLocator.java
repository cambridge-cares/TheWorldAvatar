package uk.ac.cam.cares.jps.ess;

import java.io.IOException;
import java.io.StringReader;
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

import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;


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
	
	public ArrayList<String[]> readResultfromtxt(String outputfiledir, int colnum) throws IOException {
		ArrayList<String[]> entryinstance = new ArrayList<String[]>();
		
		logger.info("reading result from " + outputfiledir);
		String content = new QueryBroker().readFileLocal(outputfiledir);
		StringReader stringreader = new StringReader(content);
		CSVReader reader = null;
		try {
			reader = new CSVReader(stringreader, '\t');
			//CSVReader reader = new CSVReader(new FileReader(outputfiledir), '\t');
			String[] record;
			while ((record = reader.readNext()) != null) {
				int element = 0;
				String[] entityline = new String[colnum];
				for (String value : record) {
	
					entityline[element] = value;
					element++;
				}
				entryinstance.add(entityline);
	
			}
		} finally {
			reader.close();
		}
		return entryinstance;
	}
	
	public double[] prepareBatteryLocationData(String indexline, String baseUrl, OntModel model) throws IOException {
		String content = new QueryBroker().readFileLocal(baseUrl + "/mappingforbranch" + ".csv");
		// System.out.println("dir= "+content);
		List<String[]> readinglist = MatrixConverter.fromCsvToArray(content);
		int r = readinglist.size();
		String branchInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "SELECT ?entity ?num1 ?num2 " + "WHERE {?entity  a  j1:UndergroundCable  ."
				+ "?entity   j2:hasInput ?num1 ." + "?entity   j2:hasOutput ?num2 ." + "}";

		String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT ?entity ?valx ?valy " + "WHERE {?entity  a  j1:BusNode  ."
				+ "?entity   j3:hasGISCoordinateSystem ?coor ." + "?coor  j3:hasProjectedCoordinate_x ?x ."
				+ "?x  j2:hasValue ?valuex ." + "?valuex  j2:numericalValue ?valx ."
				+ "?coor  j3:hasProjectedCoordinate_y ?y ." + "?y  j2:hasValue ?valuey ."
				+ "?valuey  j2:numericalValue ?valy ." + "}";

		ResultSet resultSet = JenaHelper.query(model, busInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListbus = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		List<ElectricalComponentObject> buslist = new ArrayList<ElectricalComponentObject>();

		for (int x = 0; x < resultListbus.size(); x++) {
			ElectricalComponentObject bus = new ElectricalComponentObject(resultListbus.get(x)[0]);
			bus.setx(Double.valueOf(resultListbus.get(x)[1]));
			bus.sety(Double.valueOf(resultListbus.get(x)[2]));
			buslist.add(bus);
		}

		int t = 0;
		double[] ans = new double[2];
		ans[0] = 0.0;
		ans[1] = 0.0;
		while (t < r) {
			if (readinglist.get(t)[1].equals(indexline)) {
				String result2 = new QueryBroker().queryFile(readinglist.get(t)[0], branchInfo);
				String[] keys2 = JenaResultSetFormatter.getKeys(result2);

//				System.out.println("keys="+keys2.length);
				List<String[]> resultListbranch = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
//				System.out.println("sizeofresultbranch="+resultListbranch.size());
				String bus1 = resultListbranch.get(0)[1];
				String bus2 = resultListbranch.get(0)[2];
				double bus1x = 0.0;
				double bus1y = 0.0;
				double bus2x = 0.0;
				double bus2y = 0.0;
				for (int h = 0; h < buslist.size(); h++) {
					if (buslist.get(h).getObjectIRI().contains(bus1)) {
						bus1x = buslist.get(h).getx();
						bus1y = buslist.get(h).gety();
					}
					if (buslist.get(h).getObjectIRI().contains(bus2)) {
						bus2x = buslist.get(h).getx();
						bus2y = buslist.get(h).gety();
					}
				}
				double xbat = (bus1x + bus2x) / 2;
				double ybat = (bus1y + bus2y) / 2;
				System.out.println("x= " + xbat);
				System.out.println("y= " + ybat);

				ans[0] = xbat;
				ans[1] = ybat;
			}
			t++;
		}
		return ans;
	}
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject joforess = AgentCaller.readJsonParameter(request);
		String ENIRI=joforess.getString("electricalnetwork");
		String storagetype=joforess.getString("storage");
		
		Double valueboundary=0.3;
		OntModel model = readModelGreedy(ENIRI);
		
		createBatteryOwlFile(model, storagetype,valueboundary);
		
		
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
				+ "SELECT ?entity ?vploss ?bus1 ?bus2 "

				+ "WHERE {?entity  a  j1:UndergroundCable  ." 
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?ploss ." 
				+ "?ploss  a  j3:PLoss  ." 
				+ "?ploss  j2:hasValue ?vploss ." // ploss
				
				+ "?entity   j6:hasInput ?bus1 ."
				+ "?entity   j6:hasOutput ?bus2 ."
				+ "FILTER (xsd:double(?vploss) >= "+valueboundary+") " 
				+ "}";
		
		

		ResultSet resultSet = JenaHelper.query(model, branchoutputInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		
		for(int d=0;d<resultList.size();d++) {
			
		}
		
		return resultList;
		
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
					String newiri = QueryBroker.getIriPrefix() + "/sgp/jurongisland/jurongislandpowernetwork/"+typebat+"-"+String.format("%03d", d+1)+".owl";
					//String newiri="http://www.jparksimulator.com/kb/
					finalcontent=finalcontent.replace(iriprefix+typebat+".owl",newiri); //individual file name changed
					
					broker.putOld(newiri,finalcontent);
					indbat.put(newiri);
					indbat.put(x);
					indbat.put(y);
					listofbat.put(indbat);
				
				
				d++;
			}
			return listofbat;
	}

}
