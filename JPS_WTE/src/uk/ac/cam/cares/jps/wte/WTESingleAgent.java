package uk.ac.cam.cares.jps.wte;

import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns= {"/processresult"})
public class WTESingleAgent extends JPSHttpServlet {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/** Find offsite technologies that use technology
	 * 
	 */
	public static String Offsiteoutput = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "SELECT ?entity ?Tech1 " 
			+ "WHERE {"
			+ "?entity   j1:useTechnology ?Tech1 ."  
			+ "}"
			+ "ORDER BY DESC(?Tech1)";

	
	/** derive property that defines numerical values as described in the ontology
	 * 
	 * @param jenaOwlModel
	 * @return
	 */
	private DatatypeProperty getNumericalValueProperty(OntModel jenaOwlModel) {
		return jenaOwlModel.getDatatypeProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
	}
	
	/** derive property that defines subsystem relationships as described in the ontology
	 * 
	 * @param jenaOwlModel (OntModel)
	 * @return
	 */
	private ObjectProperty getHasSubsystemRelation(OntModel jenaOwlModel) {
		return jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
	}
	

	/** derive property that defines delivery of waste
	 * 
	 * @param jenaOwlModel (OntModel)
	 * @return
	 */
	private ObjectProperty getisDeliveredTo(OntModel jenaOwlModel) {
		return jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#isDeliveredTo");
	}
	
	/** main function. Reads the values in and copies the templates back. 
	 * 
	 */
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String baseUrl= requestParams.optString("baseUrl", "testFood");
		String wasteIRI=requestParams.optString("wastenetwork", "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		OntModel model= WastetoEnergyAgent.readModelGreedy(wasteIRI);
		try {
			//read for FC details
			List<String[]> resu =  readAndDump(model,WastetoEnergyAgent.FCQuery);
			//select in year 1
			List<String[]> fcMapping = createFoodCourt(resu);
			//properties of OnsiteTech
			List<String[]> propertydataonsite = readAndDump(model, WastetoEnergyAgent.WTFTechOnsiteQuery);
			List<String[]> inputoffsitedata = readResult(baseUrl,"n_unit_max_offsite.csv");
			List<String> onsiteiricomplete=updateinOnsiteWT(fcMapping,baseUrl,propertydataonsite,15);
			List<String[]> sitemapping= updateNewFC(baseUrl,inputoffsitedata);
			updateFCHelper(sitemapping);
			updateKBForSystem(wasteIRI, baseUrl, WastetoEnergyAgent.wasteSystemOutputQuery,onsiteiricomplete); //for waste system	
			updateinOffsiteWT(inputoffsitedata,baseUrl, 15);
		 }catch (Exception e) {
			e.printStackTrace();
		}			 
		 
		return requestParams;
	}
	/** reads the result from the csv file produced and returns as List<String[]>
	 * 
	 * @param baseUrl String
	 * @param filename name of the file. 
	 * @return
	 * @throws IOException
	 */
	public List<String[]> readResult(String baseUrl,String filename) throws IOException {

        String outputFile = baseUrl + "/"+filename;
        String csv = new QueryBroker().readFileLocal(outputFile);
        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		
		return simulationResult;
	}
	/** scans in model and reads out according to Query
	 * 
	 * @param model Ontological model created in processRequestParameters
	 * @return result of Query
	 */
	public List<String[]> readAndDump(OntModel model, String mainquery) {
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysfc = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysfc);
		return resultList;
	}
	/** helper function for createFC for later conversion
	 * Basically gets the iris, and xy coordinates
	 * @param resultList
	 * @return
	 */
	public List<String[]> createFoodCourt(List<String[]> resultList) {
	 List<String[]> inputdata = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			//entity, x, y
			String[] mapper = {resultList.get(d)[5],resultList.get(d)[1], resultList.get(d)[2] };// only extract and y
			if (resultList.get(d)[4].contentEquals("1")) { //self select for year
				inputdata.add(mapper);
			}
		}
		return inputdata;
	}
	/** creates the Onsite Waste Treatment Facility OWL file
	 * 
	 * @param inputdata {[List<String[]>]} list of FC 
	 * @param baseUrl String
	 * @return List<String> list of IRIS of onsite WTF
	 * @throws Exception
	 */
		
	public List<String> updateinOnsiteWT(List<String[]> inputdata,
			String baseUrl,
			List<String[]> propertydata, int index) throws Exception { //creating needed onsite WTF while returning complete set of onsite iri
		
		List<String[]> unitofonsite=readResult(baseUrl,"year by year_number of units (onsite)_"+index+".csv");
		List<String[]>onsiteunitmapping=new ArrayList<String[]>();
		List<String> mappedonsiteiri=new ArrayList<String>();
		int size3=unitofonsite.size();
		int colamount3=unitofonsite.get(0).length;
		for(int x=0;x<size3;x++) { //currently 1 with one tech
			for(int y=0;y<colamount3;y++) { 
				String[]linemapping= new String[size3];//109 elements	
				BigDecimal bd = new BigDecimal(unitofonsite.get(x)[y]);
				double newval= Double.parseDouble(bd.toPlainString());
				if(newval>=1) {
					//onsite IRIs not created!

					linemapping[0]=bd.toPlainString();
					int FCname = y +1;
					mappedonsiteiri.add("http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/OnSiteWasteTreatment-" 
							+String.format("%03d", FCname) +".owl#OnSiteWasteTreatment-"+String.format("%03d", FCname));

					onsiteunitmapping.add(linemapping);	
					
				}else {
					linemapping[0]="0";
					onsiteunitmapping.add(linemapping);	
				}

				
			}

		}
		WTEKBCreator converter = new WTEKBCreator();
		//create Onsite WTF
		converter.startConversion("onsitewtf",inputdata,onsiteunitmapping,propertydata);
//		mappedonsiteiri=converter.onsiteiri;
		converter.onsiteiri = mappedonsiteiri;
		return mappedonsiteiri;
	}
	public List<String[]> updateNewFC(String baseUrl,
			List<String[]> inputdataoffsite) throws IOException{
		List<String[]>sitemapping=new ArrayList<String[]>();
		for (int i = 1; i<= 15; i++) {
			List<String[]> clusterOnsite = readResult(baseUrl,"year by year_Waste flow relation (onsite)_"+i+".csv");
			List<String[]> clusterOffsite = readResult(baseUrl,"year by year_Waste flow relation (offsite)_"+i+".csv");
			 
			//check for onsite if cluster
			int size=clusterOnsite.size();//size = no of FC Actual
			for (int j = 0; j<size;j++ ) {
				HashSet<String> s = new HashSet<>(Arrays.asList(clusterOnsite.get(j)));
				 if (s.size() == 1) {
					 //then it's offsite
					int y = Arrays.asList(clusterOffsite.get(j)).indexOf("1");
					int IndexOffsiteHeader = y % 3; // index 0,3,6 is the first wtf, 1,4,7 is the 2nd, 2,5,8 is
					// the 3rd
					String currentoffwtf = inputdataoffsite.get(0)[IndexOffsiteHeader];
					String[]linemapping= {Integer.toString(j+1),Integer.toString(0),currentoffwtf,Integer.toString(i)};
					sitemapping.add(linemapping);	
				 }else {
					 //otherwise onsite
					int FCname = j + 1;
					int y = Arrays.asList(clusterOnsite.get(j)).indexOf("1")+1;
					String currentwtf = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/OnSiteWasteTreatment-" 
							+String.format("%03d", y) +".owl#OnSiteWasteTreatment-"+String.format("%03d", y);
					String[]linemapping= {Integer.toString(FCname),Integer.toString(y),currentwtf,Integer.toString(i)};
					sitemapping.add(linemapping);
				 }
		        
			}
		}
		return sitemapping;
	}
	public void updateFCHelper( List<String[]>sitemapping) {
		int noOfFC = sitemapping.size()/15;
		for (int i = 1; i<= noOfFC; i++) {
			OntModel model = JenaHelper.createModel();
			
			String fc = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/FoodCourt-"+String.format("%03d", i)+".owl";
			model.read(fc, null);
			for (int j = 0; j< 15; j++) {
				String[] siteArray = sitemapping.get((i-1)+noOfFC*j);
				String fcWaste = fc + "#V_WasteProductionOfFoodCourt-"+String.format("%03d", i)+"_"+Integer.toString(j+1);
				Individual entity = model.getIndividual(fcWaste);
				Resource entityonsite = model.createResource(siteArray[2]);
				entity.addProperty(getisDeliveredTo(model), entityonsite);
				
			}
			String content = JenaHelper.writeToString(model);
			new QueryBroker().putOld(fc, content);

		}
	}
	
	/** if dump without cluster, sparql update is updated into onsite / offsite WTF directly
	 * 
	 * @param baseUrl
	 * @param inputdataonsite
	 * @param inputdataoffsite
	 * @param foodcourtmap
	 * @return
	 * @throws Exception
	 */
	public List<String> updateinFC(String baseUrl,List<String> inputdataonsite,List<String[]> inputdataoffsite,List<String[]> foodcourtmap) throws Exception { //update the fc and giving selected onsite iri list
		List<String>selectedOnsite=new ArrayList<String>();
		//both of them have row= fc amount, col represents onsite or offsite per tech
		List<String[]>treatedwasteon=readResult(baseUrl,"Treated waste (onsite).csv");
		//noOfFc x noOfOnsiteWTF
		List<String[]>onsitemapping=new ArrayList<String[]>();
		int size=treatedwasteon.size();
		for(int x=0;x<size;x++) {
			for(int y=0;y<size;y++) {
				String wastetransfer=treatedwasteon.get(x)[y]; //in ton/day
				if(Double.parseDouble(wastetransfer)>0.01) {
					String[]linemapping= {""+x,""+y,wastetransfer};
					onsitemapping.add(linemapping);
				}
			}
		}
		
		List<String[]>treatedwasteoff=readResult(baseUrl,"Treated waste (offsite).csv");
		//noOfFc x 3 x 3
		List<String[]>offsitemapping=new ArrayList<String[]>();
		int size2=treatedwasteoff.size();
		int colamount2=treatedwasteoff.get(0).length;
		for(int x=0;x<size2;x++) {
			for(int y=0;y<colamount2;y++) { //3tech*3instance
				String wastetransfer=treatedwasteoff.get(x)[y]; //in ton/day
				if(Double.parseDouble(wastetransfer)>0.01) {
					String[]linemapping= {""+x,""+y,wastetransfer};
					offsitemapping.add(linemapping);
				}
			}
		}
		
	
		
		String sparqlStart = "PREFIX OW:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> \r\n" 
		+"PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n"
			+ "INSERT DATA { \r\n";
		
		//outputdata= treated waste onsite
		//input data onsite=onsiteiri
		for (int d = 0; d < foodcourtmap.size(); d++) {// each iri of foodcourt
			int wasteindex = 1;

			StringBuffer b = new StringBuffer();
			if (onsitemapping.size() > 0) {
				String currentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#WasteDeliveredAmount-" + wasteindex;
				String valuecurrentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#V_WasteDeliveredAmount-"
						+ wasteindex;
				Double numfromres = Double.parseDouble(onsitemapping.get(d)[2]);
				int onsiteindex = Integer.valueOf(onsitemapping.get(d)[1]);
				String currentwtf = inputdataonsite.get(onsiteindex);
				b.append("<" + foodcourtmap.get(d)[0] + "> OW:deliverWaste <" + currentwaste + "> . \r\n");
				b.append("<" + currentwaste + "> a OW:WasteTransfer . \r\n");
				b.append("<" + currentwaste + "> OCPSYST:hasValue <" + valuecurrentwaste + "> . \r\n");
				b.append("<" + valuecurrentwaste + "> a OCPSYST:ScalarValue . \r\n");
				b.append("<" + valuecurrentwaste + "> OCPSYST:numericalValue " + numfromres + " . \r\n");
				b.append("<" + valuecurrentwaste
						+ "> OCPSYST:hasUnitOfMeasure <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_day> . \r\n");
				b.append("<" + currentwaste + "> OW:isDeliveredTo <" + currentwtf + "> . \r\n");
				wasteindex++;
				selectedOnsite.add(currentwtf);
			}

			if (offsitemapping.size() > 0) {
				String currentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#WasteDeliveredAmount-" + wasteindex;
				String valuecurrentwaste = foodcourtmap.get(d)[0].split("#")[0] + "#V_WasteDeliveredAmount-"
						+ wasteindex;
				Double numfromres = Double.parseDouble(offsitemapping.get(d)[2]);
				int offsiteindex = Integer.valueOf(offsitemapping.get(d)[1]);
				int IndexOffsiteHeader = offsiteindex % 3; // index 0,3,6 is the first wtf, 1,4,7 is the 2nd, 2,5,8 is
															// the 3rd
				String currentoffwtf = inputdataoffsite.get(0)[IndexOffsiteHeader];
				b.append("<" + foodcourtmap.get(d)[0] + "> OW:deliverWaste <" + currentwaste + "> . \r\n");
				b.append("<" + currentwaste + "> a OW:WasteTransfer . \r\n");
				b.append("<" + currentwaste + "> OCPSYST:hasValue <" + valuecurrentwaste + "> . \r\n");
				b.append("<" + valuecurrentwaste + "> a OCPSYST:ScalarValue . \r\n");
				b.append("<" + valuecurrentwaste + "> OCPSYST:numericalValue " + numfromres + " . \r\n");
				b.append("<" + valuecurrentwaste
						+ "> OCPSYST:hasUnitOfMeasure <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_day> . \r\n");
				b.append("<" + currentwaste + "> OW:isDeliveredTo <" + currentoffwtf + "> . \r\n");
						wasteindex++;
			}

			String sparql = sparqlStart + b.toString() + "} \r\n";
			try {
			      FileWriter myWriter = new FileWriter("C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\testFWec7e04f8-831f-43ab-a22d-9b91dc059b7b\\localhost_8080\\data\\78d15fd0-ff0d-4930-bf83-f0e5b93d85ae\\filename.txt");
			      myWriter.write(sparql);
			      myWriter.close();
			      
			    } catch (IOException e) {
			      System.out.println("An error occurred.");
			      e.printStackTrace();
			    }
			new QueryBroker().updateFileOLD(foodcourtmap.get(d)[0], sparql);

		}
		
		return selectedOnsite;
	}
	/** updates the knowledge base of the composite systems. 
	 * 
	 * @param iriofnetwork
	 * @param baseUrl
	 * @param queryupdate wasteSystemOutputQuery costs of composite systems
	 * @param onsiteiri
	 * @throws IOException
	 */
	public void updateKBForSystem(String iriofnetwork, String baseUrl, String queryupdate,List<String> onsiteiri) throws IOException {
		List<String[]>economic=readResult(baseUrl,"year by year_Economic output.csv");
		String result = new QueryBroker().queryFile(iriofnetwork,queryupdate);
		String[] keyswt = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
		System.out.println("answer number= " + resultList.size());
		OntModel model = JenaHelper.createModel();
		model.read(iriofnetwork, null);
		for (int ind = 0; ind < keyswt.length; ind++) {
			Individual inst = model.getIndividual(resultList.get(0)[ind]);
			inst.setPropertyValue(getNumericalValueProperty(model),
			model.createTypedLiteral(Double.parseDouble(economic.get(ind)[0])));
			
		}
		
		Individual entity = model.getIndividual(resultList.get(0)[0]);
		for(int wtfamount=0;wtfamount<onsiteiri.size();wtfamount++) {
			Resource entityonsite = model.createResource(onsiteiri.get(wtfamount));
			entity.addProperty(getHasSubsystemRelation(model), entityonsite);
		}
		
		
		String content = JenaHelper.writeToString(model);
		new QueryBroker().putOld(resultList.get(0)[0], content);

	}
	
	
	/** updates the OWL file for the Offsite Waste Treatment facilities. 
	 * 
	 * @param inputdata List<String[]>
	 * @param baseUrl String
	 * @throws Exception
	 */
	public void updateinOffsiteWT(List<String[]> inputdata,String baseUrl, int indexByYear) throws Exception {
		//assume inputdata= input offsite data
		List<String[]>unitofoffsite=readResult(baseUrl,
				"year by year_number of units (offsite)_"+indexByYear+".csv");
		System.out.println("it goes to the offsite update");
		//filter the arrayfirst to take only non zero values
		List<String[]>filtered=new ArrayList<String[]>();
		for(int r=0;r<unitofoffsite.size();r++) {
			for(int i=0;i<unitofoffsite.get(0).length;i++) {
				String element=unitofoffsite.get(r)[i];
				if(Double.parseDouble(element)>0.01) {
					String[]component= {""+r,inputdata.get(0)[i],element};
					filtered.add(component);
				}
			}
		}
		
		if(filtered.size()>0) {
			UpdateBuilder sb = new UpdateBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
					.addPrefix("plant", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#")
					.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
					.addVar("?CpropVal").addWhere("?entity" ,"a", "j6:Building")
					.addWhere("?entity" ,"plant:hasCapacity", "?capacity").addWhere("?capacity" ,"j2:hasValue", "?vProp")
					.addWhere("?vProp" ,"j2:numericalValue", "?CpropVal").addOrderBy("?capacity");
			Query q = sb.build();
			ResultSet resultSetx = JenaHelper.query(model, q.toString());
			
			SelectBuilder sb = new SelectBuilder().addPrefix("OW", "http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#")
					.addPrefix("OCPSYST", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
					
			String sparqlStart = "PREFIX OW:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> \r\n" 
					+"PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n"
						+ "INSERT DATA { \r\n";
			for(int w=0;w<filtered.size();w++) {
				StringBuffer b = new StringBuffer();
				String currentunit = filtered.get(w)[1].split("#")[0] + "#UnitDeviceOf-" + filtered.get(w)[1].split("#")[1]+"_"+indexByYear; //w is precaution if duplicate instance
				int numunit = Integer.valueOf(filtered.get(w)[2]);
				//String currentwtf = inputdataonsite.get(onsiteindex);
				//0=incineration
				//1=codigestion
				//2=anaerobic
				String result = new QueryBroker().queryFile(filtered.get(w)[1], Offsiteoutput);
				String[] keyswt = JenaResultSetFormatter.getKeys(result);
				List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
				String techiri=resultList.get(Integer.valueOf(filtered.get(w)[0]))[1];
				b.append("<" + techiri + "> OW:realizedByDevice <" + currentunit + "> . \r\n");
				b.append("<" + currentunit + "> a OW:WasteTreatmentDevice . \r\n");
				b.append("<" + currentunit + "> OW:usedInYear " + indexByYear + " . \r\n");
				b.append("<" + currentunit + "> OW:amountOfUnit " + numunit + " . \r\n");
				String sparql = sparqlStart + b.toString() + "} \r\n";
				new QueryBroker().updateFile(filtered.get(w)[1], sparql);
			}
		}
	}
	
}
