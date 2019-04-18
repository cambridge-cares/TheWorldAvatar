package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVWriter;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

//@WebServlet(urlPatterns = {"/NuclearAgent/startsimulation", "/NuclearAgent/processresult"})
public class NuclearAgent extends HttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	
	public static String csvFile = "D:/JPS/JParkSimulator-git/JPS_POWSYS/testres/Landlots.csv";
	public static String csvFile2 = "D:/JPS/JParkSimulator-git/JPS_POWSYS/testres/Loadpoints.csv";
	String rs_mechanism;
	OntModel jenaOwlModel = null;
	OntModel jenaOwlModel2 = null;
	
	private Logger logger = LoggerFactory.getLogger(NuclearAgent.class);
	
	public static synchronized ResultSet queryFromOWLFile(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    

		return results;
	}
	
	public static void copyFile(String from, String to) throws IOException{ 
	    File original=new File(from);
		File copied = new File(
	    	      to);
	    	    FileUtils.copyFile(original, copied);
		}


	
	public void runGAMS() throws IOException, InterruptedException {
        System.out.println("Start");
        System.out.println("separator= "+File.separator);
        String executablelocation ="C:/GAMS/win64/26.1/gams.exe";
        //String folderlocation ="D:/Users/KADIT01/Documents/gamsdir/projdir/";
        String folderlocation ="C:/JPS_DATA/workingdir/JPS_POWSYS/";
        String[] cmdArray = new String[5];
        
        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation + "final.gms";
        cmdArray[2] = "WDIR="+folderlocation;
        cmdArray[3] = "SCRDIR="+folderlocation;
        cmdArray[4] = "LO=2";
//      cmdArray[2] = "WDIR="+folderlocation + "TMP";
//      cmdArray[3] = "SCRDIR="+folderlocation + "TMP";
        
        String cmdArrayinstring=cmdArray[0]+" "+cmdArray[1]+","+cmdArray[2]+","+cmdArray[3]+" "+cmdArray[4];
        
		//System.out.println(cmdArrayinstring);
        //Process p = Runtime.getRuntime().exec(cmdArray);
		   //p.waitFor();
		String startbatCommand ="C:/JPS_DATA/workingdir/JPS_POWSYS/gamsexecute.bat";
		
		ArrayList<String> groupcommand= new ArrayList<String>();
		//groupcommand.add("start");
		//groupcommand.add("C:/JPS_DATA/workingdir/JPS_POWSYS/gamsexecute.bat");
		//groupcommand.add("C:/GAMS/win64/26.1/gams.exe");
		//groupcommand.add("C:/JPS_DATA/workingdir/JPS_POWSYS/final.gms,WDIR=C:/JPS_DATA/workingdir/JPS_POWSYS/,SCRDIR=C:/JPS_DATA/workingdir/JPS_POWSYS/");
		//groupcommand.add("LO=2");
		
        
	CommandHelper.executeSingleCommand(folderlocation,startbatCommand);
		//CommandHelper.executeCommands(folderlocation, groupcommand);   
        System.out.println("Done");
        

	}
	
	
	public void createNewCSV2(ArrayList<String[]>lotinfo, String csvFileout ,String flag) throws IOException {
		CSVWriter writer = new CSVWriter(new FileWriter(csvFileout));
		int numberofinfo=lotinfo.size();
		if(flag.contentEquals("landlot")) {
			String[]header= {"id","ys","xs","as","dcs"}; //for landlot
			writer.writeNext(header,false);
		}
		else if(flag.contentEquals("bus")){
			String[]header= {"id","yp","xp","Dp","rhop"}; //for bus
			writer.writeNext(header,false);	
		}
		
		for(int a=0;a<numberofinfo;a++) {
			String[]content= {lotinfo.get(a)[0],lotinfo.get(a)[1],lotinfo.get(a)[2],lotinfo.get(a)[3],lotinfo.get(a)[4]};
    		writer.writeNext(content,false);
			
		}
		writer.close();
		
	}
	
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String path = request.getServletPath();
		System.out.println("path= "+path);
		if ("/NuclearAgent/startsimulation".equals(path)) {
		
			JSONObject jofornuc = AgentCaller.readJsonParameter(request);
			
			String lotiri = null;
			String iriofnetwork = null;
	
			try {
				lotiri = jofornuc.getString("landlot");
				iriofnetwork = jofornuc.getString("electricalnetwork");
				System.out.println("it's going here");
				startSimulation(lotiri, iriofnetwork);
			} catch (JSONException | InterruptedException e1) {
				System.out.println("some error happen");
				logger.error(e1.getMessage(), e1);
				e1.printStackTrace();
			}
			
			
		} else if ("/NuclearAgent/processresult".equals(path)) {
			
			try {
				List<String> result = processSimulationResult();
				JSONObject resultjson = new JSONObject().put("plantirilist", result);
				AgentCaller.printToResponse(resultjson.toString(), response);	
			} catch (NumberFormatException | URISyntaxException e) {
				logger.error(e.getMessage(), e);
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		}	
	}
	
	public void startSimulation(String lotiri, String iriofnetwork) throws IOException, InterruptedException {
		
		
		String outputdir="C:/JPS_DATA/workingdir/JPS_POWSYS/inputlandlots.csv";
		prepareCSVLandlot(lotiri,outputdir); //used to create csv
    
		//-----------------------------------------1st input file finished-------------------------------------------------------------------	
		String outputdir2="C:/JPS_DATA/workingdir/JPS_POWSYS/inputloadpoints.csv";
        prepareCSVLoad(iriofnetwork,outputdir2); //used to create csv
		
        //-----------------------------------------2nd input file finished-------------------------------------------------------------------		
	

        copyFile("D:\\JPS\\JParkSimulator-git\\JPS_POWSYS\\testres\\constants_req.csv","C:\\JPS_DATA\\workingdir\\JPS_POWSYS\\constants_req.csv");
        //-----------------------------------------3rd input file finished-------------------------------------------------------------------
    
   
        copyFile("D:\\JPS\\JParkSimulator-git\\JPS_POWSYS\\testres\\parameters_req.csv","C:\\JPS_DATA\\workingdir\\JPS_POWSYS\\parameters_req.csv");
        //-----------------------------------------4th input file finished-------------------------------------------------------------------
    
        //temporary unused
        runGAMS();
	}
	
	public List<String> processSimulationResult() throws NumberFormatException, IOException, URISyntaxException {
	
		//String csvfileoutputgams="C:\\JPS_DATA\\workingdir\\JPS_POWSYS\\results.csv";
		String csvfileoutputgams="C:\\JPS_DATA\\workingdir\\JPS_POWSYS\\resultsex.csv"; //just for temporary before the model runs
		//   recreate the nuclear powerplant on flight
		NuclearKBCreator in= new NuclearKBCreator();
		System.out.println("starting conversion to owl file");
		ArrayList<String> result=in.startConversion(csvfileoutputgams);
		return result;
	}

	public void prepareCSVLandlot(String lotiri,String outputdir) throws IOException {
		jenaOwlModel = ModelFactory.createOntologyModel();	
		jenaOwlModel.read(lotiri, null);
		
		

		String lotsInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?xvalue ?yvalue ?areavalue ?distancevalue "
				
				+ "WHERE {?entity  a  j1:Landlot  ." 
				+ "?entity   j5:hasSurfaceGeometry ?sur ."
				+ "?sur   j5:has_area ?surarea ."
				+ "?surarea   j2:hasValue ?vsurarea ."
				+ "?vsurarea   j2:numericalValue ?areavalue ."
				
				+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
				+ "?coorsys   j7:hasProjectedCoordinate_x ?x ."
				+ "?x   j2:hasValue ?xval ."
				+ "?xval   j2:numericalValue ?xvalue ."
				+ "?coorsys   j7:hasProjectedCoordinate_y ?y ."
				+ "?y   j2:hasValue ?yval ."
				+ "?yval   j2:numericalValue ?yvalue ."
				
				+ "?entity   j1:hasDistanceToClosestWaterSources ?distance ."
				+ "?distance   j2:hasValue ?distval ."
				+ "?distval   j2:numericalValue ?distancevalue ."
								
				+ "}";
		
	ResultSet rs_landlot = NuclearAgent.queryFromOWLFile(lotsInfo,jenaOwlModel); 

	ArrayList<String[]> totallotresult= new ArrayList<String[]>();
	
	IriMapper mapper= new IriMapper();
	int index=1;
    while(rs_landlot.hasNext()) {
    	QuerySolution qs_p = rs_landlot.nextSolution();
		Resource name = qs_p.getResource("entity") ;     //extract the lots name 
	    //String stName = name.toString().split("ID")[1];
	    
	    String iri = name.toString();
	    mapper.add(iri, ""+"s"+index, "lot");
	    
	    
	    Literal x = qs_p.getLiteral("xvalue") ;     //extract the x centre
	    String stx = x.getString();   
	    Literal y = qs_p.getLiteral("yvalue") ;     //extract the y centre
	    String sty = y.getString(); 
	    Literal area = qs_p.getLiteral("areavalue") ;     //extract the area
	    String areast = area.getString();
	    Literal distance = qs_p.getLiteral("distancevalue") ;     //extract the area
	    String distancest = distance.getString();
	    
	    String [] queryresult= new String[5];
	    //queryresult[0]=stName;
	    queryresult[0]="s"+index;
	    queryresult[1]=sty;
	    queryresult[2]=stx;
	    queryresult[3]=areast;
	    queryresult[4]=distancest;
	    
	    totallotresult.add(queryresult);
	    index++;
    }
   
    
//    int number=totallotresult.size();
//	
//	//ordering process
//	ArrayList<String[]> totallotresultordered= new ArrayList<String[]>();
//	int init=1;
//	while(totallotresultordered.size()<number) {
//		for(int r=0;r<number;r++) {
//			if(totallotresult.get(r)[0].split("s")[1].contentEquals(String.valueOf(init))) {
//				totallotresultordered.add(totallotresult.get(r));					
//			}
//		}
//		init++;
//	}
	
   // mapper.serialize("C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforlot.csv");
    
    //make csv file for the landlots

    createNewCSV2(totallotresult,outputdir,"landlot");
    System.out.println("landlots input ok");
    
	}

	public String prepareCSVLoad(String iriofnetwork,String outputdir) throws IOException {
		
		
		
		String electricalnodeInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				
				+ "WHERE {?entity  a  j2:CompositeSystem  ." 
				+ "?entity   j2:hasSubsystem ?component ."								
				+ "}";
		
		jenaOwlModel2 = ModelFactory.createOntologyModel();	
		jenaOwlModel2.read(iriofnetwork, null); 
		ResultSet rs_electricalnode = NuclearAgent.queryFromOWLFile(electricalnodeInfo,jenaOwlModel2); 	
		
		ArrayList<String> totalnodeelectricresult= new ArrayList<String>();
		
		while(rs_electricalnode.hasNext()) {
			QuerySolution qs_p = rs_electricalnode.nextSolution();
			Resource iriofnode = qs_p.getResource("component");
		    String stiriofnode = iriofnode.toString();  
		    
		    totalnodeelectricresult.add(stiriofnode);
		}
		
		
		String busInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?xvalue ?yvalue ?activepowervalue "
				
				+ "WHERE {?entity  a  j1:BusNode  ." 
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?Pd ."
				+ "?Pd  a  j3:PdBus  ."
				+ "?Pd  j2:hasValue ?vpd ."
				+ "?vpd   j2:numericalValue ?activepowervalue ."
				
				+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
				+ "?coorsys   j7:hasProjectedCoordinate_x ?x ."
				+ "?x   j2:hasValue ?xval ."
				+ "?xval   j2:numericalValue ?xvalue ."
				+ "?coorsys   j7:hasProjectedCoordinate_y ?y ."
				+ "?y   j2:hasValue ?yval ."
				+ "?yval   j2:numericalValue ?yvalue ."
								
				+ "}";
		
		int numberofiri=totalnodeelectricresult.size();
   	
		ArrayList<String[]> totalnodebusresult= new ArrayList<String[]>();
		
		IriMapper mapper= new IriMapper();
		
		double mean = 0.0079;
		double variance = 0.004;
		double stddev = Math.sqrt(variance);
		int idcounter=1;
		for(int t=1;t<=numberofiri;t++) {
			OntModel jenaOwlModel3  = ModelFactory.createOntologyModel();	
			
			jenaOwlModel3.read(totalnodeelectricresult.get(t-1), null); 
			ResultSet rs_busnode = NuclearAgent.queryFromOWLFile(busInfo,jenaOwlModel3);
			
			
		    while(rs_busnode.hasNext()) {
		    	QuerySolution qs_p = rs_busnode.nextSolution();
		    	Resource iriofbusnode = qs_p.getResource("entity");
			    //String stiriofbusnode = "p"+Integer.valueOf(iriofbusnode.toString().split("EBus-")[2]);
			    
			    
			    
			    String iri = iriofbusnode.toString();
			    mapper.add(iri, ""+"p"+idcounter, "bus");
			    
			    Literal xvalue=qs_p.getLiteral("xvalue");
			    String stxvalue = xvalue.toString();
			    Literal yvalue=qs_p.getLiteral("yvalue");
			    String styvalue = yvalue.toString();
			    Literal Pdvalue=qs_p.getLiteral("activepowervalue");
			    String stPdvalue = Pdvalue.toString().replace("^^","@").split("@")[0];
			    
			    String [] queryresult= new String[5];
			    queryresult[0]="p"+idcounter;
			    queryresult[1]=styvalue;
			    queryresult[2]=stxvalue;
			    queryresult[3]=stPdvalue;
			    
			    boolean popDensityFound = false;
				while (!popDensityFound) {

					double popDensity = stddev * new Random().nextGaussian() + mean;
					if (popDensity > 0) {
					      popDensityFound = true;
					      queryresult[4]=String.valueOf(popDensity);
					}
				}
			    
			    totalnodebusresult.add(queryresult);
			    idcounter++;
		    }
		   
		}


		
		/**
		//ordering process
		int number=totalnodebusresult.size();
		ArrayList<String[]> totalnodebusresultordered= new ArrayList<String[]>();
		int init=1;
		while(totalnodebusresultordered.size()<number) {
			//System.out.println("size0= "+totalnodebusresultordered.size());
			for(int r=0;r<number;r++) {
				if(totalnodebusresult.get(r)[0].split("p")[1].contentEquals(String.valueOf(init))) {
					totalnodebusresultordered.add(totalnodebusresult.get(r));					
				}
			}
			init++;
		}**/
		
		
		
		// mapper.serialize("C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforbus.csv");
		
		createNewCSV2(totalnodebusresult,outputdir,"bus");
		
		System.out.println("bus input ok");
		return outputdir;
	}

}
