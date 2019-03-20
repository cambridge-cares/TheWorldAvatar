package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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




@WebServlet("/NuclearAgent")
public class Nuclear extends HttpServlet {
	
	public static String csvFile = "D:/JPS/JParkSimulator-git/JPS_POWSYS/testres/Landlots.csv";
	public static String csvFile2 = "D:/JPS/JParkSimulator-git/JPS_POWSYS/testres/Loadpoints.csv";
	String rs_mechanism;
	OntModel jenaOwlModel = null;
	OntModel jenaOwlModel2 = null;
	
	private Logger logger = LoggerFactory.getLogger(Nuclear.class);
	
	public static synchronized ResultSet queryFromOWLFile(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    

		return results;
	}
	
	public void runGAMS() {
		 System.out.println("Start");
         String[] cmdArray = new String[5];
         cmdArray[0] = "C:/GAMS/win64/24.3/" + File.separator + "gams.exe";
         cmdArray[1] = "C:/JPS_DATA/workingdir/JPS_POWSYS/gams/" + File.separator + "final.gms";
         //cmdArray[2] = "C:/JPS_DATA/workingdir/JPS_POWSYS/gams/" + File.separator + "tmp";
         cmdArray[2] = "C:/JPS_DATA/workingdir/JPS_POWSYS/gams/" + File.separator + "gams";
         cmdArray[3] = "LO=3";
         try {
                Process p = Runtime.getRuntime().exec(cmdArray);
                BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
                String s = null;
                while((s=stdInput.readLine()) !=null){
                   System.out.println(s);
                }
                p.waitFor();
         }
         catch (java.io.IOException e )
         {
                System.err.println(">>>>" + e.getMessage() );
                e.printStackTrace();
         }
         catch (InterruptedException e )
         {
                System.err.println(">>>>" + e.getMessage() );
                e.printStackTrace();
         }
         System.out.println("Done");
	}
	
	/*public void createCSV(String csvFileref,ArrayList<String[]>lotinfo,String csvFileout) throws IOException {
		
		CSVWriter writer = new CSVWriter(new FileWriter(csvFileout));
		String line = "";
        String cvsSplitBy = ",";
        int linereader=0;
        
    	try (BufferedReader br = new BufferedReader(new FileReader(csvFileref))) {
    		 
            while ((line = br.readLine()) != null) {
            	//System.out.println("linereader= "+linereader);
            	
            	if(linereader==0) {
            		System.out.println("skipped because it's header");
            	}
            	
            	else {
            		int elementperlineincsv=5; //assume the element in every line has 5
            		String[] entries2= new String[elementperlineincsv];
                    String[] iri = line.split(cvsSplitBy);
                    //System.out.println("content1= "+iri[0]);

                    for(int count=0;count<elementperlineincsv-1;count++){
                    	 entries2[count]=lotinfo.get(linereader-1)[count];
                    }
              		
                    entries2[elementperlineincsv-1]=iri[elementperlineincsv-1]; //last element of csv should be the distance for lot and rho for bus node 
            		  
              		
              		writer.writeNext(entries2);
            	}
            	linereader++;
            }
   	    
            writer.close();

        } catch (IOException e) {
            e.printStackTrace();
        }

	}*/
	
	public ArrayList<CSVdata> readCSV(String csvFileref,String separatorforindex) throws IOException {
		
		String line = "";
        String cvsSplitBy = ",";
        int linereader=0;
        ArrayList<CSVdata> taken= new ArrayList<CSVdata>();
    	try (BufferedReader br = new BufferedReader(new FileReader(csvFileref))) {
    		
    		
    		
            while ((line = br.readLine()) != null) {
            	
            	if(linereader==0) {
            		//System.out.println("skipped because it's header");
            	}
            	
            	else {
                    String[] iri = line.split(cvsSplitBy);
                    CSVdata ind=new CSVdata(iri[4]);
                    //System.out.println(iri[0]);
                    ind.setindex(Integer.valueOf(iri[0].split(separatorforindex)[1]));
                    taken.add(ind);
            	}
            	linereader++;
            }
//            System.out.println("taken size="+taken.size());
//            System.out.println("first data="+taken.get(0).getmaindata());
//            System.out.println("first data index="+taken.get(0).getindex());
        } catch (IOException e) {
            e.printStackTrace();
        }
    	return taken;
	}
	
	public void createNewCSV(String csvFileref,ArrayList<String[]>lotinfo,String csvFileout, String indexseparatorcsv,String indexseparatorowl) throws IOException {
		
		CSVWriter writer = new CSVWriter(new FileWriter(csvFileout));
		String line = "";
        String cvsSplitBy = ",";
        int linereader=0;
        
    	try (BufferedReader br = new BufferedReader(new FileReader(csvFileref))) {
    		 
            while ((line = br.readLine()) != null) {
            	//System.out.println("linereader= "+linereader);
            	
            	if(linereader==0) {
            		System.out.println("skipped because it's header");
            	}
            	
            	else {
            		int elementperlineincsv=5; //assume the element in every line has 5
            		String[] entries2= new String[elementperlineincsv];
                    for(int count=0;count<elementperlineincsv-1;count++){
                    	 entries2[count]=lotinfo.get(linereader-1)[count];
                    }
              		int numbofdata=readCSV(csvFileref,indexseparatorcsv).size();
              		
              		for(int a=0;a<numbofdata;a++) {
              			if(Integer.valueOf(entries2[0].split(indexseparatorowl)[1].split(".owl")[0])==(readCSV(csvFileref,indexseparatorcsv).get(a).getindex())){
              				entries2[elementperlineincsv-1]=readCSV(csvFileref,indexseparatorcsv).get(a).getmaindata();
              			}
              		}
              		            		
              		
              		writer.writeNext(entries2);
              		//System.out.println("written");
            	}
            	linereader++;
            }
   	    
            writer.close();

        } catch (IOException e) {
            logger.error(e.getMessage());
        }

	     //System.out.println(entries[0]);

	    // System.out.println("done");

	}
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jofornuc = AgentCaller.readJsonParameter(request);
		
		String lotiri = null;
		String iriofnetwork = null;

		try {
			lotiri = jofornuc.getString("landlot");
			iriofnetwork = jofornuc.getString("electricalnetwork");


		} catch (JSONException e1) {
			logger.error(e1.getMessage(), e1);
			e1.printStackTrace();
			
			
		}
		
		prepareCSVLandlot(lotiri);
    
//-----------------------------------------1st input file finished-------------------------------------------------------------------	
    
    prepareCSVLoad(iriofnetwork);
		
    //-----------------------------------------2nd input file finished-------------------------------------------------------------------		
	
    
    
    
    runGAMS();
	
	
	
	String csvfileoutputgams="";
    
//   recreate the nuclear powerplant on flight
	NuclearKBCreator in= new NuclearKBCreator();
	try {
		in.startConversion(csvfileoutputgams);
	} catch (URISyntaxException e) {
		logger.error(e.getMessage());
	}
	
	
	}

	public void prepareCSVLandlot(String lotiri) throws IOException {
		jenaOwlModel = ModelFactory.createOntologyModel();	
		jenaOwlModel.read(lotiri, null);
		
		

		String lotsInfo= "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?xvalue ?yvalue ?areavalue "
				
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
								
				+ "}";
		
	ResultSet rs_landlot = Nuclear.queryFromOWLFile(lotsInfo,jenaOwlModel); 

	ArrayList<String[]> totallotresult= new ArrayList<String[]>();
	
    while(rs_landlot.hasNext()) {
    	QuerySolution qs_p = rs_landlot.nextSolution();
		Resource name = qs_p.getResource("entity") ;     //extract the lots name 
	    String stName = name.toString();  
	    Literal x = qs_p.getLiteral("xvalue") ;     //extract the x centre
	    String stx = x.getString();   
	    Literal y = qs_p.getLiteral("yvalue") ;     //extract the y centre
	    String sty = y.getString(); 
	    Literal area = qs_p.getLiteral("areavalue") ;     //extract the area
	    String areast = area.getString();
	    
	    String [] queryresult= new String[4];
	    queryresult[0]=stName;
	    queryresult[1]=sty;
	    queryresult[2]=stx;
	    queryresult[3]=areast;
	    
	    totallotresult.add(queryresult);
    }
	
    //make csv file for the landlots
    createNewCSV(csvFile,totallotresult,"C:/JPS_DATA/workingdir/JPS_POWSYS/inputlandlots.csv","s","-IDs");
	}

	public void prepareCSVLoad(String iriofnetwork) throws IOException {
		String electricalnodeInfo= "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				
				+ "WHERE {?entity  a  j2:CompositeSystem  ." 
				+ "?entity   j2:hasSubsystem ?component ."								
				+ "}";
		
		jenaOwlModel2 = ModelFactory.createOntologyModel();	
		jenaOwlModel2.read(iriofnetwork, null); 
		ResultSet rs_electricalnode = Nuclear.queryFromOWLFile(electricalnodeInfo,jenaOwlModel2); 	
		
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
		for(int t=0;t<numberofiri;t++) {
			OntModel jenaOwlModel3  = ModelFactory.createOntologyModel();	
			//System.out.println("iri chosen= "+totalnodeelectricresult.get(t));
			jenaOwlModel3.read(totalnodeelectricresult.get(t), null); 
			ResultSet rs_busnode = Nuclear.queryFromOWLFile(busInfo,jenaOwlModel3);
			
			System.out.println("query ok");
			System.out.println("iri chosen= "+totalnodeelectricresult.get(t));
		    while(rs_busnode.hasNext()) {
		    	QuerySolution qs_p = rs_busnode.nextSolution();
		    	Resource iriofbusnode = qs_p.getResource("entity");
			    String stiriofbusnode = iriofbusnode.toString();
			    Literal xvalue=qs_p.getLiteral("xvalue");
			    String stxvalue = xvalue.toString();
			    Literal yvalue=qs_p.getLiteral("yvalue");
			    String styvalue = yvalue.toString();
			    Literal Pdvalue=qs_p.getLiteral("activepowervalue");
			    String stPdvalue = Pdvalue.toString().replace("^^","@").split("@")[0];
			    
			    String [] queryresult= new String[4];
			    queryresult[0]=stiriofbusnode;
			    queryresult[1]=styvalue;
			    queryresult[2]=stxvalue;
			    queryresult[3]=stPdvalue;
			    
			    totalnodebusresult.add(queryresult);
			    System.out.println("adding success");
		    }
		}
		
		createNewCSV(csvFile2,totalnodebusresult,"C:/JPS_DATA/workingdir/JPS_POWSYS/inputloadpoints.csv","p","-");
	}

}
