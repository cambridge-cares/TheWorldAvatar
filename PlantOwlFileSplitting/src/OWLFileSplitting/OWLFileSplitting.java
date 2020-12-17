package OWLFileSplitting;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;





public class OWLFileSplitting {
	
	public static String baseURL2 = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/powerplants/";
	public static String baseURL = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/temporary/";
	String plantname=null;
	
	static Individual naturalgas;
	static Individual coal;
	static Individual oil;
	static Individual bituminous;
	static Individual subbituminous;
	static Individual lignite;
	static Individual anthracite;
	static Individual coalbiomass;
	static Individual Cogeneration;
	static Individual CombinedCycleGasTurbine;
	static Individual GasEngine;
	static Individual OpenCycleGasTurbine;
	static Individual SubCriticalThermal;
	static Individual SuperCriticalThermal;
	static Individual UltraSuperCriticalThermal;
	static Individual tonperyear;
	static Individual m;
	static Individual usdperkw;
	static Individual MW;
	static Individual mwh;
	static Individual length;
	static Individual xaxis;
	static Individual yaxis;
		
	private OntClass powerplantclass = null;
	private OntClass organizationclass = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass valueclass = null;
	private OntClass scalarvalueclass = null;
	
	
	private OntClass ageclass = null;
	private OntClass designcapacityclass = null;
	private OntClass emissionclass = null;
	private OntClass generationcostsclass = null;
	private OntClass annualgenerationclass = null;

	private ObjectProperty hasdimension = null;
	private ObjectProperty referto = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hasaddress = null;
	private ObjectProperty isownedby = null;
	private ObjectProperty designcapacity = null;
	private ObjectProperty hasyearofbuilt = null;
	private ObjectProperty realizes = null;
	
	private ObjectProperty consumesprimaryfuel = null;
	private ObjectProperty hasemission = null;
	private ObjectProperty hascosts = null;
	private ObjectProperty hasannualgeneration = null;
	private ObjectProperty usesgenerationtechnology = null;

	private DatatypeProperty numval = null;
	private DatatypeProperty hasname = null;
	
	ArrayList <String> datalist = new ArrayList <String>();
	
	private String 		sparqlstring = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> " 
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
			+ "SELECT DISTINCT ?entity ?vemission ?vcapa ?generation "
			+ "WHERE {?entity   j3:realizes ?process ."
			+ "?process   a cp:TemperatureChange  ."
			
			
			+ "?emission   j2:hasValue ?valueemission ."
			+ "?valueemission   j2:numericalValue ?vemission ."
			+ "?entity   j4:designCapacity ?capa  ."
			+ "?capa   j2:hasValue ?valuecapa ."
			+ "?valuecapa   j2:numericalValue ?vcapa ." 
			+ "}";
	
	
	public static synchronized ResultSet query(String sparql, OntModel model) {
		Query query = QueryFactory.create(sparql);
		QueryExecution queryExec = QueryExecutionFactory.create(query, model);
		ResultSet rs = queryExec.execSelect();   
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		ResultSetFormatter.out(System.out, results, query);
		return results;
	}
	
	public ArrayList <String> queryPowerplantProperty(OntModel jenaOwlModel,String sparqlstring) {
		String instance = null;
		String value1 = null;
		String capacity = null;
		String gen = null;

		
		ResultSet rs_plant = OWLFileSplitting.query(sparqlstring,jenaOwlModel); 
		
		for (; rs_plant.hasNext();) {			
			QuerySolution qs_p = rs_plant.nextSolution();

			Resource cpiri = qs_p.getResource("entity");
			instance = cpiri.toString();
			datalist.add(instance);
			Resource geniri = qs_p.getResource("generation");
			gen = geniri.toString();
			datalist.add(gen);
			Literal co = qs_p.getLiteral("vemission"); // extract the name of the source
			value1 = co.getString();
			datalist.add(value1);
			Literal cap = qs_p.getLiteral("vcapa"); // extract the name of the source
			capacity = cap.getString();
			datalist.add(capacity);
			System.out.println ("co2emission= "+value1);
			System.out.println ("capacity= "+capacity);			
		}
		

		
		return datalist;
	}
	
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		powerplantclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant");
		organizationclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#Organization");
		coordinateclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
		coordinatesystemclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		valueclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		scalarvalueclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");		
		ageclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#YearOfBuilt");
		designcapacityclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#DesignCapacity");
		emissionclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#CO2_emission");
		generationcostsclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#GenerationCosts");
		annualgenerationclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#AnnualGeneration");

		hasdimension = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDimension");
		referto = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#refersToAxis");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		hasaddress = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasAddress");
		isownedby = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#isOwnedBy");
		realizes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#realizes");
		designcapacity = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#designCapacity");
		hasyearofbuilt = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#hasYearOfBuilt");
		consumesprimaryfuel = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#consumesPrimaryFuel");
		hasemission = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission");
		hascosts = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasCosts");
		hasannualgeneration = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#hasAnnualGeneration");
		usesgenerationtechnology = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#usesGenerationTechnology");
			
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		hasname = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#hasName");
		
		naturalgas = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas");
		coal = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal");
		oil = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil");
		anthracite = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Anthracite");
		bituminous = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Bituminous");
		lignite = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Lignite");
		subbituminous = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Subbituminous");
		coalbiomass = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CoalBiomass");
		
		Cogeneration = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Cogeneration");
		CombinedCycleGasTurbine = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine");
		OpenCycleGasTurbine = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OpenCycleGasTurbine");
		GasEngine = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#GasEngine");
		SubCriticalThermal = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SubCriticalThermal");
		SuperCriticalThermal = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SuperCriticalThermal");
		UltraSuperCriticalThermal = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#UltraSuperCriticalThermal");
		tonperyear=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_yr");
		m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		usdperkw=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#USD_per_kW");
		MW=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");
		mwh=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MWh");
		length=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#length");
		xaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#x-axis");
		yaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#y-axis");
	}
	
	public void doConversion(OntModel jenaOwlModel, String plantname ,String countryname,String ownername,String fueltype,String tech,String xnumval,String ynumval,String emissionnumval,String costnumval,String anngennumval,String capanumval,String agenumval){
		Individual genprocess=null;
		Individual fuel=null;
		Individual technology=null;

		Individual plant = powerplantclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#"+plantname);
		Individual age = ageclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#YearOfBuilt_of_"+plantname);
		Individual capa = designcapacityclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#capa_of_"+plantname);
		Individual plantcoordinate = coordinatesystemclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#CoordinateSystem_of_"+plantname);
		Individual owner = organizationclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#Owner_of_"+plantname);
		//Individual country = jenaOwlModel.getIndividual("http://dbpedia.org/resource/" + countryname);
		
		
		
		Resource countryResource = jenaOwlModel.createResource("http://dbpedia.org/resource/" + countryname);
//		Individual ontologyInd = jenaOwlModel.getIndividual("http://www.semanticweb.org/kevin/ontologies/2018/8/untitled-ontology-1852");
//		Property imports = jenaOwlModel.getProperty("http://www.w3.org/2002/07/owl#imports");
//		ontologyInd.addProperty(imports, countryResource);
//		System.out.println("MY ont=" + ontologyInd + " imports=" + imports + " country=" + countryResource);
//		

		
		
		
		
		
		Individual emission = emissionclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#CO2Emission_of_"+plantname);
		Individual cost = generationcostsclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#GenerationCost_of_"+plantname);
		Individual anngen = annualgenerationclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#AnnualGeneration_of_"+plantname);
		Individual xcoordinate = coordinateclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#x_coordinate_of_"+plantname);
		Individual ycoordinate = coordinateclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#y_coordinate_of_"+plantname);
		Individual xcoordinatevalue = valueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_x_coordinate_of_"+plantname);
		Individual ycoordinatevalue = valueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_y_coordinate_of_"+plantname);
		Individual capavalue = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_capa_of_"+plantname);
		Individual agevalue = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_YearOfBuilt_of_"+plantname);
		Individual emissionvalue = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_CO2Emission_of_"+plantname);
		Individual costvalue = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_GenerationCost_of_"+plantname);
		Individual anngenvalue = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_AnnualGeneration_of_"+plantname);
	}
	
	public void startConversion() throws Exception {

		 String csvFile = "D:/JParkSimulator-git-dev-database/PlantOwl/new 2.csv";
	        String line = "";
	        String cvsSplitBy = ",";

	        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

	            while ((line = br.readLine()) != null) {

	                // use comma as separator
	                
	            	String[] data = line.split(cvsSplitBy);
	            	
	            	

	                //System.out.println("Country [code= " + country[4] + " , name=" + country[5] + "]");
	            	
	            	String filePath = baseURL + "planttemplatekb2.owl"; // the empty owl file
	            	
	            	//revise this condition again later
	            	if (data.length<12)
	    			{
	    				plantname="PowerPlant_"+data[0]+"_"+UUID.randomUUID();
	    				
	    				String filePath2 = baseURL2 + plantname+".owl"; // the result of written owl file
	    				
	    				FileInputStream inFile = new FileInputStream(filePath);
		    			Reader in = new InputStreamReader(inFile, "UTF-8");
		    				    			
		    			OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		    			jenaOwlModel.read(in, null);

		    			initOWLClasses(jenaOwlModel);
		    			
	    				doConversion(jenaOwlModel, plantname,data[0],"unknown",data[2],data[3],"0","0","0","0",data[6],data[1],data[4]); //plant,country,owner,fuel,tech,x,y,emission,cost,anngen,capa,age	
	    				
	    				/** save the updated model file */
		    			savefile(jenaOwlModel, filePath2);
	    			}
	            	
	            	else
	            	{	                
	    			/** load your knowledge base from an owl file; additionally */
	            		
	            		
	    			
	    			
	    			
	    			//replace the name from all the strange character
	    			data[8] = data[8].replaceAll("\\(","");
	    			data[8] = data[8].replaceAll("\\)","");
	    			data[8] = data[8].replaceAll("&apos;", "");
	    			data[8] = data[8].replaceAll("'", "");
	    			data[8] = data[8].replaceAll("&amp;", "&");
	    			data[8] = data[8].replaceAll("©", "");
	    			data[8] = data[8].replaceAll("Ã-", "A");
	    			data[8] = data[8].replaceAll("Ã", "A");
	    			data[8] = data[8].replaceAll("¶", "");
	    			data[8] = data[8].replaceAll("¼", "");									

	    			//System.out.println("data8= "+data[8]);
	    			
	    			String filePath2 = baseURL2 + data[8]+".owl"; // the result of written owl file
	    			
	    			
	    			//System.out.println(filePath2);
	    			FileInputStream inFile = new FileInputStream(filePath);
	    			Reader in = new InputStreamReader(inFile, "UTF-8");
	    				    			
	    			OntModel jenaOwlModel = ModelFactory.createOntologyModel();
	    			jenaOwlModel.read(in, null);

	    			initOWLClasses(jenaOwlModel);
	    			
	    			

	    			doConversion(jenaOwlModel, data[8],data[0],data[11],data[2],data[3],data[10],data[9],"0","0",data[6],data[1],data[4]); //plant,country,owner,fuel,tech,x,y,emission,cost,anngen,capa,age
	            	

	    			/** save the updated model file */
	    			savefile(jenaOwlModel, filePath2);
	            	}

	            }

	        } catch (IOException e) {
	            e.printStackTrace();
	        }
	
	}
	
	public void comparewordincsv(String csvfile) {
		
        String line = "";
        String cvsSplitBy = ",";
        Charset charset = StandardCharsets.UTF_8;
      //  String content = new String(Files.readAllBytes(path), charset);
    	try (BufferedReader br = new BufferedReader(new FileReader(csvfile))) {
int a=1;
            while ((line = br.readLine()) != null) {

                // use comma as separator
                String[] iri = line.split(cvsSplitBy);
                
                if (!iri[0].equals("United_States")&&!iri[0].equals("United_Kingdom")){
                	if(!iri[8].contains(iri[0]))
                	{
                
                	System.out.println(" [line= "+a+" name= " + iri[8] + " , country=" + iri[0] + "]");	
                
                	}
                }
                
                
               // content = content.replaceAll("<j.1:numericalValue>", "<j.1:numericalValue rdf:datatype=\"http://www.w3.org/2001/XMLSchema#double\">");
                
                //Files.write(path2, content.getBytes(charset));	
		    	a++;
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    	
	}
	
	
	
	
	public static void main(String[] args) throws Exception {
		System.out.println("Starting Process");
		String csvFile = "D:/JParkSimulator-git-dev-database/PlantOwl/modifieddata.csv";
		
		OWLFileSplitting converter = new OWLFileSplitting();
		converter.comparewordincsv(csvFile);

	}
}
