package uk.ac.cam.cares.jps.men;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.List;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.ResultSetFactory;
import com.hp.hpl.jena.query.ResultSetFormatter;
import com.hp.hpl.jena.query.ResultSetRewindable;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.men.entity.FeasibleConnection;
import uk.ac.cam.cares.jps.men.entity.INamed;
import uk.ac.cam.cares.jps.men.entity.Parameters;
import uk.ac.cam.cares.jps.men.entity.Product;
import uk.ac.cam.cares.jps.men.entity.Sink;
import uk.ac.cam.cares.jps.men.entity.Source;
import uk.ac.cam.cares.jps.men.entity.Transportation;

public class MenDataProvider {
	
	private List<Source> sources = new ArrayList<Source>();
	private List<Sink> sinks = new ArrayList<Sink>();
	private List<FeasibleConnection> feasibleConnections = new ArrayList<FeasibleConnection>();
	private List<Transportation> transportations = new ArrayList<Transportation>();
				
	public static void main(String[] args) throws Exception {
		
		MenDataProvider provider = new MenDataProvider();
		provider.startCalculation();
	}
	
	public void startCalculation() {
		
		getData();
		
		System.out.println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	    System.out.println("Number of sources = " + sources.size());
	    System.out.println("Number of sinks = " + sinks.size());
	    System.out.println("Number of connections = " + feasibleConnections.size());
	    System.out.println("Number of transp = " + transportations.size());
	    
	    System.out.println("sources = " + sources);
	    System.out.println("sinks = " + sinks);
	    System.out.println("connections = " + feasibleConnections);
	    System.out.println("transportations = " + transportations); 
	    System.out.println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		// TODO-AE remove owl file from resource dir
		String TaskFile = ".\\res\\MassExchangeNetworkIntegration.owl";   
 
	    
	    MenGamsConverter converter = new MenGamsConverter();
	    // TODO-AE URGENT switch to true for highest base price?
	    Parameters parameters = new Parameters(50., 1., 1., 1.05, false);
	    converter.calculate(sources, sinks, feasibleConnections, transportations, parameters);
	}
	
	private void getData() {
		
		String Jr_MEN_OKB = ".\\res\\Jr_MEN.owl";                  // location of the owl file that contains information for the mass exchange network
		String Transport_OKB = ".\\res\\Jr_Transportation_simplified.owl";                  // location of the owl file that contains information for the transportation system
		String OKB = ".\\res\\JurongChemicalPlant_trimmed.owl";                      // define file location of the ontological knowledge base 


		// based on the Jr_MEN OKB
		String nameOfResourceNetwork = "Jurong_MEN";  //define the name of the resource network that is to be synthesized 

		// Step_1 Identify Sources
		
		String IdSource = "PREFIX rns:<http://www.jparksimulator.com/ResourceNetworkSimplified.owl#> "
				+ "SELECT ?source ?IRI "
				+ "WHERE { ?entity a rns:ResourceNetwork ."
				+ "?entity rns:hasSources ?s_i ."
				+ "?s_i rns:hasName ?source ."
				+ "?s_i rns:hasIRI ?IRI ."
				+ "FILTER regex (str(?entity), '"+ nameOfResourceNetwork +"') ."
				+ "}"
				+ "ORDER BY ?sources DESC(?added)"
				;
		
		ResultSet rs_source = sparql(Jr_MEN_OKB, IdSource);  //query information (name and IRI) for the sources
		
		//go through the source set and extract source info				
		while(rs_source.hasNext()) {
			QuerySolution qs_s = rs_source.nextSolution();
		    Literal sourceLit = qs_s.getLiteral("source") ;     //extract the name of the source
		    String sourceName = sourceLit.getString();
		    Literal iri = qs_s.getLiteral("IRI") ;           //extract the IRI of the source
		    String sourceIRI = iri.getString();
		    
		    
		    //extract product information (product name, capacity, price) from the OKB
		    String sourceInfo = "PREFIX cp:<"+ sourceIRI.split("#")[0] +"#> "
		    		+ "SELECT ?product ?capacity ?price ?nearSea "
		    		+ "WHERE {?entity  a  cp:ChemicalPlant  ."
		    		+ "?entity   cp:nearSea ?nearSea ."
		    		+ "?entity   cp:produces ?ProductI ."
		    		+ "?ProductI cp:name ?product ."
		    		+ "?ProductI  cp:capacity ?capacity ."
		    		+ "?ProductI cp:price    ?price . "
		    		+ "FILTER regex (str(?entity), '"+ sourceIRI.split("#")[1] +"')"
		    		+ "}"
		    		+ "ORDER BY ?product DESC(?added)"
		    		;
		    
		    ResultSet rs_prod = sparql(OKB, sourceInfo);         //query product information (product name, capacity, price) for the sources
		    //go through the product set and extract capacity and price
		    for( ; rs_prod.hasNext(); ) {
		    	QuerySolution qs_p = rs_prod.nextSolution();
		    	Literal pro = qs_p.getLiteral("product") ;       //extract the name of the source
			    String productName = pro.getString();
		    	Literal cap = qs_p.getLiteral("capacity") ;      //extract the name of the source
			    String capacity = cap.getString();
			    Literal pri = qs_p.getLiteral("price") ;         //extract the IRI of the source
			    String price = pri.getString();
			    Literal nS = qs_p.getLiteral("nearSea") ;         //extract the IRI of the source
			    String nearSea = nS.getString();
			    
			    Product product = new Product(productName);
			    product.setCapacity(Double.valueOf(capacity));
			    product.setPrice(Double.valueOf(price));
			    Source source = new Source(sourceName, product);
			    // TODO-AE in OWL files, the value true and false are used 
			    source.setNearSea(Boolean.valueOf(nearSea));
			    
			    sources.add(source);
		    }

		}		
		
		System.out.println("Number of sources = " + sources.size());
		System.out.println("sources = " + sources);
		
		//Step_2 Identify Sinks
		String IdSink = "PREFIX rns:<http://www.jparksimulator.com/ResourceNetworkSimplified.owl#> "
				+ "SELECT ?sink ?IRI "
				+ "WHERE { ?entity a rns:ResourceNetwork ."
				+ "?entity rns:hasSinks ?s_i ."
				+ "?s_i rns:hasName ?sink ."
				+ "?s_i rns:hasIRI ?IRI ."
				+ "FILTER regex (str(?entity), '"+ nameOfResourceNetwork +"') ."
				+ "}"
				+ "ORDER BY ?sinks DESC(?added)"
				;
		
		ResultSet rs_sink = sparql(Jr_MEN_OKB, IdSink);  //query information (name and IRI) for the sinks
		//go through the source set and extract sink info				
		while(rs_sink.hasNext()) {
			
			
			
			QuerySolution qs_s = rs_sink.nextSolution();
		    Literal sinkLit = qs_s.getLiteral("sink") ;     //extract the name of the sink
		    String sinkName = sinkLit.getString();
		    Literal iri = qs_s.getLiteral("IRI") ;           //extract the IRI of the sink
		    String sinkIRI = iri.getString();
		    				    
		    System.out.println("sink IRI =" + sinkIRI);
		    
		    //extract product information (product name, capacity, price) from the OKB
		    String sinkInfo = "PREFIX cp:<"+ sinkIRI.split("#")[0] +"#> "
		    		+ "SELECT ?rawMaterial ?demand ?nearSea "
		    		+ "WHERE {?entity  a  cp:ChemicalPlant  ."
		    		+ "?entity   cp:nearSea ?nearSea ."
		    		+ "?entity cp:consumes ?RawMaterialI ."
		    		+ "?RawMaterialI  cp:name ?rawMaterial ."
		    		+ "?RawMaterialI  cp:demand ?demand ."
		    		+ "FILTER regex (str(?entity), '"+ sinkIRI.split("#")[1] +"')"
		    		+ "}"
		    		+ "ORDER BY ?rawMaterial DESC(?added)"
		    		;
		    
		    ResultSet rs_rm = sparql(OKB, sinkInfo);         //query raw material information (raw material name, demand) for the sources
		    //go through the product set and extract capacity and price
		    for( ; rs_rm.hasNext(); ) {
		    	QuerySolution qs_r = rs_rm.nextSolution();
		    	Literal rm = qs_r.getLiteral("rawMaterial") ;       //extract the name of the source
			    String rawMaterialName = rm.getString();
		    	Literal dem = qs_r.getLiteral("demand") ;      //extract the name of the source
			    String demand = dem.getString();
			    Literal nS = qs_r.getLiteral("nearSea") ;      //extract the name of the source
			    String nearSea = nS.getString();
			    
			    Product product = new Product(rawMaterialName);
			    product.setCapacity(Double.valueOf(demand));
			    Sink sink = new Sink(sinkName, product);
			    // TODO-AE in OWL files, the value true and false are used 
			    sink.setNearSea(Boolean.valueOf(nearSea));
			    
			    sinks.add(sink);
		    }				    
		}
		
		
		System.out.println("Number of sinks = " + sinks.size());
		System.out.println("sinks = " + sinks);
		
		//step 3 to connect the sink & the source with feasible transportation & distance 
		for (Sink sink : sinks) {
			for (Source source : sources) {
			    
			    if (INamed.equalNames(sink.getProduct(), source.getProduct())) {
			    	
			    	System.out.println("..................................................");
			    	System.out.println("next feabible connection: " + sink + " " + source);
			
					String dis = "PREFIX tp:<http://www.jparksimulator.com/transportation_simple.owl#> "
				    		+ "SELECT ?distance "
				    		+ "WHERE { ?entity a tp:TransportationRoute ."
				    		+ "?entity tp:startsFrom ?startP ."
				    		+ "?startP tp:hasName ?startPN ."
				    		+ "?entity tp:endsAt ?endP ."
				    		+ "?endP   tp:hasName ?endPN ."
				    		+ "?entity tp:hasLength ?distance ."
				    		+ "FILTER regex (str(?startPN), '"+ sink.getName() +"') ."
				    		+ "FILTER regex (str(?endPN), '"+ source.getName() +"') "
				    		+ "}"
				    		;
					
					ResultSet rs_dis = sparql(Transport_OKB, dis);
					QuerySolution qs_dis = rs_dis.nextSolution();
				    Literal distanceL = qs_dis.getLiteral("distance") ;     //extract the name of the sink
				    String distance = distanceL.getString();   

				    FeasibleConnection connection = new FeasibleConnection(source, sink);
				    connection.setDistance(Float.valueOf(distance));
				    
				    feasibleConnections.add(connection);
				}					
			}
		}
		
		
		
		//Step_4 extract the transportation means' information from transportation ontology
		
		//Land transportation
		String lt = "PREFIX tp:<http://www.jparksimulator.com/transportation_simple.owl#> "
				+ "SELECT ?lt ?Ctrans ?emission ?Cinst "
				+ "WHERE { ?entity a tp:LandTransportation ."
				+ "?entity tp:hasName ?lt ."
				+ "?entity tp:hasTransportationCost ?Ctrans ."
				+ "?entity tp:hasEmission ?emission ."
				+ "?entity tp:hasInstallationCost ?Cinst ."
				+ "}"
				;
		
		ResultSet rs_lt = sparql(Transport_OKB, lt);
		//go through the land transportation methods and extract the name, transportation cost, CO2 emission and installation cost
	    while(rs_lt.hasNext()) {
	    	QuerySolution qs_lt = rs_lt.nextSolution();
			Literal ltrans = qs_lt.getLiteral("lt") ;     //extract the transportation means' name
		    String ltransN = ltrans.getString();
		    Literal Ct = qs_lt.getLiteral("Ctrans") ;     //extract the transportation cost of the transportation means
		    String Ctrans = Ct.getString();  
		    Literal em = qs_lt.getLiteral("emission") ;     //extract the CO2 emission of the transportation means
		    String emission = em.getString();
		    Literal Cinst = qs_lt.getLiteral("Cinst") ;     //extract the installation cost of the transportation means
		    String Cinstallation = Cinst.getString();
		    
		    Transportation trans = new Transportation(ltransN);
		    trans.setTransportationCost(Double.valueOf(Ctrans));
		    trans.setEmission(Double.valueOf(emission));
		    trans.setInstallationCost(Double.valueOf(Cinstallation));
		    
		    transportations.add(trans);
	    }
	    
		

	    
	    //Short-sea transportation
		String sst = "PREFIX tp:<http://www.jparksimulator.com/transportation_simple.owl#> "
				+ "SELECT ?sst ?Ctrans ?emission ?Cinst "
				+ "WHERE { ?entity a tp:ShortSeaTransportation ."
				+ "?entity tp:hasName ?sst ."
				+ "?entity tp:hasTransportationCost ?Ctrans ."
				+ "?entity tp:hasEmission ?emission ."
				+ "?entity tp:hasInstallationCost ?Cinst ."
				+ "}"
				;
		
		ResultSet rs_sst = sparql(Transport_OKB, sst);
		//go through the short sea transportation methods and extract the name, transportation cost, CO2 emission and installation cost
	    while(rs_sst.hasNext()) {
	    	QuerySolution qs_sst = rs_sst.nextSolution();
			Literal ltrans = qs_sst.getLiteral("sst") ;     //extract the transportation means' name
		    String transName = ltrans.getString();  
		    Literal Ct = qs_sst.getLiteral("Ctrans") ;     //extract the transportation cost of the transportation means
		    String Ctrans = Ct.getString();   
		    Literal em = qs_sst.getLiteral("emission") ;     //extract the CO2 emission of the transportation means
		    String emission = em.getString(); 
		    Literal Cinst = qs_sst.getLiteral("Cinst") ;     //extract the installation cost of the transportation means
		    String Cinstallation = Cinst.getString();
		    
		    Transportation trans = new Transportation(transName);
		    trans.setTransportationCost(Double.valueOf(Ctrans));
		    trans.setEmission(Double.valueOf(emission));
		    trans.setInstallationCost(Double.valueOf(Cinstallation));
		    
		    transportations.add(trans);
	    }  
	}
	
	/**
	 * This method is to perform a given sparql query task on an ontological knowledge base (OKB) 
	 * @param Qstring
	 */
	public static ResultSet sparql (String fileLocat, String Qstring) {
	
		OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		try {
		    File file = new File(fileLocat);
		    FileInputStream reader = new FileInputStream(file);
		    model.read(reader,null);     //load the ontology model
		} catch (Exception e) {
		    e.printStackTrace();
		}

		Query query = QueryFactory.create(Qstring);
		QueryExecution qe = QueryExecutionFactory.create(query, model);
		ResultSet rs = qe.execSelect();                                    //the ResultSet can only be iterated once
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    //reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetFormatter.out(System.out, results, query);
				
		return results;
	}
}



