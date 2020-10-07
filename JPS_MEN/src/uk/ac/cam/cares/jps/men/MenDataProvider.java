package uk.ac.cam.cares.jps.men;

import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Literal;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.men.entity.FeasibleConnection;
import uk.ac.cam.cares.jps.men.entity.INamed;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;
import uk.ac.cam.cares.jps.men.entity.Product;
import uk.ac.cam.cares.jps.men.entity.Sink;
import uk.ac.cam.cares.jps.men.entity.Source;
import uk.ac.cam.cares.jps.men.entity.Transportation;

public class MenDataProvider {
	
	private List<Source> sources = new ArrayList<Source>();
	private List<Source> totalsources = new ArrayList<Source>();
	private List<Sink> totalsinks = new ArrayList<Sink>();
	private List<Sink> sinks = new ArrayList<Sink>();
	private List<FeasibleConnection> feasibleConnections = new ArrayList<FeasibleConnection>();
	private List<Transportation> transportations = new ArrayList<Transportation>();
	private Logger logger = LoggerFactory.getLogger(MenDataProvider.class);	


	public MenResult startCalculation(MenCalculationParameters parameters, String transportationIRI, List<String> chemicalPlantIRIs)  {
		
		getData(transportationIRI, chemicalPlantIRIs);

		logger.debug("Number of sources = " + totalsources.size());
		logger.debug("Number of sinks = " + totalsinks.size());
	    logger.debug("Number of connections = " + feasibleConnections.size());
	    logger.debug("Number of transp = " + transportations.size());
	    
	    logger.debug("sources = " + totalsources);
	    logger.debug("sinks = " + totalsinks);
	    logger.debug("connections = " + feasibleConnections);
	    logger.debug("transportations = " + transportations);  
	    
	    MenGamsConverter converter = new MenGamsConverter();
	    return converter.calculate(totalsources, totalsinks, feasibleConnections, transportations, parameters);
		 
	}
	private void getData(String Transport_OKB,List<String>plantkb)  {


		// based on the Jr_MEN OKB
		String nameOfResourceNetwork = "Jurong_MEN"; // define the name of the resource network that is to be
														// synthesized

		// Step_1 Take Data from OWL of the plant and sources-related property
		addSourceDataFromKB(plantkb, nameOfResourceNetwork);

		// Step_2 Take Data from OWL of the plant and sinks-related property
		addSinkDataFromKB(plantkb, nameOfResourceNetwork);

		// Step_3 Identify Sinks
		puresinkscategorial(sinks, sources);

		// Step_4 Identify Sources
		puresourcescategorial(sources, sinks);

		// step 5 to connect the sink & the source with feasible transportation &
		// distance
		connectSinkandSourceWithTransportation(totalsinks, totalsources);

		// Step_6 extract the transportation means' information from transportation
		// ontology
		getTransportationMeansInfoFromKB(Transport_OKB);

		
	}

	public List<Transportation> getTransportationMeansInfoFromKB(String Transport_OKB) {
		
		//Land transportation 1
		String lt = "PREFIX tp:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?lt ?Ctrans ?emission ?Cinst "
				+ "WHERE { ?entity a tp:Truck ."
				+ "?entity tp:hasName ?lt ."
				+ "?entity tp:hasEmission ?sstemission ."
				+ "?entity tp:hasInstallationCost ?sstinsta ."
				+ "?entity tp:hasTransportationCost ?ssttrans ."
				
				+ "?sstemission j2:hasValue ?valsstemission ."
				+ "?sstinsta j2:hasValue ?valsstinsta ."
				+ "?ssttrans j2:hasValue ?valssttrans ."
				
				+ "?valssttrans j2:numericalValue ?Ctrans ."
				+ "?valsstemission j2:numericalValue ?emission ."
				+ "?valsstinsta j2:numericalValue ?Cinst ."
				+ "}"
				;
		
		//Land transportation 2
		String lt2 = "PREFIX tp:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?lt ?Ctrans ?emission ?Cinst "
				+ "WHERE { ?entity a tp:LandPipelines ."
				+ "?entity tp:hasName ?lt ."
				+ "?entity tp:hasEmission ?sstemission ."
				+ "?entity tp:hasInstallationCost ?sstinsta ."
				+ "?entity tp:hasTransportationCost ?ssttrans ."
				
				+ "?sstemission j2:hasValue ?valsstemission ."
				+ "?sstinsta j2:hasValue ?valsstinsta ."
				+ "?ssttrans j2:hasValue ?valssttrans ."
				
				+ "?valssttrans j2:numericalValue ?Ctrans ."
				+ "?valsstemission j2:numericalValue ?emission ."
				+ "?valsstinsta j2:numericalValue ?Cinst ."
				+ "}"
				;
		
		ResultSet rs_lt = sparql(Transport_OKB, lt);
		//go through the land transportation methods and extract the name, transportation cost, CO2 emission and installation cost
	    while(rs_lt.hasNext()) {
	    	QuerySolution qs_lt = rs_lt.nextSolution();
			Literal ltrans = qs_lt.getLiteral("lt") ;     //extract the transportation means' name (lt= land transportation)
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
	    
		ResultSet rs_lt2 = sparql(Transport_OKB, lt2);
		//go through the land transportation methods and extract the name, transportation cost, CO2 emission and installation cost
	    while(rs_lt2.hasNext()) {
	    	QuerySolution qs_lt = rs_lt2.nextSolution();
			Literal ltrans = qs_lt.getLiteral("lt") ;     //extract the transportation means' name (lt= land transportation)
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
	    
	    //Short-sea transportation 1
		String sst = "PREFIX tp:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?sst ?Ctrans ?emission ?Cinst "
				+ "WHERE { ?entity a tp:Ship ."
				+ "?entity tp:hasName ?sst ."
				+ "?entity tp:hasEmission ?sstemission ."
				+ "?entity tp:hasInstallationCost ?sstinsta ."
				+ "?entity tp:hasTransportationCost ?ssttrans ."
				+ "?sstemission j2:hasValue ?valsstemission ."
				+ "?sstinsta j2:hasValue ?valsstinsta ."
				+ "?ssttrans j2:hasValue ?valssttrans ."
				+ "?valssttrans j2:numericalValue ?Ctrans ."
				+ "?valsstemission j2:numericalValue ?emission ."
				+ "?valsstinsta j2:numericalValue ?Cinst ."
				+ "}"
				;
		
		//Short-sea transportation 2
		String sst2 = "PREFIX tp:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?sst ?Ctrans ?emission ?Cinst "
				+ "WHERE { ?entity a tp:SeaPipelines ."
				+ "?entity tp:hasName ?sst ."
				+ "?entity tp:hasEmission ?sstemission ."
				+ "?entity tp:hasInstallationCost ?sstinsta ."
				+ "?entity tp:hasTransportationCost ?ssttrans ."
				+ "?sstemission j2:hasValue ?valsstemission ."
				+ "?sstinsta j2:hasValue ?valsstinsta ."
				+ "?ssttrans j2:hasValue ?valssttrans ."
				+ "?valssttrans j2:numericalValue ?Ctrans ."
				+ "?valsstemission j2:numericalValue ?emission ."
				+ "?valsstinsta j2:numericalValue ?Cinst ."
				+ "}"
				;
		
		ResultSet rs_sst = sparql(Transport_OKB, sst);
		//go through the short sea transportation methods and extract the name, transportation cost, CO2 emission and installation cost
	    while(rs_sst.hasNext()) {
	    	QuerySolution qs_sst = rs_sst.nextSolution();
			Literal ltrans = qs_sst.getLiteral("sst") ;     //extract the transportation means' name (short sea transportation)
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
	    
		ResultSet rs_sst2 = sparql(Transport_OKB, sst2);
		//go through the short sea transportation methods and extract the name, transportation cost, CO2 emission and installation cost
	    while(rs_sst2.hasNext()) {
	    	QuerySolution qs_sst = rs_sst2.nextSolution();
			Literal ltrans = qs_sst.getLiteral("sst") ;     //extract the transportation means' name (short sea transportation)
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
	    
	    logger.debug("Number of transportation means = " + transportations.size());
	    logger.debug("transportation means = " +  transportations);
		return  transportations;
	}

	public List<FeasibleConnection> connectSinkandSourceWithTransportation(List<Sink> sinks,List<Source> sources) {
		for (Sink sink : sinks) {
			for (Source source : sources) {
			    
			    if (INamed.equalNames(sink.getProduct(), source.getProduct())) {

				    
				    Double distance = distancecalcforUTMcoord(sink.getx(),sink.gety(),source.getx(),source.gety());
				  String distvalue = String.valueOf(distance);
				    FeasibleConnection connection = new FeasibleConnection(source, sink);
				   connection.setDistance(Float.valueOf(distvalue));
				    
				    feasibleConnections.add(connection);
				}					
			}
		}
		logger.info("Number of feasible Connections = " + feasibleConnections.size());
		logger.info("connection = " +  feasibleConnections);
		return  feasibleConnections;
	}

	public List<Sink> addSinkDataFromKB(List<String> plantkb, String nameOfResourceNetwork) {


		for (int file = 0; file < plantkb.size(); file++) {
			// extract rawmaterial information (rawmaterial name, demand) from the OKB
			String sinkInfo = "PREFIX cp:<" + "http://www.theworldavatar.com/ontology/ontoeip/chemicalplants/ChemicalPlant.owl"+ "#> " 
					+ "PREFIX cp2:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl" + "#> "
					+ "PREFIX cpname:<http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl" + "#> "
					+ "PREFIX j0:<" + "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl" + "#> "
					+ "PREFIX j3:<" + "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> " 
					+ "SELECT ?entity ?entityname  ?rawmaterial ?numvaldemand ?nearSea ?numvalx ?numvaly "
					+ "WHERE {?entity  a  cp:ChemicalPlant  ." 
					+ "?entity   cp2:nearSea ?nearSea ."
					+ "?entity   cpname:hasName ?entityname ." 
					+ "?entity   j0:consumes ?rawmatI ."
					+ "?rawmatI cpname:hasName ?rawmaterial ." 
					+ "?rawmatI  j0:hasDemand ?demand ."
					+ "?demand  j2:hasValue ?valuedemand ." 
					+ "?valuedemand  j2:numericalValue ?numvaldemand ."
					+ "?entity   cp2:hasGISCoordinateSystem ?coordinates ."
					+ "?coordinates   cp2:hasProjectedCoordinate_x ?x ."
					+ "?coordinates   cp2:hasProjectedCoordinate_y ?y ."
					+ "?x   j2:hasValue ?valuex ."
					+ "?valuex   j2:numericalValue ?numvalx ."
					+ "?y   j2:hasValue ?valuey ."
					+ "?valuey   j2:numericalValue ?numvaly ."
					// + "FILTER regex (str(?entity), '"+
					// plantkb.get(0).split("new\\\\")[file].split(".owl")[0] +"')"
					+ "}" + "ORDER BY ?product DESC(?added)";

			ResultSet rs_rm = sparql(plantkb.get(file), sinkInfo); // query raw material information (raw material name,
																	// demand) for the sources
			// go through the product set and extract capacity and price
			for (; rs_rm.hasNext();) {
				QuerySolution qs_r = rs_rm.nextSolution();

				Literal compname = qs_r.getLiteral("entityname");
				String sinkName = compname.getString();
				Literal rm = qs_r.getLiteral("rawmaterial"); // extract the name of the sink
				String rawMaterialName = rm.getString();
				Literal dem = qs_r.getLiteral("numvaldemand"); // extract the name of the sink
				String demand = dem.getString();
				Literal nS = qs_r.getLiteral("nearSea"); // extract the name of the sink
				String nearSea = nS.getString();
				Literal xcoordinate = qs_r.getLiteral("numvalx");
				String xvalue = xcoordinate.getString();
				Literal ycoordinate = qs_r.getLiteral("numvaly");
				String yvalue = ycoordinate.getString();


				Product product = new Product(rawMaterialName);
				product.setCapacity(Double.valueOf(demand));
				Sink sink = new Sink(sinkName, product);
				sink.setNearSea(Boolean.valueOf(nearSea));
				sink.setx(Double.valueOf(xvalue));
				sink.sety(Double.valueOf(yvalue));
				sinks.add(sink);
			}
		}

		// logger.info("Number of sinks = " + sinks.size());
		// logger.info("sinks = " + sinks);
		return sinks;
	}


	public List<Source> addSourceDataFromKB(List<String> plantkb, String nameOfResourceNetwork) {


		for (int file = 0; file < plantkb.size(); file++) {

			String sourceInfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/chemicalplants/ChemicalPlant.owl#> " 
					+ "PREFIX cp2:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX cpname:<" + "http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#> "
					+ "PREFIX j0:<" + "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#> "
					+ "PREFIX j3:<" + "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> " 
					+ "SELECT ?entity ?entityname  ?product ?numvalcapacity ?numvalprice ?nearSea ?numvalx ?numvaly "
					+ "WHERE {?entity  a  cp:ChemicalPlant  ." 
					+ "?entity   cp2:nearSea ?nearSea ."
					+ "?entity   cpname:hasName ?entityname ." 
					+ "?entity   j0:produces ?ProductI ."
					+ "?ProductI cpname:hasName ?product ." 
					+ "?ProductI  j0:hasCapacity ?capacity ."
					+ "?capacity  j2:hasValue ?valuecapacity ." 
					+ "?valuecapacity  j2:numericalValue ?numvalcapacity ."
					+ "?ProductI j3:hasPrice    ?price . " 
					+ "?price  j2:hasValue ?valueprice ."
					+ "?valueprice  j2:numericalValue ?numvalprice ."
					+ "?entity   cp2:hasGISCoordinateSystem ?coordinates ."
					+ "?coordinates   cp2:hasProjectedCoordinate_x ?x ."
					+ "?coordinates   cp2:hasProjectedCoordinate_y ?y ."
					+ "?x   j2:hasValue ?valuex ."
					+ "?valuex   j2:numericalValue ?numvalx ."
					+ "?y   j2:hasValue ?valuey ."
					+ "?valuey   j2:numericalValue ?numvaly ."
					+ "}" + "ORDER BY ?product DESC(?added)";

			
			// only for debug:;
			//System.out.println("name urgent="+plantkb.get(file));
			ResultSet rs_prod = sparql(plantkb.get(file), sourceInfo); // query product information (product name,
																		// capacity, price) for the sources
			//logger.info("filelocat= "+plantkb.get(file));
			// go through the product set and extract capacity and price
			for (; rs_prod.hasNext();) {
				QuerySolution qs_p = rs_prod.nextSolution();

				Literal compname = qs_p.getLiteral("entityname");
				String sourceName = compname.getString();

				Literal pro = qs_p.getLiteral("product"); // extract the name of the source
				String productName = pro.getString();
				Literal cap = qs_p.getLiteral("numvalcapacity"); // extract the name of the source
				String capacity = cap.getString();
				Literal pri = qs_p.getLiteral("numvalprice"); // extract the IRI of the source
				String price = pri.getString();
				Literal nS = qs_p.getLiteral("nearSea"); // extract the IRI of the source
				String nearSea = nS.getString();
				Literal xcoordinate = qs_p.getLiteral("numvalx");
				String xvalue = xcoordinate.getString();
				Literal ycoordinate = qs_p.getLiteral("numvaly");
				String yvalue = ycoordinate.getString();

				Product product = new Product(productName);
				product.setCapacity(Double.valueOf(capacity));
				product.setPrice(Double.valueOf(price));
				Source source = new Source(sourceName, product);
				source.setNearSea(Boolean.valueOf(nearSea));
				source.setx(Double.valueOf(xvalue));
				source.sety(Double.valueOf(yvalue));
				sources.add(source);
			}
		}
		// }

		// logger.info("Number of sources = " + sources.size());
		 //logger.info("sources = " + sources);
		return sources;
	}
	
	public List<Source> puresourcescategorial(List<Source> sources, List<Sink> sinks) {
		int numberloop1 = sources.size();
		int numberloop2 = sinks.size();

		for (int s = 0; s < numberloop1; s++) {

			String sourcesproduct = sources.get(s).getProduct().getName();
			for (int d = 0; d < numberloop2; d++) {
				String sinksrawmat = sinks.get(d).getProduct().getName();
				if (sourcesproduct.equals(sinksrawmat)) {
					if (totalsources.contains(sources.get(s)) == false) {
						totalsources.add(sources.get(s));
					}
				}
			}
		}
		return totalsources;
	}
	
	public List<Sink> puresinkscategorial(List<Sink> sinks, List<Source> sources) {
		int numberloop1 = sinks.size();

		int numberloop2 = sources.size();

		for (int s = 0; s < numberloop1; s++) {
			String sinksrawmat = sinks.get(s).getProduct().getName();
			for (int d = 0; d < numberloop2; d++) {
				String sourcesproduct = sources.get(d).getProduct().getName();
				if (sourcesproduct.equals(sinksrawmat)) {
					if (totalsinks.contains(sinks.get(s)) == false) {
						totalsinks.add(sinks.get(s));
					}
				}
			}			
		}
		
		return totalsinks;
	}
	
	
	
	/**
	 * This method is to perform a given sparql query task on an ontological knowledge base (OKB) 
	 * @param Qstring
	 */
	public static ResultSet sparql (String fileLocat, String sparql) {
		OntModel model = JenaHelper.createModel(fileLocat); 		
		return JenaHelper.query(model,sparql);
	}


	public double distancecalcforWGS84coord(Double lon1, Double lat1, Double lon2, Double lat2) {
		double a = 6378137, b = 6356752.314245, f = 1 / 298.257223563;
		double L = Math.toRadians(lon2 - lon1);
		double U1 = Math.atan((1 - f) * Math.tan(Math.toRadians(lat1)));
		double U2 = Math.atan((1 - f) * Math.tan(Math.toRadians(lat2)));
		double sinU1 = Math.sin(U1), cosU1 = Math.cos(U1);
		double sinU2 = Math.sin(U2), cosU2 = Math.cos(U2);
		double cosSqAlpha;
		double sinSigma;
		double cos2SigmaM;
		double cosSigma;
		double sigma;

		double lambda = L, lambdaP, iterLimit = 100;
		do {
			double sinLambda = Math.sin(lambda), cosLambda = Math.cos(lambda);
			sinSigma = Math.sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda)
					+ (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) * (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda));
			if (sinSigma == 0) {
				return 0;
			}

			cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cosLambda;
			sigma = Math.atan2(sinSigma, cosSigma);
			double sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma;
			cosSqAlpha = 1 - sinAlpha * sinAlpha;
			cos2SigmaM = cosSigma - 2 * sinU1 * sinU2 / cosSqAlpha;

			double C = f / 16 * cosSqAlpha * (4 + f * (4 - 3 * cosSqAlpha));
			lambdaP = lambda;
			lambda = L + (1 - C) * f * sinAlpha
					* (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)));

		} while (Math.abs(lambda - lambdaP) > 1e-12 && --iterLimit > 0);

		if (iterLimit == 0) {
			return 0;
		}

		double uSq = cosSqAlpha * (a * a - b * b) / (b * b);
		double A = 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)));
		double B = uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)));
		double deltaSigma = B * sinSigma * (cos2SigmaM + B / 4 * (cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)
				- B / 6 * cos2SigmaM * (-3 + 4 * sinSigma * sinSigma) * (-3 + 4 * cos2SigmaM * cos2SigmaM)));

		double s = b * A * (sigma - deltaSigma)/1000; //in km unit
		
		return s;

	}

	
	public double distancecalcforUTMcoord (Double x1,Double y1,Double x2,Double y2)
	{
		double distance= Math.sqrt(Math.pow(Math.abs(x2-x1),2)+Math.pow(Math.abs(y2-y1),2))/1000; //in km
		return distance;
	}
}



