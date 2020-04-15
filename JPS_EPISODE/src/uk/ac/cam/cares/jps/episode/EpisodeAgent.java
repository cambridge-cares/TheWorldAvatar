package uk.ac.cam.cares.jps.episode;

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

public class EpisodeAgent extends DispersionModellingAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final String separator="\t";
	
	//later to be moved to config
	double dx_rec=100; //TODO hardcoded? decide the dx for the receptor
	double dy_rec=100;//TODO hardcoded? decide the dy for the receptor
	double dx=1000.0 ;//decide the dx for the scope
	double dy=1000.0 ;//decide the dy for the scope	
	double dz=10;
	double nz=13;
	double upperheight=75.0;
	double lowerheight=2.0;
	
    private static final String DATA_KEY_COLLECTION = "collection";
    private static final String DATA_KEY_ITEMS = "items";
    private static final String DATA_KEY_LAT = "lat";
    private static final String DATA_KEY_LON = "lon";
    private static final String DATA_KEY_MMSI = "mmsi";
    private static final String DATA_KEY_SS = "ss";
    private static final String DATA_KEY_CU = "cu";
    
    String chimneyiriInfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
            + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#> "
            + "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
            + "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
            + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#> "
            + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
            + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#> "
            + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
            + "SELECT ?heightchimneyval ?diameterchimneyval ?massfval ?tempval ?densval ?vpolflowrate ?polflowrateval "
            + "WHERE {?entity  a  j3:Pipe  ."
            + "?entity   j3:hasHeight ?heightchimney ."
            + "?heightchimney  j2:hasValue ?vheightchimney ."
            + "?vheightchimney  j2:numericalValue ?heightchimneyval ."
            + "?entity   j3:hasInsideDiameter ?diameterchimney ."
            + "?diameterchimney  j2:hasValue ?vdiameterchimney ."
            + "?vdiameterchimney  j2:numericalValue ?diameterchimneyval ."
            + "?entity   j4:realizes ?proc ."
            + "?proc j5:hasOutput ?waste ."
            + "?waste j6:refersToGeneralizedAmount ?genwaste ."
            + "?genwaste   j2:hasProperty ?massf ."
            + "?massf   j2:hasValue ?vmassf ."
            + "?vmassf   j2:numericalValue ?massfval ."
            + "?genwaste   j2:hasSubsystem ?matamount ."
            + "?matamount   j7:refersToMaterial ?mat ."
            + "?mat   j8:thermodynamicBehavior ?thermo ."
            + "?thermo   j9:has_temperature ?temp ."
            + "?temp  j2:hasValue ?vtemp ."
            + "?vtemp  j2:numericalValue ?tempval ."
            + "?thermo   j9:has_density ?dens ."
            + "?dens  j2:hasValue ?vdens ."
            + "?vdens  j2:numericalValue ?densval ."
            + "?mat   j8:intrinsicCharacteristics ?char ."
            + "?char j2:hasProperty ?polflowrate ."
            + "?polflowrate j2:hasValue ?vpolflowrate ."
            + "?vpolflowrate j2:numericalValue ?polflowrateval ."
            + "}";
    
    String chimneyiriparticleInfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
            + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#> "
            + "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
            + "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
            + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#> "
            + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
            +"PREFIX j10:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#>"
            + "SELECT ?overallflowrateval ?particulatefractionval ?particulatediameterval "
            + "WHERE {?entity  a  j3:Pipe  ."
            + "?entity   j3:hasHeight ?heightchimney ."
            + "?heightchimney  j2:hasValue ?vheightchimney ."
            + "?vheightchimney  j2:numericalValue ?heightchimneyval ."
            + "?entity   j3:hasInsideDiameter ?diameterchimney ."
            + "?diameterchimney  j2:hasValue ?vdiameterchimney ."
            + "?vdiameterchimney  j2:numericalValue ?diameterchimneyval ."
            + "?entity   j4:realizes ?proc ."
            + "?proc j5:hasOutput ?waste ."
            + "?waste j6:refersToGeneralizedAmount ?genwaste ."
            + "?genwaste   j2:contains ?particle ." + 
              " ?particle j2:hasProperty ?overallflowrate ." + 
            " ?overallflowrate j2:hasValue ?voverallflowrate ." + 
            "?voverallflowrate j2:numericalValue ?overallflowrateval ." + 
            "?particle j7:hasRepresentativeParticle ?particulate ." + 
            "?particulate j2:hasProperty ?particulatefraction ." + 
            "?particulatefraction j2:hasValue ?vparticulatefraction ." + 
            "?vparticulatefraction j2:numericalValue ?particulatefractionval ." + 
            "?particulate j10:has_length ?particulatediameter ." + 
            "?particulatediameter j2:hasValue ?vparticulatediameter ." + 
            "?vparticulatediameter j2:numericalValue ?particulatediameterval ."
            
            + "}";
			
    private JSONArray getEntityData(JSONObject input) {
        JSONArray coordinates = new JSONArray();

        if (input.has(DATA_KEY_COLLECTION)) {
            JSONObject entities = input.getJSONObject(DATA_KEY_COLLECTION);
            if (entities.has(DATA_KEY_ITEMS)) {
                JSONArray items = entities.getJSONArray(DATA_KEY_ITEMS);
                for (Iterator<Object> i = items.iterator(); i.hasNext();) {
                    JSONObject item = (JSONObject) i.next();
                    if (item.has(DATA_KEY_LAT) & item.has(DATA_KEY_LON)) {
                        JSONObject latlon = new JSONObject();
                        latlon.put(DATA_KEY_LAT, item.getDouble(DATA_KEY_LAT));
                        latlon.put(DATA_KEY_LON, item.getDouble(DATA_KEY_LON));
                        latlon.put(DATA_KEY_MMSI, item.get(DATA_KEY_MMSI));
                        latlon.put("speed", item.getDouble(DATA_KEY_SS));
                        latlon.put("angle", item.getDouble(DATA_KEY_CU));
                        coordinates.put(latlon);
                    }
                }
            }
        }

        return coordinates;
    }
    
    private double unitflowconverter(double numberInGramPerS) {
    	double result=numberInGramPerS*0.001*365*24*3600;
    	return result;
    }
	@Override
	public void createWeatherInput(String dataPath, String filename,List<String>stniri) {	
		
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
				+ "PREFIX j6:<http://www.w3.org/2006/time#>" 
				+ "SELECT ?entity ?class ?propval ?proptimeval ?graph "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{ GRAPH ?graph "
				+ "{ "
				 
				+ "  ?entity j4:observes ?prop ." 
				+ " ?prop a ?class ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY DESC(?proptimeval)LIMIT 9";
//		String dataseturl="";//which is the weather stn dataset
//		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, sensorinfo);
//		 String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
//		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		RepositoryConnection con = WeatherAgent.repo.getConnection();
		 List<String[]> listmap = new ArrayList<String[]>();
			TupleQuery query = con.prepareTupleQuery(QueryLanguage.SPARQL, sensorinfo);
			TupleQueryResult result = query.evaluate();
			int d = 0;
			try {
				while (result.hasNext()) {
					BindingSet bindingSet = result.next();
					//String iri = bindingSet.getValue("graph").stringValue();
					String classprop = bindingSet.getValue("class").stringValue();
					String propval = bindingSet.getValue("propval").stringValue();
					String dateval = bindingSet.getValue("proptimeval").stringValue();
					String graphval = bindingSet.getValue("graph").stringValue();
					String[] content = { classprop,propval,dateval,graphval};
					listmap.add(content);

					d++;
				}
				System.out.println("total data=" + d);

			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		 System.out.println("size="+listmap.size());
		 
		 List<String[]> resultquery = new ArrayList<String[]>();
			
			
			
			
	        String[]header= {"*","yyyy","mm","dd","hh","FF1","DD1","T25m","DT","RH%","PP_mm","Cloud","Press","FF2","DD2"};
	        resultquery.add(0,header);
	        String time=listmap.get(0)[2];
	        String[]content=new String[15];
	        content[0]="";
	        content[1]=time.split("-")[0];
	        content[2]=time.split("-")[1];
	        content[3]=time.split("-")[2].split("T")[0];
	        content[4]=time.split("-")[2].split("T")[1].split(":")[0];
	        for(int r=0;r<listmap.size();r++) {
//	        	System.out.println(listmap.get(r)[0]);
//	        	System.out.println(listmap.get(r)[3]);
	        	if(listmap.get(r)[3].toLowerCase().contains(stniri.get(1))) {
	        		System.out.println("it goes number 2");
	        		if(listmap.get(r)[0].toLowerCase().contains("speed"))
	        		content[13]=listmap.get(r)[1];
	        		else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
	        			content[14]=listmap.get(r)[1];
	        		}
	        	}else if(listmap.get(r)[3].toLowerCase().contains(stniri.get(0))) {
	        		System.out.println("it goes number 1");
	        		if(listmap.get(r)[0].toLowerCase().contains("speed")) {
		        		content[5]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
		        		content[6]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("temperature")) {
		        		content[7]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("humidity")) {
		        		String decimalhumidity=listmap.get(r)[1];
		        		double percent=Double.valueOf(decimalhumidity)*100;
		        		content[9]=""+percent;
		        	}else if(listmap.get(r)[0].toLowerCase().contains("precipitation")) {
		        		content[10]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("cloud")) {
		        		content[11]=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("pressure")) {
		        		content[12]=listmap.get(r)[1];
		        	}
	        	}
	        }
	        content[8]="0.022"; //currently hardcoded but can be substituted by model
	        resultquery.add(content);
	        StringBuilder sb= new StringBuilder();
	       //convert to tsv
	        for(int v=0;v<resultquery.size();v++) {
		        for (int c=0;c<resultquery.get(0).length;c++) {
		        	sb.append(resultquery.get(v)[c]);
		        	if(resultquery.get(0).length-c!=1) {
		        		sb.append(separator);
		        	}
		        	else {
		        		sb.append("\n");
		        	}
		        }
	        	
	        }
	        new QueryBroker().putLocal(dataPath + "/"+filename, sb.toString());  	
	}

	@Override
	public void createEmissionInput(String dataPath, String filename,JSONObject shipdata) {
		JSONArray coordinateship=getEntityData(shipdata);
	    OntModel jenaOwlModel = JenaHelper.createModel();
	    
		int shipAmount=coordinateship.length();
		List<String[]> resultquery = new ArrayList<String[]>();
		 String[]header= {"snap","xcor","ycor","Hi","Vi","Ti","radi","BH","BW","Pvec","Pdir","pcir_ang","Ptstart","Ptend","NOx","NMVOC","CO","SO2","NH3","PM2.5","PM10"};
		 resultquery.add(0,header);
		for (int ship = 0; ship < shipAmount; ship++) {
			String mmsi = coordinateship.getJSONObject(ship).get(DATA_KEY_MMSI).toString();
			String iriofchimney = "http://www.theworldavatar.com/kb/ships/" + mmsi + "/" + "Chimney-1.owl" + "#"
					+ "Chimney-1";
			
			jenaOwlModel.read(iriofchimney);

			List<String[]> resultListChimneyGasPollutant = queryIRI(chimneyiriInfo, jenaOwlModel);

			JSONObject mappollutant = new JSONObject();

			for (int d = 0; d < resultListChimneyGasPollutant.size(); d++) {
				if (resultListChimneyGasPollutant.get(d)[5].contains("EmissionRate")) {
					String key = resultListChimneyGasPollutant.get(d)[5].split("#V_")[1];
					mappollutant.put(key, resultListChimneyGasPollutant.get(d)[6]);
				}
			}

			List<String[]> resultListParticlePollutant = queryIRI(chimneyiriparticleInfo, jenaOwlModel);
			double fractionpm25 = 0.0;
			double fractionpm10 = 0.0;
			for (int x = 0; x < resultListParticlePollutant.size(); x++) {
				if (Double.valueOf(resultListParticlePollutant.get(x)[2]) <= 0.0000025) {
					fractionpm25 = fractionpm25 + Double.valueOf(resultListParticlePollutant.get(x)[1]);
				} else if (Double.valueOf(resultListParticlePollutant.get(x)[2]) <= 0.00001) {
					fractionpm10 = fractionpm10 + Double.valueOf(resultListParticlePollutant.get(x)[1]);
				}
			}
			// all emission are in g/s
			double emissionratepm25 = fractionpm25 * Double.valueOf(resultListParticlePollutant.get(0)[0]);
			double emissionratepm10 = fractionpm10 * Double.valueOf(resultListParticlePollutant.get(0)[0]);
			double nox = Double.valueOf(mappollutant.getString("ChemSpecies_Nitrogen__dioxide_EmissionRate"))
					+ Double.valueOf(mappollutant.getString("PseudoComponent_Nitrogen__oxides_EmissionRate"));
			double voc = Double.valueOf(mappollutant.getString("PseudoComponent_Unburned_Hydrocarbon_EmissionRate"));
			double co = Double.valueOf(mappollutant.getString("ChemSpecies_Carbon__monoxide_EmissionRate"));
			double so2 = Double.valueOf(mappollutant.getString("ChemSpecies_Sulfur__dioxide_EmissionRate"));

			double area = Math.PI * Math.pow(Double.valueOf(resultListChimneyGasPollutant.get(0)[1]) / 2, 2);
			double massflowrate = Double.valueOf(resultListChimneyGasPollutant.get(0)[2]); // in kg/hr
			double density = Double.valueOf(resultListChimneyGasPollutant.get(0)[4]); // kg/m3
			double velocity = massflowrate / area / density; // should be in m/hr

			String[] content = new String[21];
			content[0] = "8";
			double shipx = coordinateship.getJSONObject(ship).getDouble(DATA_KEY_LON);
			double shipy = coordinateship.getJSONObject(ship).getDouble(DATA_KEY_LAT);
			double[] locationshipconverted = CRSTransformer.transform("EPSG:4326", "EPSG:32648",
					new double[] { shipx, shipy });
			content[1] = "" + locationshipconverted[0];
			content[2] = "" + locationshipconverted[1];
			content[3] = resultListChimneyGasPollutant.get(0)[0];// QUERY FROM CHIMNEY height //in m
			content[4] = "" + velocity;// DERIVED FROM CHIMNEY velocity
			content[5] = resultListChimneyGasPollutant.get(0)[3];// QUERY FROM CHIMNEY //temperature celcius
			content[6] = "" + (Double.valueOf(resultListChimneyGasPollutant.get(0)[1]) / 2);// QUERY FROM CHIMNEY radius
																							// //in m
			content[7] = "10"; // constant unused building height
			content[8] = "20";// constant unused building width;
			content[9] = coordinateship.getJSONObject(ship).get("speed").toString(); // should be in knot from the
																							// beginning ??
			content[10] = coordinateship.getJSONObject(ship).get("angle").toString();
			content[11] = "0";// circularangle assume it moves straightline
			content[12] = "0";// moving starting time;
			content[13] = "3600";// moving ending time;
			content[14] = "" + unitflowconverter(nox);
			content[15] = "" + unitflowconverter(voc); // voc
			content[16] = "" + unitflowconverter(co); // CO
			content[17] = "" + unitflowconverter(so2); // SO2
			content[18] = "-999"; // NH3 not avilable
			content[19] = "" + unitflowconverter(emissionratepm25); // pm2.5
			content[20] = "" + unitflowconverter(emissionratepm10); // pm10
			resultquery.add(content);
		}
		    
		 new QueryBroker().putLocal(dataPath + "/"+filename, MatrixConverter.fromArraytoCsv(resultquery)); 
	}

	private List<String[]> queryIRI(String chimneyiriInfo, OntModel jenaOwlModel) {
		ResultSet resultSet = JenaHelper.query(jenaOwlModel, chimneyiriInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		return resultList;
	}

	public void createControlTopologyFile(List<String>srtmlist,JSONObject region,String dataPath, String Filename) throws IOException {
		JSONObject in= new JSONObject();
		JSONArray srtm= new JSONArray();
		for(int x=0;x<srtmlist.size();x++) {
			srtm.put(srtmlist.get(x));
		}
		in.put("srtminput",srtm);
		in.put("regioninput",region);
		String  modifiedcontent=modifyTemplate(Filename,in);
		 new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createControlWeatherFile(JSONObject region,String dataPath, String Filename,List<String>stniri) throws IOException {
		JSONArray weather=new JSONArray();
		for(int t=0; t<stniri.size();t++) {
			JSONObject stn= new JSONObject();
			String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
					+ "PREFIX j6:<http://www.w3.org/2006/time#>" 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "SELECT DISTINCT ?valueofx ?valueofy ?valueofz "
					+ "{ GRAPH <"+stniri.get(t)+"> "
					+ "{ "
					 
					+ "  ?entity j4:observes ?prop ." 
					+ "  ?prop a j4:OutsideAirTemperature ." 
					+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."

					+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
					+ "?y  j2:hasValue ?vy ." 
					+ "?vy  j2:numericalValue ?valueofy ."

					+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
					+ "?x  j2:hasValue ?vx ." 
					+ "?vx  j2:numericalValue ?valueofx ."
					+ "?coorsys  j7:hasProjectedCoordinate_z  ?z  ."
					+ "?z  j2:hasValue ?vz ." 
					+ "?vz  j2:numericalValue ?valueofz ."
					
					+ "}" 
					+ "}" ;
			
			RepositoryConnection con = WeatherAgent.repo.getConnection();
				TupleQuery query = con.prepareTupleQuery(QueryLanguage.SPARQL, sensorinfo);
				TupleQueryResult result = query.evaluate();
				int d = 0;
				try {
					while (result.hasNext()) {
						BindingSet bindingSet = result.next();
						String name=stniri.get(t).split("#")[1];
						String valueofx = bindingSet.getValue("valueofx").stringValue();
						String valueofy = bindingSet.getValue("valueofy").stringValue();
						String valueofz = bindingSet.getValue("valueofz").stringValue();
						
						stn.put("name",name);
						stn.put("x",valueofx);
						stn.put("y",valueofy);
						stn.put("z",valueofz);

						d++;
					}
					System.out.println("total data=" + d);

				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
				weather.put(stn);	
		}

		JSONObject in= new JSONObject();
		in.put("regioninput",region);
		in.put("weatherinput",weather);
		String  modifiedcontent=modifyTemplate(Filename,in);
		 new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createReceptorFile(JSONObject inputparameter,String dataPath, String Filename) {
		String lowx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowerx").toString();
		String lowy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowery").toString();
		String upx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("upperx").toString();
		String upy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("uppery").toString();
	       

		double z_rec=9.5;//TODO hardcoded?
		double nx_rec =(Double.valueOf(upx)-Double.valueOf(lowx))/dx_rec;
		double ny_rec =(Double.valueOf(upy)-Double.valueOf(lowy))/dy_rec;
		
		//assume no monitor station is needed
		List<String[]>resultquery= new ArrayList<String[]>();
		String[]key= {"simid","nstation","nx_rec","ny_rec","dx_rec","dy_rec","z_rec","xsw_main","ysw_main","rcmax"};
		resultquery.add(key);
		String[] content= new String[key.length];
		content[0]="{Citychem-Singapore--testnotapm-00001}";
		content[1]="0"; //assume no monitor station
		content[2]=""+nx_rec;	
		content[3]=""+ny_rec;
		content[4]=""+dx_rec;
		content[5]=""+dy_rec;
		content[6]=""+z_rec;
		content[7]=lowx;
		content[8]=lowy;	
		content[9]="-9900";
		resultquery.add(content);
		
		
		
		//convert to tsv
		 StringBuilder sb= new StringBuilder();
        for(int v=0;v<resultquery.get(0).length;v++) {
	        for (int c=0;c<resultquery.size();c++) {
	        	sb.append(resultquery.get(c)[v]);
	        	if(c!=1) {
	        		sb.append(separator);
	        	}
	        	else {
	        		sb.append("\n");
	        	}
	        }
        	
        }
        
        new QueryBroker().putLocal(dataPath + "/"+Filename, sb.toString());
        
	}
	
	private double[] calculateCenterPoint(double xup, double yup,double xdown, double ydown) {
		double xcenter=(xup+xdown)/2;
		double ycenter=(yup+ydown)/2;
		double[]res= {xcenter,ycenter};
		return res;
	}
	private double[] calculateLowerLeftInit(double xup, double yup,double xdown, double ydown) {
		double xlowerinit=-1*(xup-xdown)/2;
		double ylowerinit=-1*(yup-ydown)/2;
		double[]res= {xlowerinit,ylowerinit};
		return res;
	}
	private double[] calculatenumberCellDetails(double xup, double yup,double xdown, double ydown, double dx, double dy) {
		// dx and dy should be  min 1000 m
		double nx=(xup-xdown)/dx;
		double ny=(yup-ydown)/dy;
		if(nx%5!=0||ny%5!=0) { //nx and ny should be factor of 5
			System.out.println("boundary conditions is not fulfilled");
			System.out.println("nx= "+nx);
			System.out.println("ny= "+ny);
			
			return null;
		}
		double[]ncell= {nx,ny};
		return ncell;
	}
	
	public String modifyTemplate(String filename,JSONObject inputparameter) throws IOException { 
		
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		String fileContext = FileUtils.readFileToString(file);
		String epsg=inputparameter.getJSONObject("regioninput").getJSONObject("region").get("srsname").toString();
		String lowx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowerx").toString();
		String lowy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("lowercorner").get("lowery").toString();
		String upx=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("upperx").toString();
		String upy=inputparameter.getJSONObject("regioninput").getJSONObject("region").getJSONObject("uppercorner").get("uppery").toString();



		//it is assumed to be in utm 48 or this calculation?
		double proclowx=Double.valueOf(lowx);
		double procupx=Double.valueOf(upx);
		double proclowy=Double.valueOf(lowy);
		double procupy=Double.valueOf(upy);
		double[] center=calculateCenterPoint(procupx,procupy,proclowx,proclowy);
		double[] leftcorner=calculateLowerLeftInit(procupx,procupy,proclowx,proclowy);
		double[] ncell=calculatenumberCellDetails(procupx,procupy,proclowx,proclowy,dx,dy);
		
		if(filename.contentEquals("aermap.inp")) { //assume srtm=1
			String srtmin=inputparameter.getJSONArray("srtminput").get(0).toString();
			String tif="N01E103.tif";
			//String tif1="DATAFILE  "+"./srtm3/"+srtmin+"   tiffdebug";
			String tif1="   DATAFILE  \"./srtm3/"+tif+"\"   tiffdebug";
			String tif1aft="DATAFILE  "+"./srtm3/"+srtmin+"   tiffdebug";
			String tif2="   DATAFILE  \"./srtm3/N01E104.tif\"   tiffdebug";
			fileContext = fileContext.replaceAll(tif1,tif1aft); //replace with the new tif
			fileContext = fileContext.replaceAll(tif2,""); //replace with the new tif
			//fileContext = fileContext.replaceAll("48",""); //replace with the new value of UTM coordinate system if needed
			fileContext = fileContext.replaceAll("330600.0",""+(proclowx-1000)); //replace with the new value xmin-1000
			fileContext = fileContext.replaceAll("118000.0",""+(proclowy-1000)); //replace with the new value ymin-1000
			fileContext = fileContext.replaceAll("402600.0",""+(procupx+1000)); //replace with the new value xmax+1000
			fileContext = fileContext.replaceAll("190000.0",""+(procupy+1000)); //replace with the new value ymax+1000
			fileContext = fileContext.replaceAll("366600.0",""+center[0]); //replace with the new value x center point
			fileContext = fileContext.replaceAll("154000.0",""+center[1]); //replace with the new value y center point
			fileContext = fileContext.replaceAll("-35000.0a",""+leftcorner[0]);
			fileContext = fileContext.replaceAll("35b",""+ncell[0]);
			fileContext = fileContext.replaceAll("2000.0c",""+dx);
			fileContext = fileContext.replaceAll("-35000.0d",""+leftcorner[1]);
			fileContext = fileContext.replaceAll("35e",""+ncell[1]);
			fileContext = fileContext.replaceAll("2000.0f",""+dy);
			
			return fileContext;

		}else if(filename.contentEquals("run_file.asc")) {
			String name1=inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("name").toString();
			String loc1x=inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("x").toString();
			String loc1y=inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("y").toString();
			String heighttemp=inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("z").toString();
			String name2=inputparameter.getJSONArray("weatherinput").getJSONObject(1).get("name").toString();
			String loc2x=inputparameter.getJSONArray("weatherinput").getJSONObject(1).get("x").toString();
			String loc2y=inputparameter.getJSONArray("weatherinput").getJSONObject(1).get("y").toString();
			
			double[] p1convert = CRSTransformer.transform("EPSG:4326", "EPSG:32648", new double[] {Double.valueOf(loc1x),Double.valueOf(loc1y)});
			double[] p2convert = CRSTransformer.transform("EPSG:4326", "EPSG:32648", new double[] {Double.valueOf(loc2x),Double.valueOf(loc2y)});
			double dx1=p1convert[0]-proclowx;
			double dy1=p1convert[1]-proclowy;
			double dx2=p2convert[0]-proclowx;
			double dy2=p2convert[1]-proclowy;
			
			
			//System.out.println("line 0= "+filename);
			String[]line=fileContext.split("\n");
			List<String> lineoffile=Arrays.asList(line);
			List<String> newcontent= new ArrayList<String>();
			for( int r=0;r<23;r++) {
				newcontent.add(lineoffile.get(r));
			}
			
			double[] pmidconvert = CRSTransformer.transform("EPSG:32648", "EPSG:4326", new double[] {center[0], center[1]});
			 DecimalFormat df= new DecimalFormat("#.#") ;
			 String xmid=df.format(pmidconvert[0]);
			 System.out.println("xmid="+xmid);
			 String ymid=df.format(pmidconvert[1]);
			 System.out.println("ymid="+ymid);
			 String line23=lineoffile.get(23);
			 line23=line23.replace("1.4", ymid);
			 line23=line23.replace("103.8", xmid);
			 newcontent.add(line23);
			 
			 String line24=lineoffile.get(24);
			 line24=line24.replace("-8","-8");//change if timezone is different
			 newcontent.add(line24);
			 newcontent.add(lineoffile.get(25));
			 String line26b=lineoffile.get(26).split("!")[1]+"!"+lineoffile.get(26).split("!")[2]+"!"+lineoffile.get(26).split("!")[3];
			 String line26=ncell[0]+separator+ncell[1]+separator+nz+separator+"!"+line26b;
			 newcontent.add(line26);
			 System.out.println("line26= "+line26);
			 
			 String line27b=lineoffile.get(27).split("!")[1];
			 String line27=dx+separator+dy+separator+"!"+line27b;
			 newcontent.add(line27);
			 System.out.println("line27= "+line27);
			 newcontent.add(lineoffile.get(28));//base with stretch factor
			 newcontent.add(lineoffile.get(29));//base
			 for(int r=1;r<nz;r++) {
				 newcontent.add(dz+separator+"!"+"\n");
			 }
			for( int r=42;r<73;r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(name1+separator+"!"+lineoffile.get(73).split("!")[1]);
			newcontent.add(dx1+separator+dy1+separator+"!"+lineoffile.get(74).split("!")[1]);
			newcontent.add(lineoffile.get(75));
			newcontent.add(heighttemp+separator+"!"+lineoffile.get(76).split("!")[1]);
			newcontent.add(upperheight+separator+"!"+lineoffile.get(77).split("!")[1]);
			newcontent.add(lowerheight+separator+"!"+lineoffile.get(78).split("!")[1]);
			for( int r=79;r<82;r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(name2+separator+"!"+lineoffile.get(82).split("!")[1]);
			newcontent.add(dx2+separator+dy2+separator+"!"+lineoffile.get(83).split("!")[1]);
			for( int r=84;r<89;r++) {
				newcontent.add(lineoffile.get(r));
			}
			 //assume wind measurement height=const for both
				
					
			 String contentupdate="";
			 for (int x=0;x<newcontent.size();x++) {
				 contentupdate+=newcontent.get(x);
			 }
			return contentupdate;
		}
		
		
		return fileContext;
	}
}
