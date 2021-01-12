package uk.ac.cam.cares.jps.dispersion.episode;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.RoundingMode;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletException;
import javax.ws.rs.BadRequestException;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.ModelFactory;
import org.cts.CRSFactory;
import org.cts.registry.EPSGRegistry;
import org.cts.registry.RegistryManager;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.dispersion.general.DispersionModellingAgent;
import uk.ac.cam.cares.jps.dispersion.sparql.WeatherStation;

public class EpisodeAgent extends DispersionModellingAgent {
	
	public  EpisodeAgent() {
		  EpisodeConfig episodeconfig= new EpisodeConfig();  // Set the initial value for config
		  System.out.println("initializing the value of config properties");
		 System.out.println("episode confif dxrec= "+episodeconfig.getDx_rec());
		 dx_rec=episodeconfig.getDx_rec();
		 dy_rec=episodeconfig.getDy_rec();
		 z_rec=episodeconfig.getZ_rec();
		 nx=episodeconfig.getNx();
		 ny=episodeconfig.getNy();
		 dz=episodeconfig.getDz();
		 nz=episodeconfig.getNz();
		 upperheight=episodeconfig.getUpperheight();
		 lowerheight=episodeconfig.getLowerheight();
		 deltaT=episodeconfig.getDeltaT();
		 
		  }

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final String separator="\t";
	
	//later to be moved to config
	private double dx_rec; // decide the dx for the receptor
	private double dy_rec;// decide the dy for the receptor
	private double z_rec;//decide the base receptor level
	private int nx ;//decide the dx for the scope
	private int ny;//decide the dy for the scope	
	private double dz;
	private int nz;
	private double upperheight;
	private double lowerheight;
	private double deltaT;
	boolean restart=false;
	
	//below is based on location input (city iri)
	private String epsgInUTM="48N";//48N
	private String epsgActive="EPSG:32648";
	private String gmttimedifference="-8"; //it should be dependent on the location it simulates
	
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
            + "?entity   j4:realizes ?proc ."
            + "?proc j5:hasOutput ?waste ."
            + "?waste j6:refersToGeneralizedAmount ?genwaste ."
            + "?genwaste   j2:contains ?particle ." 
            + 
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
		

    
	public static void main(String[] args) throws ServletException{
		EpisodeAgent episodeAgent = new EpisodeAgent();
		episodeAgent.init();
	}
    
    private double unitflowconverter(double numberInGramPerS) {
    	double result=numberInGramPerS*0.001*365*24*3600;
    	return result;
    }
    
	public void copyTemplate(String newdir, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
    
	private List<String[]> queryKBIRI(String chimneyiriInfo, OntModel jenaOwlModel) {
		ResultSet resultSet = JenaHelper.query(jenaOwlModel, chimneyiriInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		return resultList;
	}
	
	
	public String getPreviousHourDatapath(String agent,String cityIRI) {
	
		String metadataResult;
		long millis = System.currentTimeMillis();
		String toSimulationTime = MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis);
		String fromSimulationTime = MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis-3600*1000);
    	List<String> topics = new ArrayList<String>();
    	topics.add(cityIRI);
		metadataResult = MetaDataQuery.queryResources(null, null, null, agent,  fromSimulationTime, toSimulationTime, null, topics);
        String[] keys = JenaResultSetFormatter.getKeys(metadataResult);
        List<String[]>listmap = JenaResultSetFormatter.convertToListofStringArrays(metadataResult, keys);
        int size=listmap.size();
        if(size<1) {
        	return "empty";
        }
        String directory=listmap.get(0)[0];
        String datapath=directory.split("/output")[0];
        
		return datapath;
	}
	
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(EpisodeAgent.class);
    }
	 Logger logger = LoggerFactory.getLogger(EpisodeAgent.class);
	
	 
	    @Override
    protected JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject responseParams=new JSONObject();

        if (validateInput(requestParams)) {
            JSONArray stnIRI=requestParams.getJSONArray("stationiri"); //ok
            JSONObject shipdata=requestParams.getJSONObject("ship");
            String dataPath = QueryBroker.getLocalDataPath()+"/input";
            String cityIRI = requestParams.getString("city"); //later to be used for annotation??
            String agent=requestParams.getString("agent");
            String airstn=requestParams.getString("airStationIRI");
            String extrainfo=requestParams.toString();
            new QueryBroker().putLocal(QueryBroker.getLocalDataPath()+"/extra_info.json", extrainfo);			

            // create a Scope object
            JSONObject region = requestParams.getJSONObject(Region.keyRegion);
            Scope sc = new Scope(region);

            // collect region specific properties
            epsgInUTM = sc.getUTMzone();
            epsgActive = Region.getTargetCRSName(agent, cityIRI);
            // time zone required by Episode is the negative of GMT, see section 4.3 of citychem user guide
            gmttimedifference = String.valueOf(-sc.getTimeZone());

            // convert scope to local CRS
            sc.transform(epsgActive);

            // Get filenames required for topology
            List<String>srtm=new ArrayList<String>();
            srtm = Region.getSRTM(cityIRI);
            for(int x=0;x<srtm.size();x++) {
                // copy topology files from workingdir to simulation directory
                // source files have hgt extension
                // note that the code is currently hard coded to take in a maximum of 2 hgt files
                copyTemplate(dataPath, srtm.get(x)+".hgt");
            }

            List<String>stniri=new ArrayList<String>();
            stniri.add(stnIRI.getString(0));
            stniri.add(stnIRI.getString(1));

            WeatherStation[] weatherStations = new WeatherStation[stnIRI.length()];
            // first station is the main station, second station only provides wind speed/direction
            for (int i=0; i<stnIRI.length(); i++) {
            	weatherStations[i] = new WeatherStation(stnIRI.getString(i));
            }
            
            //check if it's the first run or not
            String olddatapath=getPreviousHourDatapath(agent,cityIRI);
            if(!olddatapath.contains("empty")) {
                restart=true;
                File file = new File( olddatapath+ "/output/icmhour.nc");
                File file3des=new File(dataPath + "/icmhour.nc");
                File file2 = new File( olddatapath+ "/output/plume_segments.dat");
                File file2des=new File(dataPath + "/plume_segments.dat");
                try {
                    FileUtils.copyFile(file, file3des);
                    FileUtils.copyFile(file2, file2des);
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                //new QueryBroker().putLocal(dataPath + "/icmhour.nc", file);

                //new QueryBroker().putLocal(dataPath + "/plume_segments.dat", file2);
            }

            System.out.println("Creating input files for Episode simulation...");

            createEmissionInput(dataPath, "points.csv",shipdata);
            createEmissionInput(dataPath, "lines.csv",shipdata);
            try { //for control file
                createControlTopologyFile(srtm, dataPath, "aermap.inp",sc);
                createControlWeatherORCityChemFile(dataPath, "run_file.asc",weatherStations,sc);
                createControlWeatherORCityChemFile(dataPath, "citychem_restart.txt",weatherStations,sc);
                createControlEmissionFile(shipdata,dataPath,"cctapm_meta_LSE.inp",sc);
                createControlEmissionFile(shipdata,dataPath,"cctapm_meta_PSE.inp",sc);
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            createReceptorFile(dataPath,"receptor_input.txt",sc);
            createWeatherInput(dataPath,"mcwind_input.txt",weatherStations);

            //zip all the input file created
            File inputfile=null;
            try {
                inputfile=getZipFile(dataPath);

            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            try {
                JSONObject jsonforslurm = new JSONObject();
                boolean value=true;
//              if (restart==true) {//all the time must be true??
//                  value=false;
//              }
                long millis = System.currentTimeMillis();
                String executiontime=MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis);
                jsonforslurm.put("runWholeScript",value);
                jsonforslurm.put("city",cityIRI);
                jsonforslurm.put("agent",agent);
                jsonforslurm.put("datapath",dataPath.split("/input")[0]+"/output");
                jsonforslurm.put("expectedtime", executiontime);
                jsonforslurm.put("airStationIRI", airstn);
                setUpJob(jsonforslurm.toString(),dataPath);
            } catch (IOException | SlurmJobException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            responseParams.put("folder",dataPath.split("/input")[0]+"/output/3D_instantanous_mainconc_center.dat"); //or withtBCZ?
        }
        return responseParams;
    }

    private boolean validateInput(JSONObject input) {
        // Validate inputs for the processRequestParameters method
        // Returns true if complete set of inputs are present and false if not
        boolean valid = false;
            try {
                JSONArray stnIRI=input.getJSONArray("stationiri");
                if(stnIRI.length() < 2) {
                    System.out.println("Number of weather stations:" + stnIRI.length());
                    System.out.println("Episode agent: At least 2 weather stations are required.");
                    throw new Exception();
                }
                // at the moment it will always give two stations, in principle episode can use more than 2 stations
                for(int x=0;x<stnIRI.length();x++) {
                    // check if it's a valid URL
                    new URL(stnIRI.getString(x)).toURI();
                }
                JSONObject region = input.getJSONObject("region");
                // try creating a scope object
                Scope sc = new Scope(region); 
                String sourceCRSName = sc.getCRSName();

                // check if CRS is valid
                CRSFactory crsFact = new CRSFactory();
                RegistryManager registryManager = crsFact.getRegistryManager();
                registryManager.addRegistry(new EPSGRegistry());
                crsFact.getCRS(sourceCRSName);

                // city IRI
                String cityIRI = input.getString("city");
                new URL(cityIRI).toURI();
                // Agent IRI
                String agent=input.getString("agent");
                new URL(agent).toURI();
                
                valid = true;
            } catch (Exception e) {
                throw new BadRequestException(e);
        }
        return valid;
    }

    @Override
	public void createWeatherInput(String dataPath, String filename,List<String>stniri) {	
		 List<String[]> resultquery = new ArrayList<String[]>();

	        String[]header= {"*","yyyy","mm","dd","hh","FF1","DD1","T25m","DT","RH%","PP_mm","Cloud","Press","FF2","DD2"};
	        resultquery.add(0,header);
	        String[]content=new String[15];
	        content[0]="";
	        content[8]=""+deltaT; //currently hardcoded about the delta T but can be substituted by model
	        
    	for(int x=0;x<stniri.size();x++) {
    		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
    				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
    				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
    				+ "PREFIX j6:<http://www.w3.org/2006/time#>" 
    				+ "SELECT ?class ?propval ?proptimeval "
    				+ "{ GRAPH <"+stniri.get(x)+"> "
    				+ "{ "
    				 
    				+ "  ?entity j4:observes ?prop ." 
    				+ " ?prop a ?class ."
    				+ " ?prop   j2:hasValue ?vprop ."
    				+ " ?vprop   j2:numericalValue ?propval ." 
    				+ " ?vprop   j6:hasTime ?proptime ."
    				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 
    				+ "}" 
    				+ "}" 
    				+ "ORDER BY DESC(?proptimeval)LIMIT7";
    		List<String[]> listmap = queryEndPointDataset(sensorinfo);
    		for(int r=0;r<listmap.size();r++) {
    			if(x==0) {
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
    				String time=listmap.get(0)[2];
    	    		content[0]="";
    		        content[1]=time.split("-")[0];
    		        content[2]=time.split("-")[1];
    		        content[3]=time.split("-")[2].split("T")[0];
    		        content[4]=time.split("-")[2].split("T")[1].split(":")[0];
    				
    			}else {
    				if(listmap.get(r)[0].toLowerCase().contains("speed"))
    	        		content[13]=listmap.get(r)[1];
    	        		else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
    	        			content[14]=listmap.get(r)[1];
    	        		}
    			}
    		}
	        if(content[9]==null) {
	        	content[9]="68";
	        }
	        if(content[7]==null) {
	        	content[7]="25";
	        }
    	}
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

    // new method using weather station objects
    private void createWeatherInput(String dataPath, String filename,WeatherStation[] ws) {	
		List<String[]> resultquery = new ArrayList<String[]>();

	    String[]header= {"*","yyyy","mm","dd","hh","FF1","DD1","T25m","DT","RH%","PP_mm","Cloud","Press","FF2","DD2"};
	    resultquery.add(0,header);
	    String[]content=new String[15];
	    content[0]="";
	    LocalDateTime dateTime = LocalDateTime.ofEpochSecond(ws[0].getTimestamp(), 0, ZoneOffset.ofHours(-Integer.parseInt(gmttimedifference)));
	    content[1] = String.valueOf(dateTime.getYear()); 
	    content[2] = String.valueOf(dateTime.getMonthValue());
	    content[3] = String.valueOf(dateTime.getDayOfMonth());
	    content[4] = String.valueOf(dateTime.getHour());
	    content[5] = String.valueOf(ws[0].getWindspeed());
	    content[6] = String.valueOf(ws[0].getWinddirection());
	    content[7] = String.valueOf(ws[0].getTemperature());
	    content[8]=""+deltaT; //currently hardcoded about the delta T but can be substituted by model
	    content[9] = String.valueOf(ws[0].getHumidity());
	    content[10] = String.valueOf(ws[0].getPrecipitation());
	    content[11] = String.valueOf(ws[0].getCloudcover());
	    content[12] = String.valueOf(ws[0].getPressure());
	    content[13] = String.valueOf(ws[1].getWindspeed());
	    content[14] = String.valueOf(ws[1].getWinddirection());
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
		JSONArray coordinateship = getEntityData(shipdata);
		if (filename.contains("point")) {
			
			System.out.println("it goes to create point emission input here");

			int shipAmount = coordinateship.length();
			List<String[]> resultquery = new ArrayList<String[]>();
			String[] header = { "snap", "xcor", "ycor", "Hi", "Vi", "Ti", "radi", "BH", "BW", "Pvec", "Pdir",
					"pcir_ang", "Ptstart", "Ptend", "NOx", "NMVOC", "CO", "SO2", "NH3", "PM2.5", "PM10" };
			resultquery.add(0, header);
			for (int ship = 0; ship < shipAmount; ship++) {
				String mmsi = coordinateship.getJSONObject(ship).get(DATA_KEY_MMSI).toString();

				OntModel jenaOwlModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
				String iriofchimney = "http://www.theworldavatar.com/kb/ships/" + mmsi + "/" + "Chimney-1.owl";
				if (AgentLocator.isJPSRunningForTest()) {
					iriofchimney = "http://localhost:8080/kb/ships/" + mmsi + "/" + "Chimney-1.owl";
				}

				jenaOwlModel.read(iriofchimney);
				System.out.println("iri chimney now= " + mmsi);
				List<String[]> resultListParticlePollutant = queryKBIRI(chimneyiriparticleInfo, jenaOwlModel);
				List<String[]> resultListChimneyGasPollutant = queryKBIRI(chimneyiriInfo, jenaOwlModel);
				JSONObject mappollutant = new JSONObject();
				for (int d = 0; d < resultListChimneyGasPollutant.size(); d++) {
					if (resultListChimneyGasPollutant.get(d)[5].contains("EmissionRate")) {
						String key = resultListChimneyGasPollutant.get(d)[5].split("#V_")[1];
						mappollutant.put(key, resultListChimneyGasPollutant.get(d)[6]);
					}
				}

				double fractionpm25 = 0.0;
				double fractionpm10 = 0.0;
				for (int x = 0; x < resultListParticlePollutant.size(); x++) {
					if (Double.valueOf(resultListParticlePollutant.get(x)[2]) <= 0.0000025) {
						fractionpm25 = fractionpm25 + Double.valueOf(resultListParticlePollutant.get(x)[1]);
					}
					if (Double.valueOf(resultListParticlePollutant.get(x)[2]) <= 0.00001) {
						fractionpm10 = fractionpm10 + Double.valueOf(resultListParticlePollutant.get(x)[1]);
					}
				}
				// all emission are in g/s
				double emissionratepm25 = fractionpm25 * Double.valueOf(resultListParticlePollutant.get(0)[0]);
				double emissionratepm10 = fractionpm10 * Double.valueOf(resultListParticlePollutant.get(0)[0]);
				double nox =Double.valueOf(mappollutant.getString("PseudoComponent_Nitrogen__oxides_EmissionRate"));
				double voc = Double
						.valueOf(mappollutant.getString("PseudoComponent_Unburned_Hydrocarbon_EmissionRate"));
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
				double[] locationshipconverted = CRSTransformer.transform("EPSG:4326", epsgActive,
						new double[] { shipx, shipy });
				content[1] = "" + locationshipconverted[0];
				content[2] = "" + locationshipconverted[1];
				content[3] = resultListChimneyGasPollutant.get(0)[0];// QUERY FROM CHIMNEY height //in m
				content[4] = "" + velocity;// DERIVED FROM CHIMNEY velocity
				content[5] = resultListChimneyGasPollutant.get(0)[3];// QUERY FROM CHIMNEY //temperature celcius
				content[6] = "" + (Double.valueOf(resultListChimneyGasPollutant.get(0)[1]) / 2);// QUERY FROM CHIMNEY
																								// radius in m
				content[7] = "10"; // constant unused building height
				content[8] = "20";// constant unused building width;
				//this value is in knot, but in Kang's document, it should be in m/s
				content[9] = coordinateship.getJSONObject(ship).get("speed").toString(); // should be in knot?
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

			new QueryBroker().putLocal(dataPath + "/" + filename, MatrixConverter.fromArraytoCsv(resultquery));
		} else {
			File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/" + filename);
			double x0 = coordinateship.getJSONObject(0).getDouble(DATA_KEY_LON);
			double y0 = coordinateship.getJSONObject(0).getDouble(DATA_KEY_LAT);
			double[] locationshipconverted0 = CRSTransformer.transform("EPSG:4326", epsgActive,
					new double[] { x0, y0 });
			double[] locationshipconverted1 = CRSTransformer.transform("EPSG:4326", epsgActive,
					new double[] { x0+0.1, y0+0.1 });
			try {
				String fileContext = FileUtils.readFileToString(file);
				fileContext = fileContext.replaceAll("351474", "" + locationshipconverted0[0]);
				fileContext = fileContext.replaceAll("133855", "" + locationshipconverted0[1] );
				fileContext = fileContext.replaceAll("351476", "" + locationshipconverted1[0]);
				fileContext = fileContext.replaceAll("139903", "" +locationshipconverted1[1] );
				new QueryBroker().putLocal(dataPath + "/"+filename, fileContext); 
			} catch (IOException e) {
				logger.error(e.getMessage());

			}

		}
	}

	public void createControlTopologyFile(List<String>srtmlist,String dataPath, String Filename, Scope sc) throws IOException {
		JSONObject in= new JSONObject();
		JSONArray srtm= new JSONArray();
		for(int x=0;x<srtmlist.size();x++) {
			srtm.put(srtmlist.get(x));
		}
		in.put("srtminput",srtm);
		String  modifiedcontent=modifyTemplate(Filename,in,sc);
		 new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createControlWeatherORCityChemFile(String dataPath, String Filename,List<String>stniri,Scope sc) throws IOException {
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
			
			List<String[]> listmap = queryEndPointDataset(sensorinfo);
			for(int d=0;d<listmap.size();d++) {
				String name=stniri.get(t).split("#")[1];
				String valueofx = listmap.get(d)[0];
				String valueofy = listmap.get(d)[1];
				String valueofz = listmap.get(d)[2];
				stn.put("name",name);
				stn.put("x",valueofx);
				stn.put("y",valueofy);
				stn.put("z",valueofz);
			}
				weather.put(stn);	
		}

		JSONObject in= new JSONObject();
		in.put("weatherinput",weather);
		String  modifiedcontent=modifyTemplate(Filename,in,sc);
		 new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createControlWeatherORCityChemFile(String dataPath, String Filename,WeatherStation[] ws,Scope sc) throws IOException {
		JSONArray weather=new JSONArray();
		for(int i=0; i<ws.length; i++) {
			JSONObject stn= new JSONObject();
			stn.put("name",ws[i].getStationiri().split("#")[1]);
			stn.put("x", String.valueOf(ws[i].getXcoord()));
			stn.put("y", String.valueOf(ws[i].getYcoord()));
			stn.put("z", String.valueOf(ws[i].getZcoord()));
			weather.put(stn);
		}

		JSONObject in= new JSONObject();
		in.put("weatherinput",weather);
		String  modifiedcontent=modifyTemplate(Filename,in,sc);
	    new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createControlEmissionFile(JSONObject shipdata,String dataPath,String Filename,Scope sc) throws IOException {
		JSONObject in= new JSONObject();
		in.put("sourceinput",shipdata);
		String  modifiedcontent=modifyTemplate(Filename,in,sc);
		new QueryBroker().putLocal(dataPath + "/"+Filename, modifiedcontent); 
	}
	
	public void createReceptorFile(String dataPath, String Filename, Scope sc) {
		DecimalFormat df = new DecimalFormat("0.0");
		df.setRoundingMode(RoundingMode.HALF_EVEN);

		int nx_rec =(int) Math.round((sc.getUpperx()-sc.getLowerx())/dx_rec);
		int ny_rec =(int) Math.round((sc.getUppery()-sc.getLowery())/dy_rec);

		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/" + Filename);
		String fileContext;
		try {
			fileContext = FileUtils.readFileToString(file);
			fileContext=fileContext.replaceAll("aaa",""+nx_rec);
			fileContext=fileContext.replaceAll("bbb",""+ny_rec);
			fileContext=fileContext.replaceAll("ccc",""+dx_rec);
			fileContext=fileContext.replaceAll("ddd",""+dy_rec);
			fileContext=fileContext.replaceAll("eee",""+z_rec);
			fileContext=fileContext.replaceAll("fff",""+Double.valueOf(df.format(sc.getLowerx())));
			fileContext=fileContext.replaceAll("ggg",""+Double.valueOf(df.format(sc.getLowery())));
			new QueryBroker().putLocal(dataPath + "/"+Filename, fileContext);
		} catch (IOException e) {
			logger.error(e.getMessage());
		}
  
	}
	
	private double[] calculateLowerLeftInit(double xup, double yup,double xdown, double ydown) {
		DecimalFormat df = new DecimalFormat("0.0");
		df.setRoundingMode(RoundingMode.HALF_EVEN);
		double xlowerinit=-1*(xup-xdown)/2;
		double ylowerinit=-1*(yup-ydown)/2;
		double newdx=Double.valueOf(df.format(xlowerinit));
		double newdy=Double.valueOf(df.format(ylowerinit));
		double[]res= {newdx,newdy};
		return res;
	}
	private double[] calculateEachDistanceCellDetails(double xup, double yup,double xdown, double ydown, int nx, int ny) {
		// dx and dy should be  min 1000 m
		double dx=(xup-xdown)/nx;
		double dy=(yup-ydown)/ny;
		System.out.println("dx= "+dx);
		System.out.println("dy= "+dy);
		DecimalFormat df = new DecimalFormat("0.0");
		df.setRoundingMode(RoundingMode.HALF_EVEN);
		double newdx=Double.valueOf(df.format(dx));
		double newdy=Double.valueOf(df.format(dy));
		//nx and ny should be factor of 5
//		if(nx%5!=0||ny%5!=0) { 
//			System.out.println("boundary conditions is not fulfilled");
//			System.out.println("nx= "+nx);
//			System.out.println("ny= "+ny);
//			
//			return null;
//		}
		double[]dcell= {newdx,newdy};
		return dcell;
	}
	
	public String modifyTemplate(String filename, JSONObject inputparameter, Scope sc) throws IOException {
		String time = WeatherAgent.provideCurrentTime();
		String fileContext = "";

		// Gather common scope details 
		DecimalFormat df = new DecimalFormat("0.0");
		df.setRoundingMode(RoundingMode.HALF_EVEN);

		double proclowx = sc.getLowerx();
		proclowx=Double.valueOf(df.format(proclowx));
		double procupx = sc.getUpperx();
		procupx=Double.valueOf(df.format(procupx));
		double proclowy = sc.getLowery();
		proclowy=Double.valueOf(df.format(proclowy));
		double procupy = sc.getUppery();
		procupy=Double.valueOf(df.format(procupy));

		double[] center = sc.getScopeCentre();
		double[] leftcorner = calculateLowerLeftInit(procupx, procupy, proclowx, proclowy);
		double[] dcell = calculateEachDistanceCellDetails(procupx, procupy, proclowx, proclowy, nx, ny);

		if (filename.contentEquals("aermap.inp")) { // assume srtm=1
			File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/" + filename);
			fileContext = FileUtils.readFileToString(file);
			int sizeofsrtm=inputparameter.getJSONArray("srtminput").length();
			String srtmin1 = inputparameter.getJSONArray("srtminput").get(0).toString();
			String tif1 = "   DATAFILE  \"./srtm3/N01E103.tif\"   tiffdebug";
			String tif1aft = "   DATAFILE  \"" + "./srtm3/" + srtmin1 + ".tif\"   tiffdebug";
			String tif2 = "   DATAFILE  \"./srtm3/N01E104.tif\"   tiffdebug";
			String tif2aft="";
			if(sizeofsrtm>1) {
				String srtmin2 = inputparameter.getJSONArray("srtminput").get(1).toString();
				tif2aft = "   DATAFILE  " + "\"./srtm3/" + srtmin2 + ".tif\"   tiffdebug";
			}

			fileContext = fileContext.replaceAll(tif1, tif1aft); // replace with the new tif
			fileContext = fileContext.replaceAll(tif2, tif2aft); // replace with the new tif
			fileContext = fileContext.replaceAll("48",epsgInUTM.substring(0,epsgInUTM.length()-1)); //without the N/S
			// of UTM coordinate system if needed
			fileContext = fileContext.replaceAll("330600.0", "" + (proclowx - 1000)); // replace with the new value
																						// xmin-1000
			fileContext = fileContext.replaceAll("118000.0", "" + (proclowy - 1000)); // replace with the new value
																						// ymin-1000
			fileContext = fileContext.replaceAll("402600.0", "" + (procupx + 1000)); // replace with the new value
																						// xmax+1000
			fileContext = fileContext.replaceAll("190000.0", "" + (procupy + 1000)); // replace with the new value
																						// ymax+1000
			fileContext = fileContext.replaceAll("366600.0", "" + center[0]); // replace with the new value x center
																				// point
			fileContext = fileContext.replaceAll("154000.0", "" + center[1]); // replace with the new value y center
																				// point
			fileContext = fileContext.replaceAll("-35000.0a", "" + leftcorner[0]);
			fileContext = fileContext.replaceAll("35b", "" + nx );
			fileContext = fileContext.replaceAll("2000.0c", "" + dcell[0]);
			fileContext = fileContext.replaceAll("-35000.0d", "" + leftcorner[1]);
			fileContext = fileContext.replaceAll("35e", "" +ny );
			fileContext = fileContext.replaceAll("2000.0f", "" + dcell[1]);

			return fileContext;

		} else if (filename.contains("cctapm_meta_LSE")||filename.contains("cctapm_meta_PSE")) {
			int size = inputparameter.getJSONObject("sourceinput").getJSONObject(DATA_KEY_COLLECTION).getJSONArray(DATA_KEY_ITEMS).length();
           String lseORpse=filename.split("_")[2].split(".inp")[0];     
           File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/cctapm_meta.inp");
   			 fileContext = FileUtils.readFileToString(file);   
			String[] line = fileContext.split("\n");
			List<String> lineoffile = Arrays.asList(line);
			List<String> newcontent = new ArrayList<String>();
			for (int r = 0; r < 9; r++) {
				newcontent.add(lineoffile.get(r));
			}
			String line10b = separator + "!" + separator + lineoffile.get(9).split("!")[1];
			newcontent.add(" '"+lseORpse+"'"+ line10b);
			for (int r = 10; r < 12; r++) {
				newcontent.add(lineoffile.get(r));
			}
			String line13b = separator + "!" + separator + lineoffile.get(12).split("!")[1];
			newcontent.add(
					" '" + time.split("-")[0] + time.split("-")[1] + time.split("-")[2].split("T")[0] + "'" + line13b);
			String line14b = separator + "!" + separator + lineoffile.get(13).split("!")[1];
			newcontent.add(
					" '" + time.split("-")[0] + time.split("-")[1] + time.split("-")[2].split("T")[0] + "'" + line14b);
			String line15b = separator + "!" + separator + lineoffile.get(14).split("!")[1];
			newcontent.add(" "+time.split("-")[0] + "," + time.split("-")[1] + "," + time.split("-")[2].split("T")[0] + ","
					+ time.split("T")[1].split(":")[0] + line15b);
			String line16b = separator + "!" + separator + lineoffile.get(15).split("!")[1];
			newcontent.add(" "+time.split("-")[0] + "," + time.split("-")[1] + "," + time.split("-")[2].split("T")[0] + ","
					+ time.split("T")[1].split(":")[0] + line16b);
			String line17b = separator + "!" + separator + lineoffile.get(16).split("!")[1];
			newcontent.add(" "+nx + line17b);
			String line18b = separator + "!" + separator + lineoffile.get(17).split("!")[1];
			newcontent.add(" "+ny + line18b);
			String line19b = separator + "!" + separator + lineoffile.get(18).split("!")[1];
			newcontent.add(" "+dx_rec + line19b);
			String line20b = separator + "!" + separator + lineoffile.get(19).split("!")[1];
			newcontent.add(" "+dcell[0] + line20b);
			String line21b = separator + "!" + separator + lineoffile.get(20).split("!")[1];
			newcontent.add(" "+proclowx + "," + proclowy + line21b);// corner left
			String line22b = separator + "!" + separator + lineoffile.get(21).split("!")[1];
			newcontent.add(" '"+epsgInUTM+"'"+ line22b);// UTM
			String line23b = separator + "!" + separator + lineoffile.get(22).split("!")[1];
			newcontent.add(" "+size+line23b);// number of point source?
			for (int r = 23; r < lineoffile.size(); r++) {
				newcontent.add(lineoffile.get(r));
			}
			String contentupdate = ""; // process from arraylist to string
			for (int x = 0; x < newcontent.size(); x++) {
				contentupdate += newcontent.get(x);
			}
//			contentupdate = contentupdate.replaceAll("\r\n", "\n");
			contentupdate = contentupdate.replaceAll("\\r", "\n");
			return contentupdate;

		} else if (filename.contentEquals("citychem_restart.txt")) {
			String line502=" ../INPUT/other/icmhour.nc"+"\n";
			int restartindex=0;
			if(restart==true) {
				line502="../INPUT/other/icmhour.nc"+"\n";
				restartindex=1;	
			}
			File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/" + filename);
			fileContext = FileUtils.readFileToString(file);
			String timeinput=time.split("-")[0] + time.split("-")[1] + time.split("-")[2].split("T")[0];
			fileContext=fileContext.replace("20191118_20191118", timeinput+"_"+timeinput);
			fileContext=fileContext.replace("20130701_20130731",timeinput+"_"+timeinput);
			String name1 = inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("name").toString(); //main
			String loc1x = inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("x").toString();
			String loc1y = inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("y").toString();
			
			String[] line = fileContext.split("\n");
			List<String> lineoffile = Arrays.asList(line);
			List<String> newcontent = new ArrayList<String>();
			for (int r = 0; r < 36; r++) {
				newcontent.add(lineoffile.get(r));
			}
			
			newcontent.add("\"" +name1+"\"\n");//line 37
			for (int r = 37; r < 39; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(loc1y+" "+loc1x+"\n");//line 40
			for (int r = 40; r < 42; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add("0"+"\n");//line 43
			for (int r = 43; r < 45; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(proclowx+","+proclowy+"\n"); //line 46
			for (int r = 46; r < 48; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add("\""+epsgInUTM+"\"\n"); //line 49
			for (int r = 49; r < 51; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add("\""+epsgActive.substring(5,epsgActive.length())+"\"\n"); //line 52
			// just the EPSG number without EPSG
			for (int r = 52; r < 55; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(nx+","+ny+","+nz+","+"1,1,1\n"); //line 56
			for (int r = 56; r < 61; r++) {
				newcontent.add(lineoffile.get(r));
			}
			String line62=dcell[0]+","+dcell[1];
			String line62b="";
			for(int x=0;x<nz;x++) {
				line62b=line62b+","+dz;
			}
			newcontent.add(line62+line62b+"\n"); //line 62
			for (int r = 62; r < 93; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(gmttimedifference+"\n");//line 94 (if location=8)
			for (int r = 94; r < 96; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(time.split("-")[0] + "," + time.split("-")[1] + "," + time.split("-")[2].split("T")[0] + ","
			+ time.split("T")[1].split(":")[0]+"\n"); //line 97
			for (int r = 97; r < 99; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(time.split("-")[0] + "," + time.split("-")[1] + "," + time.split("-")[2].split("T")[0] + ","
					+ time.split("T")[1].split(":")[0]+"\n"); //line 100
			for (int r = 100; r < 125; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(lowerheight+" "+upperheight+"\n");//line 126 of zt low and zt high
			for (int r = 126; r < 465; r++) {
				newcontent.add(lineoffile.get(r));
			}
			//TODO initially assume the background concentration in control is zero for each concentration in line 466-487 ??
			for (int r = 465; r < 487; r++) {
				newcontent.add(lineoffile.get(r));
			}
			for (int r = 487; r < 496; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(restartindex+"\n"); //TODO initially should be zero in line 497 ;but for next simulation should be 1
			for (int r = 497; r < 501; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(line502); //TODO initially shouldn't see any previous result in line 502, next run should be changed
			for (int r = 502; r < 649; r++) {
				newcontent.add(lineoffile.get(r));
			}
			String segmentfraction="0.25";
			String line650=segmentfraction;
			for(int x=0;x<nz;x++) {
				line650=line650+","+segmentfraction;
			}
			newcontent.add(line650+"\n"); //line 650
			for (int r = 650; r < 715; r++) {
				newcontent.add(lineoffile.get(r));
			}
			
			String contentupdate = ""; // process from arraylist to string
			for (int x = 0; x < newcontent.size(); x++) {
				contentupdate += newcontent.get(x);
			}
			//contentupdate = contentupdate.replaceAll("\r\n", "\n");
			contentupdate = contentupdate.replaceAll("\\r", "\n");
			return contentupdate;

		} else if (filename.contentEquals("run_file.asc")) {
			File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/" + filename);
			fileContext = FileUtils.readFileToString(file);
			String timeinput=time.split("-")[0] + time.split("-")[1] + time.split("-")[2].split("T")[0];
			fileContext=fileContext.replaceAll("20191118_20191118", timeinput+"_"+timeinput);
			String name1 = inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("name").toString();
			String loc1x = inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("x").toString();
			String loc1y = inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("y").toString();
			//String heighttemp = inputparameter.getJSONArray("weatherinput").getJSONObject(0).get("z").toString();
			String heighttemp = "2.0";
			String name2 = inputparameter.getJSONArray("weatherinput").getJSONObject(1).get("name").toString();
			String loc2x = inputparameter.getJSONArray("weatherinput").getJSONObject(1).get("x").toString();
			String loc2y = inputparameter.getJSONArray("weatherinput").getJSONObject(1).get("y").toString();

			double[] p1convert = CRSTransformer.transform("EPSG:4326", epsgActive,
					new double[] { Double.valueOf(loc1x), Double.valueOf(loc1y) });
			double[] p2convert = CRSTransformer.transform("EPSG:4326", epsgActive,
					new double[] { Double.valueOf(loc2x), Double.valueOf(loc2y) });
			
			
			double dx1 = Math.abs((p1convert[0] - proclowx)/1000); //in km
			double dy1 = Math.abs((p1convert[1] - proclowy)/1000);//in km
			double dx2 = Math.abs((p2convert[0] - proclowx)/1000);//in km
			double dy2 = Math.abs((p2convert[1] - proclowy)/1000);//in km

			// System.out.println("line 0= "+filename);
			String[] line = fileContext.split("\n");
			List<String> lineoffile = Arrays.asList(line);
			List<String> newcontent = new ArrayList<String>();
			for (int r = 0; r < 23; r++) {
				newcontent.add(lineoffile.get(r));
			}

			double[] pmidconvert = CRSTransformer.transform(epsgActive, "EPSG:4326",
					new double[] { center[0], center[1] });
			DecimalFormat df2 = new DecimalFormat("#.#");
			String xmid = df2.format(pmidconvert[0]);
			System.out.println("xmid=" + xmid);
			String ymid = df2.format(pmidconvert[1]);
			System.out.println("ymid=" + ymid);
			String line23 = lineoffile.get(23);
			line23 = line23.replace("1.4", ymid);
			line23 = line23.replace("103.8", xmid);
			newcontent.add(line23);

			String line24 = lineoffile.get(24);
			line24 = line24.replace("-8", gmttimedifference);// change if timezone is different
			newcontent.add(line24);
			newcontent.add(lineoffile.get(25));
			String line26b = lineoffile.get(26).split("!")[1] + "!" + lineoffile.get(26).split("!")[2] + "!"
					+ lineoffile.get(26).split("!")[3];
			String line26 = " "+nx + " " + ny + " " + nz + separator + "!" + line26b;
			newcontent.add(line26);
			System.out.println("line26= " + line26);

			String line27b = lineoffile.get(27).split("!")[1];
			String line27 = " "+dcell[0] + " " + dcell[1] + separator + "!" + line27b;
			newcontent.add(line27);
			System.out.println("line27= " + line27);
			newcontent.add(lineoffile.get(28));// base with stretch factor
			newcontent.add(lineoffile.get(29));// base
			for (int r = 1; r < nz; r++) {
				newcontent.add(" "+dz + separator + "!" + "\n");
			}
			for (int r = 42; r < 73; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(" '"+name1 +"'"+ separator + "!" + lineoffile.get(73).split("!")[1]);
			newcontent.add(" "+dx1 + " " + dy1 + separator + "!" + lineoffile.get(74).split("!")[1]);
			newcontent.add(lineoffile.get(75));
			newcontent.add(" "+heighttemp + separator + "!" + lineoffile.get(76).split("!")[1]);
			newcontent.add(" "+upperheight + separator + "!" + lineoffile.get(77).split("!")[1]);
			newcontent.add(" "+lowerheight + separator + "!" + lineoffile.get(78).split("!")[1]);
			for (int r = 79; r < 82; r++) {
				newcontent.add(lineoffile.get(r));
			}
			newcontent.add(" '"+name2+"'" + separator + "!" + lineoffile.get(82).split("!")[1]);
			newcontent.add(" "+dx2 + " " + dy2 + separator + "!" + lineoffile.get(83).split("!")[1]);
			for (int r = 84; r < 89; r++) {
				newcontent.add(lineoffile.get(r));
			}
			// assume wind measurement height=const for both

			String contentupdate = ""; // process from arraylist to string
			for (int x = 0; x < newcontent.size(); x++) {
				contentupdate += newcontent.get(x);
			}
			contentupdate = contentupdate.replaceAll("\\r", "\n");
			return contentupdate;
		}

		return fileContext;
	}

	private File getInputFile(String datapath, String jobFolderName) throws IOException{
		//start to prepare all input files and put under folder input
		
		String inputFilePath="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\21afbf75-1d47-407e-9569-505c684f7385\\input.zip";
		//above just example
		
		inputFilePath=datapath.split("/input")[0]+"/input.zip";
		
		//zip all the input file expected (input.zip)
		
    	return new File(inputFilePath);
	}
	
	public String getNewJobFolderName(String hpcAddress, long timeStamp){
		return hpcAddress.concat("_").concat("" + timeStamp);
	}
	
	public String setUpJob(String jsonString,String datapath) throws IOException,  SlurmJobException{
    	String message = setUpJobOnAgentMachine(jsonString,datapath);
		JSONObject obj = new JSONObject();
		obj.put("message", message);
    	return obj.toString();
}
	
	private String setUpJobOnAgentMachine(String jsonInput, String datapath) throws IOException, SlurmJobException {
		initAgentProperty();
		long timeStamp = Utils.getTimeStamp();
		String jobFolderName = getNewJobFolderName(jobSubmission.slurmJobProperty.getHpcAddress(), timeStamp);
		return jobSubmission.setUpJob(jsonInput,
				new File(decodeURL(
						getClass().getClassLoader().getResource(jobSubmission.slurmJobProperty.getSlurmScriptFileName()).getPath())),
				getInputFile(datapath, jobFolderName),
				new File(decodeURL(
						getClass().getClassLoader().getResource(jobSubmission.slurmJobProperty.getExecutableFile()).getPath())),
				timeStamp);
	}
	
	public static File getZipFile(String folderName) throws IOException {
		
		
		Path sourceDirectory = Paths.get(folderName);		
		
		String zipFileName = folderName.concat(".zip");

		try {
			
		ZipOutputStream zipOutputStream = new ZipOutputStream (new FileOutputStream(zipFileName));
		
		Files.walkFileTree(sourceDirectory, new SimpleFileVisitor<Path>() {

			@Override
			public FileVisitResult visitFile(Path arg0, BasicFileAttributes arg1) throws IOException {
				
				try {
					
				Path destinationFile = sourceDirectory.relativize(arg0);
				
				zipOutputStream.putNextEntry(new ZipEntry(destinationFile.toString()));
				
				byte[] bytes = Files.readAllBytes(arg0);
				
				zipOutputStream.write(bytes, 0, bytes.length);
				
				zipOutputStream.closeEntry();
				
				}catch(IOException e) {
					
					e.printStackTrace();
				}
				
				// TODO Auto-generated method stub
				//return super.visitFile(arg0, arg1);
				
				return FileVisitResult.CONTINUE;
				
			}
		});
		
		zipOutputStream.flush();
		
		zipOutputStream.close();
		
		}catch(IOException e) {
			
			e.printStackTrace();
		}
		
		File zipFile = new File(zipFileName);
		
		return zipFile;
	
	}
	
	/**
	 * Takes a path that may contain one or more UTF-8 characters and decodes<br>
	 * these to regular characters. For example, the characters %23 and %2C are<br>
	 * decoded to hash(#) and comma (,), respectively.
	 * 
	 * @param path a path possibly containing encoded characters, e.g '#' is<br>
	 * encoded as %23 and space is encoded as %20.
	 * 
	 * @return path containing the decoded characters, e.g. %23 is converted<br>
	 * to '#' and %20 is converted to space.
	 * @throws UnsupportedEncodingException
	 */
	String decodeURL(String path) throws UnsupportedEncodingException{
		return URLDecoder.decode(path, "utf-8");
	}
}
