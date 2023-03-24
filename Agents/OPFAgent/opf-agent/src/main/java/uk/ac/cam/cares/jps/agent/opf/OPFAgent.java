package main.java.uk.ac.cam.cares.jps.agent.opf;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.sql.Connection;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import main.java.uk.ac.cam.cares.jps.agent.opf.utils.IriMapper;
import main.java.uk.ac.cam.cares.jps.agent.opf.utils.IriMapper.IriMapping;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet(urlPatterns = { "/startsimulationOPF" })

public class OPFAgent extends JPSAgent{
	private static final long serialVersionUID = -4199209974912271432L;
	private TimeSeriesClient<OffsetDateTime> tsClient;
	protected RemoteRDBStoreClient rdbStoreClient;
	protected RemoteStoreClient kbClient = new RemoteStoreClient();

	protected String dbUrl;
	protected String dbUsername;
	protected String dbPassword;
	protected String sparqlQueryEndpoint;
	protected String sparqlUpdateEndpoint;

    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(OPFAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(OPFAgent.class);

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		if (!validateInput(requestParams)) {
			throw new JSONException("");
		}

		String iriofnetwork = requestParams.getString("electricalnetwork");
		String baseMVA = requestParams.getString("baseMVA");
		String time = requestParams.getString("time");
		String baseUrl = AgentLocator.getCurrentJpsAppDirectory(this) + "/opf-result";
		String modeltype = "OPF";
		JSONObject result;

		try {
			result = startSimulationWithAccessAgent(iriofnetwork, baseUrl, modeltype, baseMVA, time);
			return result;
		} catch (IOException e) {
			throw new JPSRuntimeException("");
		}
	}

	/**
	 * Main method for OPF Agent.
	 * @param targetResourceID  URL of electrical network KG
	 * @param baseUrl           working directory
	 * @param modeltype         "OPF"
	 * @param baseMVA			baseMVA of the system
	 * @param time				
	 * @return
	 * @throws IOException
	 */
	public JSONObject startSimulationWithAccessAgent(String targetResourceID, String baseUrl, String modeltype, String baseMVA, String time) throws IOException {
		
		JSONObject resjo = new JSONObject();
		resjo.put("electricalnetwork", targetResourceID);
		
		logger.info("starting simulation for electrical network = " + targetResourceID + ", modeltype = " + modeltype + ", local data path=" + baseUrl);
		
		try {
			generateInputWithAccessAgent(targetResourceID, baseUrl, baseMVA, time);
		} catch (IOException e) {
			throw new JPSRuntimeException("Input generation failed.");
		}

		logger.info("running PyPower simulation");
		try {
			String[] fileNames = {"/baseMVA.txt", "/bus.txt", "/gen.txt", "/branch.txt", "/outputBusOPF.txt", 
								"/outputBranchOPF.txt", "/outputGenOPF.txt", "/areas.txt", "/genCost.txt", "/outputStatus.txt"};
			runPythonScript("PyPower-PF-OPF-JA-9-Java-2.py", baseUrl, fileNames);
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		String fileName = baseUrl + "/outputStatus.txt";
		BufferedReader input = new BufferedReader(new FileReader(fileName));
	    String last, line;
	    last = "";
	    while ((line = input.readLine()) != null) { 
	        last = line;
	    }
	    input.close();
		if (last.contains("Converged")) {
			resjo.put("status", "converged");	
			try {
				doConversionWithAccessAgent(targetResourceID, baseUrl, OffsetDateTime.parse(time), modeltype);
			} catch (IOException e) {
				throw new JPSRuntimeException("Output files not generated.\n");
			}		
		} else {
			resjo.put("status", "Diverged");
			//return null;
		}
		
		logger.info("finished simulation for electrical network = " + targetResourceID + ", modeltype = " + modeltype + ", local data path=" + baseUrl);
		return resjo;
	}

	/**
	 * Replace dataIRIs in busList with actual values of Pd and Qd.
	 * @param busList List of bus info
	 * @param time
	 */
	public List<String[]> processBusInput(List<String[]> busList, String time) {
		// extract dataIRIs for each bus
		List<String[]> busIRIs = new ArrayList<String[]>();
		String[] oneBus;
		for (int i = 0; i < busList.size(); i++) {
			oneBus = new String[2];
			oneBus[0] = busList.get(i)[2]; // PdIri of bus i
			oneBus[1] = busList.get(i)[3]; // QdIri of bus i
			busIRIs.add(oneBus);
		}

		try {
			loadTSClientConfigs(AgentLocator.getCurrentJpsAppDirectory(this) + "/config/client.properties");
		} catch (IOException e) {
			throw new JPSRuntimeException("Failed to read RDB info from config file.");
		}
		List<String[]> busLoadValues = readTimeSeriesData(busIRIs, OffsetDateTime.parse(time), busList.size());

		// replace dataIRIs with actual values
		for (int i = 0; i < busList.size(); i++) {
			busList.get(i)[2] = busLoadValues.get(i)[0];
			busList.get(i)[3] = busLoadValues.get(i)[1];
		}

		return busList;
	}

	/**
	 * Read bus Pd, Qd values of a given time.
	 * @param dataIRIs List of every bus's Pd and Qd dataIRIs
	 * @param time     
	 * @param numOfBus Total number of buses
	 * @return
	 */
	public List<String[]> readTimeSeriesData(List<String[]> dataIRIs, OffsetDateTime time, int numOfBus) {
		rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
		tsClient = new TimeSeriesClient<>(kbClient ,OffsetDateTime.class);
		List<String[]> busList = new ArrayList<String[]>();
		TimeSeries<OffsetDateTime> timeseries;
		String[] busValues;

		try (Connection conn = rdbStoreClient.getConnection()) {
			for (int i = 0; i < numOfBus; i++) {
				busValues = new String[2];
				timeseries = tsClient.getTimeSeriesWithinBounds(Arrays.asList(dataIRIs.get(i)), time, time, conn);
				busValues[0] = timeseries.getValuesAsString(dataIRIs.get(i)[0]).get(0); // Pd value of this bus
				busValues[1] = timeseries.getValuesAsString(dataIRIs.get(i)[1]).get(0); // Qd value of this bus
				busList.add(busValues);
			}
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }

		return busList;
	}

	/**
     * Reads the username, password, sparql endpoint from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the username, password, sparql endpoints
     */
    public void loadTSClientConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
		
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
			if (prop.containsKey("db.url")) {
                this.dbUrl = prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                this.dbUsername = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                this.dbPassword = prop.getProperty("db.password");
            } else {
                throw new IOException("Properties file is missing \"db.password=<db_password>\"");
            }
            if (prop.containsKey("sparql.query.endpoint")) {
				kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
	        } else {
	        	throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
	        }
	        if (prop.containsKey("sparql.update.endpoint")) {
	        	kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
	        } else {
	        	throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
	        }
        }
        }

	/**
	 * Generate input files by querying the electrical network graph with access agent.
	 * @param targetResourceID  targetResourceURL for access agent to query
	 * @param baseUrl 			Working directory
	 * @param baseMVA			baseMVA of the system
	 * @param time				
	 * @return
	 * @throws IOException
	 */
	public void generateInputWithAccessAgent(String targetResourceID, String baseUrl, String baseMVA, String time) throws IOException {
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "SELECT ?entity ?BusNumbervalue ?activepowervalue ?reactivepowervalue ?Qmaxvalue ?Qminvalue ?Vgvalue ?mBasevalue ?Statusvalue ?Pmaxvalue " 
		+ "?Pminvalue ?Pc1value ?Pc2value ?Qc1minvalue ?Qc1maxvalue ?Qc2minvalue ?Qc2maxvalue ?Rampagcvalue ?Ramp10value ?Ramp30value ?Rampqvalue ?apfvalue "

		+ "WHERE {?entity  a  j1:PowerGenerator  ."
		+ "?entity   j2:isModeledBy ?model ."

		+ "?model   j5:hasModelVariable ?num ." 
		+ "?num  a  j3:BusNumber  ." 
		+ "?num  j2:hasValue ?vnum ."
		+ "?vnum   j2:numericalValue ?BusNumbervalue ." // number

		+ "?model   j5:hasModelVariable ?Pg ." 
		+ "?Pg  a  j3:Pg  ." 
		+ "?Pg  j2:hasValue ?vpg ."
		+ "?vpg   j2:numericalValue ?activepowervalue ." // pg

		+ "?model   j5:hasModelVariable ?Qg ." 
		+ "?Qg  a  j3:Qg  ." 
		+ "?Qg  j2:hasValue ?vqg ."
		+ "?vqg   j2:numericalValue ?reactivepowervalue ." // qg

		+ "?model   j5:hasModelVariable ?qmax ." 
		+ "?qmax  a  j3:QMax  ." 
		+ "?qmax  j2:hasValue ?vqmax ."
		+ "?vqmax   j2:numericalValue ?Qmaxvalue ." // qmax

		+ "?model   j5:hasModelVariable ?qmin ." 
		+ "?qmin  a  j3:QMin  ." 
		+ "?qmin  j2:hasValue ?vqmin ."
		+ "?vqmin   j2:numericalValue ?Qminvalue ." // qmin

		+ "?model   j5:hasModelVariable ?Vg ." 
		+ "?Vg  a  j3:Vg  ." 
		+ "?Vg  j2:hasValue ?vVg ."
		+ "?vVg   j2:numericalValue ?Vgvalue ." // vg

		+ "?model   j5:hasModelVariable ?mbase ." 
		+ "?mbase  a  j3:mBase  ." 
		+ "?mbase  j2:hasValue ?vmbase ."
		+ "?vmbase   j2:numericalValue ?mBasevalue ." // mbase

		+ "?model   j5:hasModelVariable ?stat ." 
		+ "?stat  a  j3:Status ." 
		+ "?stat  j2:hasValue ?vstat ."
		+ "?vstat   j2:numericalValue ?Statusvalue ." // status

		+ "?model   j5:hasModelVariable ?pmax ." 
		+ "?pmax  a  j3:PMax  ." 
		+ "?pmax  j2:hasValue ?vpmax ."
		+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

		+ "?model   j5:hasModelVariable ?pmin ." 
		+ "?pmin  a  j3:PMin  ." 
		+ "?pmin  j2:hasValue ?vpmin ."
		+ "?vpmin   j2:numericalValue ?Pminvalue ." // pmin

		+ "?model   j5:hasModelVariable ?pc1 ." 
		+ "?pc1  a  j3:Pc1  ." 
		+ "?pc1  j2:hasValue ?vpc1 ."
		+ "?vpc1   j2:numericalValue ?Pc1value ." // pc1

		+ "?model   j5:hasModelVariable ?pc2 ." 
		+ "?pc2  a  j3:Pc2  ." 
		+ "?pc2  j2:hasValue ?vpc2 ."
		+ "?vpc2   j2:numericalValue ?Pc2value ." // pc2

		+ "?model   j5:hasModelVariable ?qc1min ." 
		+ "?qc1min  a  j3:QC1Min  ."
		+ "?qc1min  j2:hasValue ?vqc1min ." 
		+ "?vqc1min   j2:numericalValue ?Qc1minvalue ." // qc1min

		+ "?model   j5:hasModelVariable ?Qc1max ." 
		+ "?Qc1max  a  j3:QC1Max  ."
		+ "?Qc1max  j2:hasValue ?vQc1max ." 
		+ "?vQc1max   j2:numericalValue ?Qc1maxvalue ." // qc1max

		+ "?model   j5:hasModelVariable ?qc2min ." 
		+ "?qc2min  a  j3:QC2Min  ."
		+ "?qc2min  j2:hasValue ?vqc2min ."
		+ "?vqc2min   j2:numericalValue ?Qc2minvalue ." // qc2min

		+ "?model   j5:hasModelVariable ?Qc2max ."
		+ "?Qc2max  a  j3:QC2Max  ."
		+ "?Qc2max  j2:hasValue ?vQc2max ." 
		+ "?vQc2max   j2:numericalValue ?Qc2maxvalue ." // qc2max

		+ "?model   j5:hasModelVariable ?rampagc ." 
		+ "?rampagc  a  j3:Rampagc  ."
		+ "?rampagc  j2:hasValue ?vrampagc ." 
		+ "?vrampagc   j2:numericalValue ?Rampagcvalue ." // rampagc

		+ "?model   j5:hasModelVariable ?ramp10 ." 
		+ "?ramp10  a  j3:Ramp10  ."
		+ "?ramp10  j2:hasValue ?vramp10 ."
		+ "?vramp10   j2:numericalValue ?Ramp10value ." // ramp10

		+ "?model   j5:hasModelVariable ?ramp30 ." 
		+ "?ramp30  a  j3:Ramp30  ."
		+ "?ramp30  j2:hasValue ?vramp30 ." 
		+ "?vramp30   j2:numericalValue ?Ramp30value ." // ramp30

		+ "?model   j5:hasModelVariable ?rampq ." 
		+ "?rampq  a  j3:Rampq  ." 
		+ "?rampq  j2:hasValue ?vrampq ."
		+ "?vrampq   j2:numericalValue ?Rampqvalue ." // rampq

		+ "?model   j5:hasModelVariable ?apf ."
		+ "?apf  a  j3:APF  ." 
		+ "?apf  j2:hasValue ?vapf ."
		+ "?vapf   j2:numericalValue ?apfvalue ." // apf

		+ "}";

		String genCostInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "SELECT ?entity ?formatvalue ?startupcostvalue ?shutdowncostvalue ?gencostnvalue ?gencostn1value ?gencostn2value ?gencostcvalue "

		+ "WHERE {?entity  a  j1:PowerGenerator  ." 
		+ "?entity   j2:isModeledBy ?model ."

		+ "?model   j5:hasModelVariable ?format ." 
		+ "?format  a  j3:CostModel  ."
		+ "?format  j2:hasValue ?vformat ." 
		+ "?vformat  j2:numericalValue ?formatvalue ."

		+ "?model   j5:hasModelVariable ?startup ." 
		+ "?startup  a  j3:StartCost  ."
		+ "?startup  j2:hasValue ?vstartup ." 
		+ "?vstartup   j2:numericalValue ?startupcostvalue ."

		+ "?model   j5:hasModelVariable ?shutdown ." 
		+ "?shutdown  a  j3:StopCost  ."
		+ "?shutdown  j2:hasValue ?vshutdown ." 
		+ "?vshutdown   j2:numericalValue ?shutdowncostvalue ."

		+ "?model   j5:hasModelVariable ?gencostn ." 
		+ "?gencostn  a  j3:genCostn  ."
		+ "?gencostn  j2:hasValue ?vgencostn ." 
		+ "?vgencostn   j2:numericalValue ?gencostnvalue ."

		+ "?model   j5:hasModelVariable ?gencostn1 ." 
		+ "?gencostn1  a  j3:genCostcn-1  ."
		+ "?gencostn1  j2:hasValue ?vgencostn1 ." 
		+ "?vgencostn1   j2:numericalValue ?gencostn1value ."

		+ "?model   j5:hasModelVariable ?gencostn2 ." 
		+ "?gencostn2  a  j3:genCostcn-2  ."
		+ "?gencostn2  j2:hasValue ?vgencostn2 ." 
		+ "?vgencostn2   j2:numericalValue ?gencostn2value ."

		+ "?model   j5:hasModelVariable ?gencostc ." 
		+ "?gencostc  a  j3:genCostc0  ."
		+ "?gencostc  j2:hasValue ?vgencostc ." 
		+ "?vgencostc   j2:numericalValue ?gencostcvalue ." 

		+ "}";

		String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/> "
		+ "SELECT ?BusNumbervalue ?typevalue ?PdIri ?QdIri ?Gsvalue ?Bsvalue ?areavalue ?VoltMagvalue ?VoltAnglevalue ?BaseKVvalue ?Zonevalue ?VMaxvalue ?VMinvalue "

		+ "WHERE {?entity  a  j1:BusNode  ." 
		+ "?entity   j2:isModeledBy ?model ."
		+ "?model   j5:hasModelVariable ?num ." 
		+ "?num  a  j3:BusNumber  ." 
		+ "?num  j2:hasValue ?vnum ."
		+ "?vnum   j2:numericalValue ?BusNumbervalue ." // number

		+ "?model   j5:hasModelVariable ?type ." 
		+ "?type  a  j3:BusType  ." 
		+ "?type  j2:hasValue ?vtype ."
		+ "?vtype   j2:numericalValue ?typevalue ." // type

		+ "?entity  j6:hasActivePowerAbsorbed ?Pd ."
		+ "?Pd  a  j6:AbsorbedActivePower ." 
		+ "?Pd  om:hasValue ?PdIri ."  // dataIRI of Pd

		+ "?entity  j6:hasReactivePowerAbsorbed ?Qd ."
		+ "?Qd  a  j6:AbsorbedReactivePower ."
		+ "?Qd  om:hasValue ?QdIri ."  // dataIRI of Qd

		+ "?model   j5:hasModelVariable ?Gsvar ." 
		+ "?Gsvar  a  j3:Gs  ." 
		+ "?Gsvar  j2:hasValue ?vGsvar ."
		+ "?vGsvar   j2:numericalValue ?Gsvalue ." // Gs

		+ "?model   j5:hasModelVariable ?Bsvar ." 
		+ "?Bsvar  a  j3:Bs  ." 
		+ "?Bsvar  j2:hasValue ?vBsvar ."
		+ "?vBsvar   j2:numericalValue ?Bsvalue ." // Bs

		+ "?model   j5:hasModelVariable ?areavar ." 
		+ "?areavar  a  j3:Area  ."
		+ "?areavar  j2:hasValue ?vareavar ." 
		+ "?vareavar   j2:numericalValue ?areavalue ." // area

		+ "?model   j5:hasModelVariable ?VM ." 
		+ "?VM  a  j3:Vm  ." 
		+ "?VM  j2:hasValue ?vVM ."
		+ "?vVM   j2:numericalValue ?VoltMagvalue ." // Vm

		+ "?model   j5:hasModelVariable ?VA ." 
		+ "?VA  a  j3:Va  ." 
		+ "?VA  j2:hasValue ?vVA ."
		+ "?vVA   j2:numericalValue ?VoltAnglevalue ." // Va

		+ "?model   j5:hasModelVariable ?BKV ." 
		+ "?BKV  a  j3:baseKV  ." 
		+ "?BKV  j2:hasValue ?vBKV ."
		+ "?vBKV   j2:numericalValue ?BaseKVvalue ." // Base KV

		+ "?model   j5:hasModelVariable ?zvar ." 
		+ "?zvar  a  j3:Zone  ." 
		+ "?zvar  j2:hasValue ?vzvar ."
		+ "?vzvar   j2:numericalValue ?Zonevalue ." // Zone

		+ "?model   j5:hasModelVariable ?vmaxvar ." 
		+ "?vmaxvar  a  j3:VmMax  ."
		+ "?vmaxvar  j2:hasValue ?vvmaxvar ." 
		+ "?vvmaxvar   j2:numericalValue ?VMaxvalue ." // Vmax

		+ "?model   j5:hasModelVariable ?vminvar ." 
		+ "?vminvar  a  j3:VmMin  ."
		+ "?vminvar  j2:hasValue ?vvminvar ." 
		+ "?vvminvar   j2:numericalValue ?VMinvalue ." // Vmin

		+ "}";

		String branchInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "SELECT ?entity ?BusNumber1value ?BusNumber2value ?resistancevalue ?reactancevalue ?susceptancevalue ?rateAvalue ?rateBvalue " 
		+ "?rateCvalue ?ratiovalue ?anglevalue ?statusvalue ?angleminvalue ?anglemaxvalue "

		+ "WHERE {?entity  a  j1:UndergroundCable  ." 
		+ "?entity   j2:isModeledBy ?model ."
		+ "?model   j5:hasModelVariable ?num1 ." 
		+ "?num1  a  j3:BusFrom  ." 
		+ "?num1  j2:hasValue ?vnum1 ."
		+ "?vnum1   j2:numericalValue ?BusNumber1value ." // number 1

		+ "?model   j5:hasModelVariable ?num2 ." 
		+ "?num2  a  j3:BusTo  ." 
		+ "?num2  j2:hasValue ?vnum2 ."
		+ "?vnum2   j2:numericalValue ?BusNumber2value ." // number 2

		+ "?model   j5:hasModelVariable ?res ." 
		+ "?res  a  j3:R  ." 
		+ "?res  j2:hasValue ?vres ."
		+ "?vres   j2:numericalValue ?resistancevalue ." // resistance

		+ "?model   j5:hasModelVariable ?rea ." 
		+ "?rea  a  j3:X  ." 
		+ "?rea  j2:hasValue ?vrea ."
		+ "?vrea   j2:numericalValue ?reactancevalue ." // reactance

		+ "?model   j5:hasModelVariable ?sus ." 
		+ "?sus  a  j3:B  ." 
		+ "?sus  j2:hasValue ?vsus ."
		+ "?vsus   j2:numericalValue ?susceptancevalue ." // susceptance

		+ "?model   j5:hasModelVariable ?ratea ." 
		+ "?ratea  a  j3:RateA  ." 
		+ "?ratea  j2:hasValue ?vratea ."
		+ "?vratea   j2:numericalValue ?rateAvalue ." // rateA

		+ "?model   j5:hasModelVariable ?rateb ." 
		+ "?rateb  a  j3:RateB  ." 
		+ "?rateb  j2:hasValue ?vrateb ."
		+ "?vrateb   j2:numericalValue ?rateBvalue ." // rateB

		+ "?model   j5:hasModelVariable ?ratec ." 
		+ "?ratec  a  j3:RateC  ." 
		+ "?ratec  j2:hasValue ?vratec ."
		+ "?vratec   j2:numericalValue ?rateCvalue ." // rateC

		+ "?model   j5:hasModelVariable ?ratio ." 
		+ "?ratio  a  j3:RatioCoefficient  ."
		+ "?ratio  j2:hasValue ?vratio ." 
		+ "?vratio   j2:numericalValue ?ratiovalue ." // ratio

		+ "?model   j5:hasModelVariable ?ang ." 
		+ "?ang  a  j3:Angle  ." 
		+ "?ang  j2:hasValue ?vang ."
		+ "?vang   j2:numericalValue ?anglevalue ." // angle

		+ "?model   j5:hasModelVariable ?stat ." 
		+ "?stat  a  j3:BranchStatus ." 
		+ "?stat  j2:hasValue ?vstat ."
		+ "?vstat   j2:numericalValue ?statusvalue ." // status

		+ "?model   j5:hasModelVariable ?angmin ." 
		+ "?angmin  a  j3:AngleMin  ."
		+ "?angmin  j2:hasValue ?vangmin ." 
		+ "?vangmin   j2:numericalValue ?angleminvalue ." // anglemin

		+ "?model   j5:hasModelVariable ?angmax ." 
		+ "?angmax  a  j3:AngleMax  ."
		+ "?angmax  j2:hasValue ?vangmax ." 
		+ "?vangmax   j2:numericalValue ?anglemaxvalue ." // anglemax

		+ "}";

		String batteryInfo ="PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
		+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
		+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
		+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
		+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
		+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
		+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
		+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
		+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
		
		+ "SELECT DISTINCT ?entity ?V_ActivePowerInjection_of_VRB "
		+ "WHERE {?entity  a  j1:VanadiumRedoxBattery ."

		+ "?entity   j9:hasActivePowerInjection ?cap ."
		+ "?cap j2:hasValue ?vcap ."
		+ "?vcap  j2:numericalValue ?V_ActivePowerInjection_of_VRB ."
		+ " {?class rdfs:subClassOf j1:Battery ."
		+ "} "
		+ "UNION { ?class rdfs:subClassOf j1:EnergyStorageSystem . } ."
		+ "}";

		String busKeys[] = new String[] {"BusNumbervalue", "typevalue", "PdIri", "QdIri", "Gsvalue",
					 "Bsvalue", "areavalue", "VoltMagvalue", "VoltAnglevalue", "BaseKVvalue", "Zonevalue", "VMaxvalue", "VMinvalue"};
		String genKeys[] = new String[] {"entity", "BusNumbervalue", "activepowervalue", "reactivepowervalue", "Qmaxvalue", "Qminvalue",
					 "Vgvalue", "mBasevalue", "Statusvalue", "Pmaxvalue", "Pminvalue", "Pc1value", "Pc2value", "Qc1minvalue", "Qc1maxvalue",
					 "Qc2minvalue", "Qc2maxvalue", "Rampagcvalue", "Ramp10value", "Ramp30value", "Rampqvalue", "apfvalue"};
		String genCostKeys[] = new String[] {"entity", "formatvalue", "startupcostvalue", "shutdowncostvalue", "gencostnvalue", "gencostn1value",
					 "gencostn2value", "gencostcvalue"};
		String branchKeys[] = new String[] {"entity", "BusNumber1value", "BusNumber2value", "resistancevalue", "reactancevalue", "susceptancevalue",
					 "rateAvalue", "rateBvalue", "rateCvalue", "ratiovalue", "anglevalue", "statusvalue", "angleminvalue", "anglemaxvalue"};
		String batteryKeys[] = new String[] {"entity", "V_ActivePowerInjection_of_VRB"};

		List<String[]> busList = extractTripleInArray(targetResourceID, busInfo, busKeys, "bus", baseUrl);
		// Replace Pd and Qd dataIRIs with their actual values at the given time
		busList = processBusInput(busList, time);

		List<String[]> genList = extractTripleInArray(targetResourceID, genInfo, genKeys, "generator", baseUrl);
		List<String[]> genCostList = extractTripleInArray(targetResourceID, genCostInfo, genCostKeys, "generatorcost", baseUrl);
		List<String[]> branchList = extractTripleInArray(targetResourceID, branchInfo, branchKeys, "branch", baseUrl);
		List<String[]> batteryList = extractTripleInArray(targetResourceID, batteryInfo, batteryKeys, "battery", baseUrl);

		String content = createNewTSV(busList, baseUrl + "/mappingforbus.csv", baseUrl + "/mappingforbus.csv");
		FileUtil.writeFileLocally(baseUrl + "/bus.txt", content);

		content = createNewTSV(genList, baseUrl + "/mappingforgenerator.csv", baseUrl + "/mappingforbus.csv");
		//only add if battery is available. 	
		if (!batteryList.isEmpty()) {
			content += createDummyValueTSV(batteryList); 
		}
		FileUtil.writeFileLocally(baseUrl + "/gen.txt", content);
		
		content = createNewTSV(genCostList, baseUrl + "/mappingforgeneratorcost.csv", baseUrl + "/mappingforbus.csv");
		if (!batteryList.isEmpty()) {
			String[] dummyarray = new String[7];
			Arrays.fill(dummyarray, "0");
			for (int i = 0; i < batteryList.size(); i++) {
				String message = String.join("\t", dummyarray)+"\n";
				content += message;
			}
		}
		FileUtil.writeFileLocally(baseUrl + "/genCost.txt", content);

		content = createNewTSV(branchList, baseUrl + "/mappingforbranch.csv", baseUrl + "/mappingforbus.csv");
		FileUtil.writeFileLocally(baseUrl + "/branch.txt", content);

		FileUtil.writeFileLocally(baseUrl + "/baseMVA.txt", baseMVA);
	}

	/**
	 * Query triple store and store the return values in List<String[]> format.
	 * @param targetResourceID
	 * @param sparqlQuery
	 * @param keys    keys in JSON Array
	 * @param context bus/branch/generator
	 * @param baseUrl working directory
	 * @return
	 */
	public List<String[]> extractTripleInArray(String targetResourceID, String sparqlQuery, String[] keys, String context, String baseUrl) {
		JSONArray resultArray = callAccessAgentToQuery(targetResourceID, sparqlQuery);
		List<String[]> resultList = convertJSONArraytoList(resultArray, keys);
		resultList = generateMapping(resultList, keys, context, baseUrl);
		return resultList;
	}

	/**
	 * Convert JSONArray to list of string arrays.
	 * @param originalArray
	 * @param keys
	 * @return
	 */
	public List<String[]> convertJSONArraytoList(JSONArray originalArray, String[] keys) {
		List<String[]> resultList = new ArrayList<String[]>();

		for (int i = 0; i < originalArray.length(); i++) {
			String[] array = new String[keys.length];
			JSONObject row = originalArray.getJSONObject(i);
			for (int j = 0; j < keys.length; j++) {
				String value = row.optString(keys[j], null);
				array[j] = value;
			}
			resultList.add(array);
		}
		return resultList;
	}

	/**
	 * Generate mapping files.
	 * @param elementList
	 * @param keys
	 * @param context
	 * @param baseUrl
	 * @return
	 */
	public List<String[]> generateMapping (List<String[]> elementList, String[] keys, String context, String baseUrl) {
		String keysConcat = MiscUtil.concat(keys, ", ");
		StringBuffer b = new StringBuffer("context = ").append(context)
				.append(", keys = ").append(keys.length).append(", ").append(keysConcat);
		logger.info(b.toString());
		
		if (!context.toLowerCase().contains("output")) {
			/*
			 * special case 1. for bus, the mapper contains bus number instead of iri 
			 * case 2. for gencost no need mapper as it just read from the gen mapper
			 */
			IriMapper mapper = new IriMapper();
			for (int i = 0; i < elementList.size(); i++) {
				String[] current = elementList.get(i);
				String id = "" + (i + 1);
				mapper.add(current[0], id, context);
				// current[0]=id;// ??? no need to be there because it is not written in the tsv
			}

			String csv = mapper.serialize();
			FileUtil.writeFileLocally(baseUrl + "/mappingfor" + context + ".csv", csv);
		}
		return elementList;
	}

	/** 
	 * Load outputs from csv and store in KG/time series.
	 * @param targetResourceID
	 * @param baseUrl
	 * @param time
	 * @param modeltype
	 * @throws IOException
	 */
	public void doConversionWithAccessAgent(String targetResourceID, String baseUrl, OffsetDateTime time, String modeltype)
			throws IOException {
		logger.info("update bus entities in KG");
		ArrayList<String[]> busResult = readResult(baseUrl + "/outputBus" + modeltype.toUpperCase() + ".txt");

		// Update VoltMagnitude & VoltAngle in time series
		updateBusInfo(targetResourceID, busResult, time, baseUrl);
	}

	/**
	 * Update time series data (VoltageMagnitude & VoltageAngle) with OPF results.
	 * @param targetResourceID
	 * @param busValues 		OPF bus outputs
	 * @param time
	 * @param baseUrl
	 * @throws IOException
	 */
	public void updateBusInfo(String targetResourceID, List<String[]> busValues, OffsetDateTime time, String baseUrl) throws IOException {
		rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
		tsClient = new TimeSeriesClient<>(kbClient ,OffsetDateTime.class);

		List<OffsetDateTime> timeValues = new ArrayList<OffsetDateTime>();
		timeValues.add(time);
		List<List<?>> dataValues;
		List<TimeSeries<OffsetDateTime>> timeSeriesList = new ArrayList<>();

		for (int i = 0; i < busValues.size(); i++) {
			dataValues = new ArrayList<>();
			List<String> VmValues = new ArrayList<String>();
			List<String> VaValues = new ArrayList<String>();
			VmValues.add(busValues.get(i)[1]);
			VaValues.add(busValues.get(i)[2]);
			dataValues.add(VmValues);
			dataValues.add(VaValues);

			List<String> busLoadIRIs = queryBusOutputIRIs(targetResourceID, busValues.get(i), baseUrl);
			TimeSeries<OffsetDateTime> outputTimeSeries = new TimeSeries<OffsetDateTime>(timeValues, busLoadIRIs, dataValues);
			timeSeriesList.add(outputTimeSeries);
		}

		try (Connection conn = rdbStoreClient.getConnection()) {
			tsClient.bulkaddTimeSeriesData(timeSeriesList, conn);
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * Query the triple store for IRI of output timeseries data.
	 * @param targetResourceID
	 * @param busValues 		output bus values (of one bus)
	 * @param baseUrl
	 * @return
	 * @throws IOException
	 */
	public List<String> queryBusOutputIRIs(String targetResourceID, String[] busValues, String baseUrl) throws IOException {
		IriMapper map = new IriMapper();
		map.deserialize2(baseUrl + "/mappingforbus.csv");
		String originalBusNumber = map.getIri(busValues[0], "bus");

		String busQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/> "
				+ "SELECT ?VoltMagIRI ?VoltAngleIRI "
		
				+ "WHERE {?entity  a  j1:BusNode  ."
				+ "?entity   j2:isModeledBy ?model ."

				+ "?model   j5:hasModelVariable ?num ."
				+ "?num  a  j3:BusNumber  ."
				+ "?num  j2:hasValue ?vnum ." // busNumber
				+ "?vnum   j2:numericalValue \'" + originalBusNumber + "\' ."

				+ "?entity   j6:hasVoltageMagnitude ?VmResult ."
				+ "?VmResult  a  j6:VoltageMagnitude  ."
				+ "?VmResult  om:hasValue ?VoltMagIRI ." // Voltage Magnitude IRI

				+ "?entity   j6:hasVoltageAngle ?VaResult ."
				+ "?VaResult  a  j6:VoltageAngle  ."
				+ "?VaResult  om:hasValue ?VoltAngleIRI ." // Voltage Angle IRI

				+ "}";

		String busKeys[] = new String[] {"VoltMagIRI", "VoltAngleIRI"};
		JSONArray resultArray = callAccessAgentToQuery(targetResourceID, busQuery);
		List<String[]> busOutputIRIs = convertJSONArraytoList(resultArray, busKeys);
		return Arrays.asList(busOutputIRIs.get(0));
	}

	public JSONArray callAccessAgentToQuery (String targetResourceID, String sparqlQuery) {
		JSONArray queryResult = AccessAgentCaller.queryStore(targetResourceID, sparqlQuery);
		return queryResult;
	}
	
	public String createDummyValueTSV(List<String[]> activePower) throws IOException {
		String content = "";
		for (int i = 0; i < activePower.size(); i++) {
			String[] dummyarray = new String[21];
			Arrays.fill(dummyarray, "0");
			dummyarray[3] = activePower.get(i)[1].toString();
			String message = String.join("\t", dummyarray)+"\n";
			content += message;
	    }
		
		return content;
	}

	/**
	 * Prepare input file content based on information extracted from
	 * the triple store and mapping of components.
	 * @param componentlist
	 * @param mapdir	    mapping of this component
	 * @param mapdirbus     bus mapping
	 * @return
	 */
	public String createNewTSV(List<String[]> componentlist, String mapdir, String mapdirbus) {
		StringWriter writer = new StringWriter();
		try (BufferedWriter bw = new BufferedWriter(writer)) {
			int line = componentlist.size();
			IriMapper map2 = new IriMapper();
			List<IriMapping> original = map2.deserialize2(mapdir);
			List<IriMapping> originalforbus = map2.deserialize2(mapdirbus);// because the gen and branch input also
																			// depends on the bus number
																			// =baseUrl+"/mappingforbus.csv"
			for (int x = 0; x < line; x++) {
				int element = componentlist.get(x).length;
				for (int e = 0; e < element; e++) {

					if (mapdir.contains("bus") && e == 0) { // first condition is when the map is bus, it takes the 1st
															// element of busnumber and use the mapped id to be written
															// in tsv
						String content = componentlist.get(x)[e];
						String content2 = map2.getIDFromMap(original, content) + "\t";
						bw.write(content2);
					} else if (!mapdir.contains("bus") && e == 0) {
						// condition when the map is not bus, it ignores the 1st element ( entities real iri is not to be
						// written in tsv)
					}

					else if (e == element - 1) {// condition for every type in the last property, it will add the \n to
												// move to new row of the tsv
						String content = componentlist.get(x)[e] + "\n";
						bw.write(content);
					} else {// the rest element index (not the first and not the last

						if (e == 1 && mapdir.contains("mappingforgenerator.csv")&& !mapdir.contains("cost.csv")) { // condition that original bus
																					// number extracted from gen owl
																					// file need to be mapped to the
																					// mapped bus number

							String ori = componentlist.get(x)[e].replace(".", "x").split("x")[0]; // remove the decimal
																									// values queried
																									// from the kb in
																									// gen in the form
																									// of string
							String content2 = map2.getIDFromMap(originalforbus, ori) + "\t";
							bw.write(content2);
						}

						else if ((mapdir.contains("branch") && e == 1) || (mapdir.contains("branch") && e == 2)) {// condition that original 2 bus numbers
																												//	 extracted from branch owl file need to 
																												//	 be mapped to the mapped bus number

							String content = componentlist.get(x)[e];
							String content2 = map2.getIDFromMap(originalforbus, content) + "\t";
							bw.write(content2);

						} else if (mapdir.contains("bus") && e == 7) {
							double pu = Double.valueOf(componentlist.get(x)[e]);

							if (pu > 1.20000) {
								String basekv = componentlist.get(x)[9];
								pu = Double.valueOf(componentlist.get(x)[e]) / Double.valueOf(basekv);
							}
							String content = pu + "\t";
							bw.write(content);

						} else { // the rest normal condition
							String content = componentlist.get(x)[e] + "\t";
							bw.write(content);
						}
					}
				}
			}
			System.out.println("Done");
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		return writer.toString();
	}

	/** 
	 * Run Python Script in folder
	 * @param script
	 * @param folder    working directory
	 * @param fileNames input and output file names
	 * @return
	 * @throws Exception
	 */
	public String runPythonScript(String script, String folder, String[] fileNames) throws Exception {
		String result = "";
		String path = AgentLocator.getCurrentJpsAppDirectory(this);
		
		for (int i = 0; i < 4; i++) {
			if (!new File(folder + fileNames[i]).isFile()) {
				throw new JPSRuntimeException("Input file " + folder + fileNames[i] + " does not exist.\n");
			}
		}

		String command = "python3 " + path + "/python/model/" + script 
						+ " " + folder + fileNames[0] + " " + folder 
						+ fileNames[1] + " " + folder + fileNames[2] + " " 
						+ folder +fileNames[3] +" 1" + " " + folder 
						+ fileNames[4] + " " + folder 
						+ fileNames[5] + " " + folder 
						+ fileNames[6] + " 1" + " 1" 
						+ " " + folder + fileNames[7] + " " + folder 
						+ fileNames[8] + " " + folder + fileNames[9];
		System.out.println(command);
		result = CommandHelper.executeSingleCommand(path, command);
			
		return result;
	}

	/**
	 * Reads the result from the csv file produced and returns as List<String[]>
	 * @param outputFile
	 * @return
	 */
	public ArrayList<String[]> readResult(String outputFile) {
        String csv = FileUtil.readFileLocally(outputFile);
        ArrayList<String[]> simulationResult = new ArrayList<String[]> (fromCsvToArray(csv));
		
		return simulationResult;
	}

	/** 
	 * Cannot use MatrixConverter as they're separated by \t
	 * @param s
	 * @return
	 */
	public List<String[]> fromCsvToArray(String s) {
		List<String[]> result = new ArrayList<String[]>();		
		StringTokenizer tokenizer = new StringTokenizer(s, "\n");
		
		while (tokenizer.hasMoreTokens()) {
			String[] row = tokenizer.nextToken().split("\t");
			result.add(row);
		}
		return result;
	}

	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
    	if (requestParams.isEmpty()) {
            throw new BadRequestException("Empty request.");
        } else if (!requestParams.has("baseMVA")) {
			throw new JPSRuntimeException("Input should contain baseMVA.");
		} else if (!requestParams.has("electricalnetwork")) {
			throw new JPSRuntimeException("Input should contain electrical network IRI.");
		} else if (!requestParams.has("time")) {
			throw new JPSRuntimeException("Input should contain time interested.");
		}
		
		String time = requestParams.getString("time");
		try {
			OffsetDateTime.parse(time);
		} catch (DateTimeParseException | NullPointerException e) {
			throw new JPSRuntimeException("Invalid time format, expected format: YYYY-MM-DDTHH:mm:ss+00:00.");
		}

        try {
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);
	        return w;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        }
        return false;
    }
}
