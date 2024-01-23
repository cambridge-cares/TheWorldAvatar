package uk.ac.cam.cares.jps.agent.smartmeteragent;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class IntegrationTestHelper {

    public static String uploadRoutingData(String label, String endpoint, String uploadUrl) {
		
		String routingData = getRoutingData(label, endpoint);
				
		HttpPost request = Http.post(uploadUrl, routingData, MediaType.APPLICATION_JSON.type, null);
		String result = Http.execute(request);	
		
		return new JSONObject(result).getString("result");
	}

    public static String getRoutingData(String label, String endpoint) {
		return "[\n"+
					"{\n"+
					"	\"label\": \"" + label + "\",\n"+
					"	\"queryEndpoint\": \"" + endpoint + "\",\n" +
					"	\"updateEndpoint\": \"" + endpoint + "\"\n" +
					"}\n"+
				"]";
	}

    public static TimeSeriesClient<OffsetDateTime> prepareTimeSeries(TimeSeriesClient<OffsetDateTime> tsClient) {
        // Instantiate time series
        List<String> dataIRIs = new ArrayList<String>();
        dataIRIs.add("http://example.com/device1pd");
        dataIRIs.add("http://example.com/device1qd");
        dataIRIs.add("http://example.com/device2pd");
        dataIRIs.add("http://example.com/device2qd");
        dataIRIs.add("http://example.com/device2current");
        dataIRIs.add("http://example.com/device2voltage");
        dataIRIs.add("http://example.com/device2frequency");
        List<Class<?>> classes = new ArrayList<>();
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);

        tsClient.initTimeSeries(dataIRIs, classes, OffsetDateTime.class.getSimpleName());
        return tsClient;
    }

    public static RemoteStoreClient populateRemoteStore(RemoteStoreClient targetStoreClient) {
        List<String> triples = new ArrayList<String>();
        triples = prepareInformation(triples);
        for (int i = 0; i < 30; i++) {
            // 12 triples for bus 1, 21 triples for bus 2 
            // (bus 2 has current, voltage and frequency instantiated)
            targetStoreClient.insert(null, triples.get(i), MediaType.APPLICATION_N_TRIPLES.type);
        }
        return targetStoreClient;
    }
    
    // Example readings for testing
    // For time: 2022-10-26 18:23:00, invalid reading (not all devices are here)
    protected static String smartMeterReading1 = "1,2022-10-26 18:23:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device1\n";
    protected static String smartMeterReading2 = "2,2022-10-26 18:23:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,ExtraDevice\n";
    // For time: 2022-10-26 18:24:00, valid reading
    protected static String smartMeterReading3 = "3,2022-10-26 18:24:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device1\n";
    protected static String smartMeterReading4 = "4,2022-10-26 18:24:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.122116,0.118081,0.11309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device2\n";
    // For time: 2022-10-26 18:25:00, valid reading
    protected static String smartMeterReading5 = "5,2022-10-26 18:25:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device1\n";
    protected static String smartMeterReading6 = "6,2022-10-26 18:25:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device2\n";
    // For time: 2022-10-26 18:26:00, invalid reading (important value is empty (ch2 pd is null))
    protected static String smartMeterReading7 = "7,2022-10-26 18:25:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device1\n";
    protected static String smartMeterReading8 = "8,2022-10-26 18:25:58.000000,0,0,POWERMETER,2022-10-26 18:23:58.000000," 
                                        + "0.322116,0.318081,0.31309,,,1.448439,1.426094,1.406155,223.604309,224.251617," 
                                        + "224.19278,,,,387.620453,388.752472,387.646515,49.999737,49.999737,49.999737," 
                                        + "0.0,0.0,0.0,0.0,0.0,0.0,device2\n";

    public static List<String> prepareInformation(List<String> triples) {
        // bus 1
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> "
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> "
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy> "
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001>.");

        // BusNumber
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> "
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> "
        + "<http://localhost:8080/powernetwork/EBus-001.owl#BusNumber_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#BusNumber_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#BusNumber_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_BusNumber_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_BusNumber_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // Pd IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://example.com/device1pd>.");
        
        // Qd IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://example.com/device1qd>.");

        // bus 2
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002> "
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> "
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy> "
        + "<http://localhost:8080/powernetwork/EBus-002.owl#Model_Ebus-002>.");
       
        // BusNumber
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Model_Ebus-002> "
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> "
        + "<http://localhost:8080/powernetwork/EBus-002.owl#BusNumber_EBus-002>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#BusNumber_EBus-002> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#BusNumber_EBus-002> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-002.owl#V_BusNumber_EBus-002>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#V_BusNumber_EBus-002> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'2'.");
       
        // Pd IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed> " 
        + "<http://localhost:8080/powernetwork/EBus-002.owl#Pd_EBus-002>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Pd_EBus-002> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Pd_EBus-002> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://example.com/device2pd>.");
               
        // Qd IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed> " 
        + "<http://localhost:8080/powernetwork/EBus-002.owl#Gd_EBus-002>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Gd_EBus-002> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Gd_EBus-002> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://example.com/device2qd>.");

        // device 2 has current & voltage IRIs
        // Current IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasCurrent> " 
        + "<http://localhost:8080/powernetwork/EBus-002.owl#Current_EBus-002>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Current_EBus-002> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#Current>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Current_EBus-002> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://example.com/device2current>.");
        
        // Voltage IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Ebus-002> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActualVoltage> " 
        + "<http://localhost:8080/powernetwork/EBus-002.owl#Voltage_EBus-002>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Voltage_EBus-002> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#Voltage>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-002.owl#Voltage_EBus-002> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://example.com/device2voltage>.");

        return triples;
    }
}
