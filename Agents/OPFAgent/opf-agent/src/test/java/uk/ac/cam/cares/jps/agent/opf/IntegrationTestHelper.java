package test.java.uk.ac.cam.cares.jps.agent.opf;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
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
					"	\"label\": \""+label+"\",\n"+
					"	\"queryEndpoint\": \""+endpoint+"\",\n"+
					"	\"updateEndpoint\": \""+endpoint+"\"\n"+
					"}\n"+
				"]";
	}

    public static TimeSeriesClient<OffsetDateTime> prepareTimeSeries(TimeSeriesClient<OffsetDateTime> tsClient) {
        List<String> dataIRIs = new ArrayList<String>();
        dataIRIs.add("http://www.sampleIRI.org/sample_IRI_Pd");
        dataIRIs.add("http://www.sampleIRI.org/sample_IRI_Qd");
        dataIRIs.add("http://www.sampleIRI.org/sample_IRI_solarPd");
        dataIRIs.add("http://www.sampleIRI.org/sample_IRI_Vm");
        dataIRIs.add("http://www.sampleIRI.org/sample_IRI_Va");
        List<Class<?>> classes = new ArrayList<>();
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);
        classes.add(String.class);

        tsClient.initTimeSeries(dataIRIs, classes, OffsetDateTime.class.getSimpleName());
        List<OffsetDateTime> timeValues = new ArrayList<OffsetDateTime>();
		timeValues.add(OffsetDateTime.parse("2020-01-01T08:00:00+00:00"));

        List<String> testIRIs = new ArrayList<>();
        testIRIs.add(dataIRIs.get(0));
        testIRIs.add(dataIRIs.get(1));
        testIRIs.add(dataIRIs.get(2));
        List<List<?>> values = new ArrayList<>();
        List<String> pdValue = new ArrayList<String>();
        List<String> qdValue = new ArrayList<String>();
        List<String> solarPdValue = new ArrayList<String>();

        pdValue.add("0");
        qdValue.add("0");
        solarPdValue.add("3");
        values.add(pdValue);
        values.add(qdValue);
        values.add(solarPdValue);

        TimeSeries<OffsetDateTime> testTimeSeries = new TimeSeries<>(timeValues, testIRIs, values);
        tsClient.addTimeSeriesData(testTimeSeries);
        return tsClient;
    }

    public static RemoteStoreClient populateRemoteStore(RemoteStoreClient targetStoreClient) {
        List<String> triples = new ArrayList<String>();
        triples = prepareInformation(triples);
        for (int i = 0; i < (73 + 74 + 86 + 28); i++) {
            // 73 bus triples, 74 branch triples, 86 gen triples, 28 genCost triples
            targetStoreClient.insert(null, triples.get(i), MediaType.APPLICATION_N_TRIPLES.type);
        }
        return targetStoreClient;
    }

    public static List<String> prepareInformation(List<String> triples) {
        ///////////////////////////////////////////////////////////////////////////////////////
        // (For Testing only)
        // Contains 1 bus, 1 generator and 1 branch
        // Cannot run OPF on it.
        ///////////////////////////////////////////////////////////////////////////////////////
        // Bus
        ///////////////////////////////////////////////////////////////////////////////////////
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

        // BusType
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#BusType_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#BusType_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusType>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#BusType_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_BusType_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_BusType_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'3'.");

        // Pd IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Pd_EBus-001> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://www.sampleIRI.org/sample_IRI_Pd>.");

        // Gd IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Gd_EBus-001> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://www.sampleIRI.org/sample_IRI_Qd>.");

        // Solar PV
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Building-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#Building>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Building-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#hasBusNode> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001>.");

        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Building-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#solarPV_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#solarPV_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PhotovoltaicPanel>.");

        // solarPd IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#solarPV_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#hasGeneratedPower> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#solarPd_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#solarPd_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#GeneratedPower>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#solarPd_EBus-001> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://www.sampleIRI.org/sample_IRI_solarPd>.");

        // Gs
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Gs_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Gs_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Gs>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Gs_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_Gs_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_Gs_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Bs
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Bs_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Bs_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Bs>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Bs_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_Bs_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_Bs_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Area
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Area_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Area_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Area>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Area_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_Area_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_Area_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // Vm
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Vm_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Vm_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Vm>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Vm_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_Vm_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_Vm_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1.0'.");

        // Va
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Va_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Va_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Va>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Va_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_Va_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_Va_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0.0'.");

        // BaseKV
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#BaseKV_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#BaseKV_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#baseKV>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#BaseKV_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_BaseKV_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_BaseKV_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'11'.");

        // Zone
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#Zone_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Zone_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Zone>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Zone_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_Zone_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_Zone_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // VmMax
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#VmMax_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VmMax_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMax>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VmMax_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMax_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMax_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // VmMin
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#VmMin_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VmMin_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMin>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VmMin_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMin_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_VmMin_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // Voltage Magnitude IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageMagnitude> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#VoltMag_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VoltMag_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageMagnitude>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VoltMag_EBus-001> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://www.sampleIRI.org/sample_IRI_Vm>.");

        // Voltage Angle IRI
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageAngle> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#VoltAngle_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VoltAngle_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageAngle>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#VoltAngle_EBus-001> " 
        + "<http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> " 
        + "<http://www.sampleIRI.org/sample_IRI_Va>.");

        // Output params
        // PdGen
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#PdGen_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#PdGen_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PdGen>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#PdGen_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_PdGen_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_PdGen_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");
        
        // GdGen
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#Model_Ebus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#GdGen_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#GdGen_EBus-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#GdGen>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#GdGen_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EBus-001.owl#V_GdGen_EBus-001>.");
        triples.add("<http://localhost:8080/powernetwork/EBus-001.owl#V_GdGen_EBus-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        /////////////////////////////////////////////////////////////////////////////////////////////////
        // Branch
        /////////////////////////////////////////////////////////////////////////////////////////////////
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#ELine-013> "
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> "
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#UndergroundCable>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy> "
        + "<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013>.");
        
        // Frombus
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#FromBusNumber_ELine-013> .");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#FromBusNumber_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusFrom>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#FromBusNumber_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_FromBusNumber_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_FromBusNumber_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // Tobus
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#ToBusNumber_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#ToBusNumber_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusTo>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#ToBusNumber_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_ToBusNumber_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_ToBusNumber_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");
        
        // R
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#R_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#R_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#R>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#R_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_R_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_R_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0.018436446'.");

        // X
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#X_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#X_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#X>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#X_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_X_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_X_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0.012435537'.");

        // B
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#B_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#B_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#B>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#B_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_B_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_B_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0.0'.");

        // rateA
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#RateA_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RateA_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RateA>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RateA_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_RateA_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_RateA_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'9900'.");

        // rateB
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#RateB_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RateB_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RateB>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RateB_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_RateB_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_RateB_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // rateC
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#RateC_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RateC_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RateC>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RateC_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_RateC_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_RateC_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Ratio coefficient
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#RatioCoeff_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RatioCoeff_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#RatioCoefficient>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#RatioCoeff_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_RatioCoeff_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_RatioCoeff_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Angle
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#Angle_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Angle_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Angle>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Angle_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_Angle_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_Angle_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Status
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#Status_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Status_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BranchStatus>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Status_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_Status_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_Status_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // AngleMin
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#AngleMin_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMin_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#AngleMin>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMin_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMin_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMin_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'-360'.");

        // AngleMax
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#AngleMax_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMax_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#AngleMax>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#AngleMax_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMax_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_AngleMax_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'360'.");

        // Output params
        // Ploss
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#PLoss_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#PLoss_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PLoss>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#PLoss_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_PLoss_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_PLoss_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Qloss
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#QLoss_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#QLoss_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QLoss>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#QLoss_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_QLoss_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_QLoss_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Paverage
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#PAverage_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#PAverage_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PAverage>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#PAverage_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_PAverage_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_PAverage_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Qaverage
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#QAverage_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#QAverage_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QAverage>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#QAverage_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_QAverage_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_QAverage_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");
  
        // Saverage
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#Model_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#SAverage_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#SAverage_ELine-013> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#SAverage>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#SAverage_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/ELine-013.owl#V_SAverage_ELine-013>.");
        triples.add("<http://localhost:8080/powernetwork/ELine-013.owl#V_SAverage_ELine-013> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        ///////////////////////////////////////////////////////////////////////////////////////////////
        // Generator
        ///////////////////////////////////////////////////////////////////////////////////////////////
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001>.");

        // busNumber
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#BusNumber_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#BusNumber_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#BusNumber_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_BusNumber_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_BusNumber_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // Pg
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#PGen_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#PGen_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pg>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#PGen_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_PGen_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_PGen_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0.0'.");

        // Qg
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#QGen_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QGen_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Qg>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QGen_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_QGen_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_QGen_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0.0'.");

        // Qmax
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Qmax_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Qmax_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QMax>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Qmax_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmax_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmax_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'10.0'.");

        // Qmin
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Qmin_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Qmin_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QMin>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Qmin_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmin_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Qmin_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'-10.0'.");

        // Vg
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Vg_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Vg_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Vg>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Vg_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Vg_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Vg_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // mBase
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#mBase_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#mBase_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#mBase>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#mBase_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_mBase_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_mBase_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'100'.");

        // Status
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Status_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Status_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Status>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Status_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Status_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Status_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'1'.");

        // Pmax
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#PMax_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#PMax_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PMax>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#PMax_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_PMax_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_PMax_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'10.0'.");

        // Pmin
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#PMin_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#PMin_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PMin>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#PMin_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_PMin_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_PMin_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Pc1
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Pc1_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Pc1_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pc1>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Pc1_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc1_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc1_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Pc2
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Pc2_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Pc2_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pc2>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Pc2_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc2_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Pc2_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Qc1min
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#QC1Min_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Min_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC1Min>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Min_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Min_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Min_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");
        
        // Qc1max
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#QC1Max_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Max_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC1Max>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC1Max_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Max_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC1Max_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Qc2min
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#QC2Min_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Min_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC2Min>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Min_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Min_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Min_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Qc2max
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#QC2Max_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Max_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#QC2Max>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#QC2Max_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Max_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_QC2Max_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Rampagc
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Rampagc_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Rampagc_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Rampagc>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Rampagc_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampagc_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampagc_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Ramp10
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Ramp10_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp10_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Ramp10>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp10_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp10_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp10_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Ramp30
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Ramp30_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp30_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Ramp30>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Ramp30_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp30_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Ramp30_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Rampq
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Rampq_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Rampq_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Rampq>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Rampq_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampq_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Rampq_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // APF
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#APF_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#APF_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#APF>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#APF_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_APF_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_APF_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        ///////////////////////////////////////////////////////////////////////////////////////////////////
        // Generator Cost
        ///////////////////////////////////////////////////////////////////////////////////////////////////
        // Cost Model
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#Format_CostEq_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Format_CostEq_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#CostModel>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Format_CostEq_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_Format_CostEq_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_Format_CostEq_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'2'.");

        // Startup cost
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#StartupCost_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#StartupCost_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#StartCost>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#StartupCost_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_StartupCost_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_StartupCost_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // Shutdown cost
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#ShutdownCost_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#ShutdownCost_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#StopCost>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#ShutdownCost_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_ShutdownCost_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_ShutdownCost_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // genCostn
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#genCostn_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostn_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostn>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostn_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostn_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostn_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'3'.");

        // genCostcn-1
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-1_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-1_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostcn-1>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-1_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-1_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-1_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        // genCostcn-2
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-2_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-2_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostcn-2>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostcn-2_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-2_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostcn-2_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'20'.");

        // genCostC0
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#Model_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#genCostc0_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostc0_EGen-001> " 
        + "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> " 
        + "<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#genCostc0>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#genCostc0_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> " 
        + "<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostc0_EGen-001>.");
        triples.add("<http://localhost:8080/powernetwork/EGen-001.owl#V_genCostc0_EGen-001> " 
        + "<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " 
        + "'0'.");

        return triples;
    }
}
