package uk.ac.cam.cares.jps.ship;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.listener.LocalOntologyModelManager;

/**
 * Servlet implementation class ShipAgent
 * <p>
 * 4/7 2019 : the iri of the engine and chimney are HARDCODED
 */
@WebServlet("/ShipAgent")
public class ShipAgent extends HttpServlet {

    private static final long serialVersionUID = 1L;
    private static final String CHIMNEY = "Chimney-1";
    private static final String OWL_CHIMNEY = CHIMNEY + ".owl";
    private static final String P001 = "Particulate-001";
    private static final String EM_RATE = "_EmissionRate";

    private Logger logger = LoggerFactory.getLogger(ShipAgent.class);

    private void doConversion(OntModel jenaOwlModel, String iriofchimney, JSONObject jsonObject) throws JSONException {
        Map hmap = LocalOntologyModelManager.getSpeciesMap();

        //reset all the emission rate to be zero
        for (int b = 0; b < hmap.size(); b++) {
            String ks = (String) hmap.get(hmap.keySet().toArray()[b].toString());
            Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + ks + EM_RATE);
            valueofspeciesemissionrate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                    jenaOwlModel.createTypedLiteral(Double.valueOf(0)));
        }

        Individual particleratevalue1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE);

        if (particleratevalue1 != null) {
            particleratevalue1.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                    jenaOwlModel.createTypedLiteral(Double.valueOf(0)));
        }

        JSONObject mixture = jsonObject.getJSONObject("mixture");
        Double molecularvalue = mixture.getJSONObject("molmass").getDouble("value") * 1000;
        Double Cpvalue = mixture.getJSONObject("cp").getDouble("value");
        Double temperaturevalue = mixture.getJSONObject("temperature").getDouble("value") - 273.15;
        Double massfluxvalue = mixture.getJSONObject("massflux").getDouble("value"); //(multiplied by 100 temporarily to make it visible)
        Double densityvalue = mixture.getJSONObject("density").getDouble("value");
        int valueoftotalpollutant = jsonObject.getJSONArray("pollutants").length();


        Individual valueofmassflowrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_massF_WasteStream-001");
        valueofmassflowrate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                jenaOwlModel.createTypedLiteral(massfluxvalue));


        Individual valueofdensityrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Density_MaterialInWasteStream-001");
        valueofdensityrate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                jenaOwlModel.createTypedLiteral(densityvalue));

        Individual valueoftemperature = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Temperature_MaterialInWasteStream-001");
        valueoftemperature.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                jenaOwlModel.createTypedLiteral(temperaturevalue));

        Individual valueofcombinedmolecularmass = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_ChemSpecies_Combined_MolecularMass");
        valueofcombinedmolecularmass.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                jenaOwlModel.createTypedLiteral(molecularvalue));

        Individual valueofcombinedheatcapacity = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Cp_MaterialInWasteStream-001");
        valueofcombinedheatcapacity.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                jenaOwlModel.createTypedLiteral(Cpvalue));


        Individual particulate1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#" + P001);

        if (particulate1 == null) {
            Individual material2 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#GeneralizedAmount_WasteStreamOfChimney-1");
//			particulate1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#Particulate-001",particleclass);//if it is not there	
            particulate1 = jenaOwlModel.createIndividual("http://www.theworldavatar.com/kb/ships/Chimney-1.owl#" + P001,
                    LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_PMAMT));//temp solution 2-8-19
            material2.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_CONTAINS), particulate1);
        }
        Individual particulaterate1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#" + P001 + EM_RATE);

        if (particulaterate1 == null) {
            particulaterate1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#" + P001 + EM_RATE,
                    LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_CONVMFLR));
            particleratevalue1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE,
                    LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SCLVAL));
        }

        particulate1.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP), particulaterate1);


        particulaterate1.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL), particleratevalue1);
        if (particleratevalue1 != null) {
            particleratevalue1.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM),
                    LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_GPS));
        }

        int valueoftotalparticlesincluded = jsonObject.getJSONArray("particle").length();

        //------------------------------this part for seeing the correct format and version of json----------------------------------------
        double totalparticleemission = 0.0;
        for (int a = 0; a < valueoftotalparticlesincluded; a++) {
            if (jsonObject.getJSONArray("particle").getJSONObject(a).has("emission_rate")) {
                double valueofparticlerate = jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("emission_rate").getDouble("value");
                totalparticleemission = totalparticleemission + valueofparticlerate;
            }
        }
        if (particleratevalue1 != null) {
            particleratevalue1.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                    jenaOwlModel.createTypedLiteral(totalparticleemission*1000)); //to be in g/s instead kg/s
        }
        //---------------------------------------------------------------------------------------------------------------------------------

        for (int a = 0; a < valueoftotalparticlesincluded; a++) {
            String valueofmassfraction;
            if (jsonObject.getJSONArray("particle").getJSONObject(a).has("mass_fraction")) {
                valueofmassfraction = String.valueOf(jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("mass_fraction").get("value"));
            } else {
                double valueofparticlerate = jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("emission_rate").getDouble("value");
                valueofmassfraction = String.valueOf(valueofparticlerate / totalparticleemission);
            }
            double valueofdiameter = jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("diameter").getDouble("value");
            int valueofdensity = jsonObject.getJSONArray("particle").getJSONObject(a).getJSONObject("density").getInt("value");


            if (Double.parseDouble(valueofmassfraction) > 0) { //maybe later changed to emission rate
                Individual partialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" + P001);
                Individual diameterpartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001);
                Individual diametervaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
                Individual densitypartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001);
                Individual densityvaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
                Individual massfractionpartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001);
                Individual massfractionvaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);
                if (partialparticulate == null) {
                    partialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" + P001,
                            LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SINGPART));
                    diameterpartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001,
                            LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_DIAM));
                    diametervaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001,
                            LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SCLVAL));
                    densitypartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001,
                            LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_DENS));
                    densityvaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001,
                            LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SCLVAL));
                    massfractionpartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001,
                            LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_MASSFR));
                    massfractionvaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001,
                            LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SCLVAL));
                }
                particulate1.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASREPRPART), partialparticulate);


                partialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN),
                        diameterpartialparticulate);

                diameterpartialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL),
                        diametervaluepartialparticulate);
                diametervaluepartialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM),
                        LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M));
                diametervaluepartialparticulate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                        jenaOwlModel.createTypedLiteral(valueofdiameter * 0.000000001));


                partialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS),
                        densitypartialparticulate);

                densitypartialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL),
                        densityvaluepartialparticulate);
                densityvaluepartialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM),
                        LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM));
                densityvaluepartialparticulate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                        jenaOwlModel.createTypedLiteral(valueofdensity));


                partialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP),
                        massfractionpartialparticulate);

                massfractionpartialparticulate.addProperty((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL),
                        massfractionvaluepartialparticulate);
                massfractionvaluepartialparticulate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                        jenaOwlModel.createTypedLiteral(valueofmassfraction));
            }
        }
        double NO2value = 0;
        double NOvalue = 0;
        for (int b = 0; b < valueoftotalpollutant; b++) {
            String parametername = jsonObject.getJSONArray("pollutants").getJSONObject(b).getString("name");
            Double parametervalue = jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value") * 1000; //(multiplied by 100 temporarily to make it visible)

            if (hmap.get(parametername) != null) {
                Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(parametername) + EM_RATE);
                valueofspeciesemissionrate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                        jenaOwlModel.createTypedLiteral(parametervalue));
                if (parametername.contentEquals("NO2")) {
                    NO2value = jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value") * 1000;
                }
            } else if (parametername.contentEquals("NO")) {
                NOvalue = jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value") * 1000;
            }

        }
        Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + EM_RATE);
        valueofspeciesemissionrate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                jenaOwlModel.createTypedLiteral(NO2value + NOvalue));
        
        //adjust so that NO2 value is 5% of NOx (which is NO2 +NO)
//        double NO2valueNew=0.05*(NO2value+NOvalue);
//        Individual valueofspeciesemissionrateNO2 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NO2") + EM_RATE);
//        valueofspeciesemissionrateNO2.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
//                jenaOwlModel.createTypedLiteral(NO2valueNew));

    }

    private void startConversion(OntModel jenaOwlModel, String iriOfChimney, JSONObject jsonresultstring, String mmsi) throws IOException {

        doConversion(jenaOwlModel, iriOfChimney, jsonresultstring);
        // save the updated model
        LocalOntologyModelManager.save(jenaOwlModel, iriOfChimney, mmsi);

    }

    /**
     * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {

        JSONObject joforrec = AgentCaller.readJsonParameter(request);
        String baseURL = KeyValueManager.get(IKeys.URL_SCHEME) + KeyValueManager.get(IKeys.HOST);
        if ((AgentLocator.isJPSRunningForTest())) {
        	baseURL = KeyValueManager.get(IKeys.URL_SCHEME) + KeyValueManager.get(IKeys.HOST)+":"+KeyValueManager.get(IKeys.PORT);
        }
        String shipKbURL = baseURL + KeyValueManager.get(IKeys.PATH_KNOWLEDGEBASE_SHIPS);
        String iri = null;
        String mmsi = null;

        try {
            iri = joforrec.optString("reactionmechanism");
            mmsi = joforrec.getJSONObject("ship").get("mmsi").toString(); //only get the mmsi instead of full iri
        } catch (JSONException e1) {
            e1.printStackTrace();
        }

        ArrayList<String> cpirilist2 = new ArrayList<>();

        OntModel jenaOwlModel = LocalOntologyModelManager.createChimneyModelForMMSI(mmsi);//getModel(mmsi, shipKbURL);
        ResultSet rs_chimney = LocalOntologyModelManager.getWasteStreamInfo(jenaOwlModel);

        JSONObject jo = new JSONObject();
        cpirilist2.add(shipKbURL + "Engine-001.owl#Engine-001");
        String iriOfChimney = shipKbURL + mmsi + "/" + OWL_CHIMNEY + "#" + CHIMNEY;
        String wasteStreamIRI = null;

        for (; rs_chimney.hasNext(); ) {
            QuerySolution qs_p = rs_chimney.nextSolution();
            Resource cpiri = qs_p.getResource("wasteStream");
            wasteStreamIRI = cpiri.toString();
        }


        try {
            jo = new JSONObject().put("waste", wasteStreamIRI); // waste stream iri
        } catch (JSONException e) {
            e.printStackTrace();
        }


        //send the info to SRM Engine Agent
        JSONObject dataSet = new JSONObject();
        try {
            dataSet.put("reactionmechanism", iri);
            dataSet.put("engine", cpirilist2.get(0));
            dataSet.put("source", "ship");
            String resultjson;
            JSONObject jsonsrmresult = new JSONObject();

            // if there is no reaction mechanism
            if (iri != null) {
                if (!iri.contains("theworldavatar")) {
                    JSONObject jo2 = new JSONObject();
                    jo2.put("speed", joforrec.getJSONObject("ship").getDouble("ss"));
                    jo2.put("type", joforrec.getJSONObject("ship").get("type").toString().replace("+", " "));
                    resultjson = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SLMAgent", jo2.toString());
                    jsonsrmresult = new JSONObject(resultjson);
                } else {
                    resultjson = AgentCaller.executeGet("JPS/SRMAgent", "query", dataSet.toString());
                    jsonsrmresult = new JSONObject(resultjson).getJSONObject("results");
                }
            }

            startConversion(jenaOwlModel, iriOfChimney, jsonsrmresult, mmsi);

        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        } //convert to update value


        logger.info("message to sent back from shipagent = " + jo.toString());
        response.getWriter().write(jo.toString());


        cpirilist2.clear();

    }

}


