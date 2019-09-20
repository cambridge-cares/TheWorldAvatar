package uk.ac.cam.cares.jps.ship;

import org.apache.jena.ontology.*;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.listener.LocalEntityManagerFactory;
import uk.ac.cam.cares.jps.ship.model.ShipEntity;

import javax.persistence.EntityManager;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;

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
    private Logger logger = LoggerFactory.getLogger(ShipAgent.class);

    private void savefile(OntModel jenaOwlModel, String iriOfChimney, String mmsi) throws IOException {

        String filePath2= iriOfChimney.replaceAll("http://www.theworldavatar.com/kb", "C:/TOMCAT/webapps/ROOT/kb").split("#")[0]; //update the file locally
        filePath2= filePath2.replaceAll("http://www.theworldavatar.com:80/JPS_SHIP/kb", "C:/TOMCAT/webapps/ROOT/kb").split("#")[0]; //update the file locally
        System.out.println("filepath2= " + filePath2);
        //http://www.theworldavatar.com/kb/ships/563041225/Chimney-1.owl

        File stockDir = new File(filePath2).getParentFile();

        boolean stockdir = true;

        if (!stockDir.exists()) {
            stockdir = stockDir.mkdirs();
        }
        if (stockdir) {
            File[] listOfFiles = stockDir.listFiles();
            if (listOfFiles != null) {
                for (File listOfFile : listOfFiles) {
                    if (!listOfFile.delete()) {
                        throw new IOException("Could not clean up: " + stockdir);
                    }
                }
            }
        } else {
            throw new IOException("No such directory: " + stockdir);
        }


        FileOutputStream out = new FileOutputStream(filePath2);

        jenaOwlModel.write(out, "RDF/XML-ABBREV");

        //linkChimneyToShip(iriOfChimney, mmsi);
    }

    /**
     * Creates chimney iri record in PostgreSQL with ship mmsi as foreign key.
     * @see uk.ac.cam.cares.jps.ship.model.ShipDetailsEntity
     * @param iriOfChimney Chimney IRI of ship identified by mmsi
     * @param mmsi Maritime Mobile Service Identity (ID of ship)
     */
    private void linkChimneyToShip(String iriOfChimney, String mmsi) {
        EntityManager em = LocalEntityManagerFactory.createEntityManager();
        ShipEntity ship = em.find(ShipEntity.class, Integer.decode(mmsi));
        ship.getShipPollutionByMmsi().setChimneyIri(iriOfChimney);
        em.getTransaction().begin();
        em.persist(ship);
        em.getTransaction().commit();
    }

    private void doConversion(OntModel jenaOwlModel, String iriofchimney, JSONObject jsonObject) throws JSONException {
        /*Adding elements to HashMap*/
        HashMap<String, String> hmap = new HashMap<>();
        hmap.put("CO", "ChemSpecies_Carbon__monoxide");
        hmap.put("CO2", "ChemSpecies_Carbon__dioxide");
        hmap.put("NO2", "ChemSpecies_Nitrogen__dioxide");
        hmap.put("HC", "PseudoComponent_Unburned_Hydrocarbon");
        hmap.put("NOx", "PseudoComponent_Nitrogen__oxides");
        hmap.put("SO2", "ChemSpecies_Sulfur__dioxide");
        hmap.put("O3", "ChemSpecies_Ozone");

        DatatypeProperty numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
        OntClass particleclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ParticulateMaterialAmount");
        OntClass flowclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");
        ObjectProperty hasProperty = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
        ObjectProperty hasValue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
        ObjectProperty hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
        OntClass scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
        ObjectProperty contains = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");
        OntClass partialparticleclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#SingleParticle");
        Individual gpers = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");
        OntClass diameterclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Diameter");
        OntClass densityclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Density");
        OntClass massfractionclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MassFraction");
        ObjectProperty has_density = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");
        ObjectProperty has_length = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");
        Individual m = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
        Individual kg_per_m3 = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");
        ObjectProperty hasRepresentativeParticle = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");


        //reset all the emission rate to be zero
        for (int b = 0; b < hmap.size(); b++) {
            String ks = hmap.get(hmap.keySet().toArray()[b].toString());
            Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + ks + "_EmissionRate");
            if (valueofspeciesemissionrate == null) {
            	throw new JPSRuntimeException("error that the emission rate not exist");
            }
            else {
            	System.out.println(valueofspeciesemissionrate+" is exist 1");
            	System.out.println(numval+" is exist 2");
            	//System.out.println(jenaOwlModel+" is exist");
            }
            logger.info("ks emission= "+iriofchimney);
            valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(Double.valueOf(0)));
        }

        Individual particleratevalue1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Particulate-001_EmissionRate");

        if (particleratevalue1 != null) {
            particleratevalue1.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(Double.valueOf(0)));
        }


        Double molecularvalue = jsonObject.getJSONObject("mixture").getJSONObject("molmass").getDouble("value") * 1000;
        Double Cpvalue = jsonObject.getJSONObject("mixture").getJSONObject("cp").getDouble("value");
        Double temperaturevalue = jsonObject.getJSONObject("mixture").getJSONObject("temperature").getDouble("value") - 273.15;
        Double massfluxvalue = jsonObject.getJSONObject("mixture").getJSONObject("massflux").getDouble("value"); //(multiplied by 100 temporarily to make it visible)
        Double densityvalue = jsonObject.getJSONObject("mixture").getJSONObject("density").getDouble("value");

        int valueoftotalpollutant = jsonObject.getJSONArray("pollutants").length();


        Individual valueofmassflowrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_massF_WasteStream-001");
        valueofmassflowrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(massfluxvalue));


        Individual valueofdensityrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Density_MaterialInWasteStream-001");
        valueofdensityrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(densityvalue));

        Individual valueoftemperature = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Temperature_MaterialInWasteStream-001");
        valueoftemperature.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(temperaturevalue));

        Individual valueofcombinedmolecularmass = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_ChemSpecies_Combined_MolecularMass");
        valueofcombinedmolecularmass.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(molecularvalue));

        Individual valueofcombinedheatcapacity = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Cp_MaterialInWasteStream-001");
        valueofcombinedheatcapacity.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(Cpvalue));


        Individual particulate1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Particulate-001");

        if (particulate1 == null) {
            Individual material2 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#GeneralizedAmount_WasteStreamOfChimney-1");
//			particulate1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] +"#Particulate-001",particleclass);//if it is not there	
            particulate1 = jenaOwlModel.createIndividual("http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Particulate-001", particleclass);//temp solution 2-8-19
            material2.addProperty(contains, particulate1);
        }
        Individual particulaterate1 = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Particulate-001_EmissionRate");

        if (particulaterate1 == null) {
            particulaterate1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#Particulate-001_EmissionRate", flowclass);
            particleratevalue1 = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_Particulate-001_EmissionRate", scalarvalueclass);
        }

        particulate1.addProperty(hasProperty, particulaterate1);


        particulaterate1.addProperty(hasValue, particleratevalue1);
        if (particleratevalue1 != null) {
            particleratevalue1.addProperty(hasunit, gpers);
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
            particleratevalue1.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(totalparticleemission));
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
                Individual partialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "OfParticulate-001");
                Individual diameterpartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "OfParticulate-001");
                Individual diametervaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "OfParticulate-001");
                Individual densitypartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "OfParticulate-001");
                Individual densityvaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "OfParticulate-001");
                Individual massfractionpartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "OfParticulate-001");
                Individual massfractionvaluepartialparticulate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "OfParticulate-001");
                if (partialparticulate == null) {
                    partialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "OfParticulate-001", partialparticleclass);
                    diameterpartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "OfParticulate-001", diameterclass);
                    diametervaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "OfParticulate-001", scalarvalueclass);
                    densitypartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "OfParticulate-001", densityclass);
                    densityvaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "OfParticulate-001", scalarvalueclass);
                    massfractionpartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "OfParticulate-001", massfractionclass);
                    massfractionvaluepartialparticulate = jenaOwlModel.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "OfParticulate-001", scalarvalueclass);
                }
            	System.out.println(particulate1+" is exist 4");
            	System.out.println(hasRepresentativeParticle+" is exist 5");
            	System.out.println(partialparticulate+" is exist 6");
                particulate1.addProperty(hasRepresentativeParticle, partialparticulate);


                partialparticulate.addProperty(has_length, diameterpartialparticulate);

                diameterpartialparticulate.addProperty(hasValue, diametervaluepartialparticulate);
                diametervaluepartialparticulate.addProperty(hasunit, m);
                diametervaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofdiameter * 0.000000001));


                partialparticulate.addProperty(has_density, densitypartialparticulate);

                densitypartialparticulate.addProperty(hasValue, densityvaluepartialparticulate);
                densityvaluepartialparticulate.addProperty(hasunit, kg_per_m3);
                densityvaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofdensity));


                partialparticulate.addProperty(hasProperty, massfractionpartialparticulate);

                massfractionpartialparticulate.addProperty(hasValue, massfractionvaluepartialparticulate);
                massfractionvaluepartialparticulate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(valueofmassfraction));
            }
        }
        double NO2value = 0;
        double NOvalue = 0;
        for (int b = 0; b < valueoftotalpollutant; b++) {
            String parametername = jsonObject.getJSONArray("pollutants").getJSONObject(b).getString("name");
            Double parametervalue = jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value") * 1000; //(multiplied by 100 temporarily to make it visible)

            if (hmap.get(parametername) != null) {
                Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(parametername) + "_EmissionRate");
                valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(parametervalue));
                if (parametername.contentEquals("NO2")) {
                    NO2value = jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value") * 1000;
                }
            } else if (parametername.contentEquals("NO")) {
                NOvalue = jsonObject.getJSONArray("pollutants").getJSONObject(b).getDouble("value") * 1000;
            }

        }
        Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + "_EmissionRate");
        valueofspeciesemissionrate.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(NO2value + NOvalue));

    }

    private void startConversion(OntModel jenaOwlModel, String iriOfChimney, JSONObject jsonresultstring, String mmsi) throws IOException {


        doConversion(jenaOwlModel, iriOfChimney, jsonresultstring);


        // save the updated model file

        savefile(jenaOwlModel, iriOfChimney, mmsi);


    }


    public ResultSet query(String sparql, OntModel model) {
        Query query = QueryFactory.create(sparql);
        QueryExecution queryExec = QueryExecutionFactory.create(query, model);
        ResultSet rs = queryExec.execSelect();

        return ResultSetFactory.copyResults(rs);
    }


    /**
     * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {

        JSONObject joforrec = AgentCaller.readJsonParameter(request);
        String baseURL = KeyValueManager.get(IKeys.URL_SCHEME) + KeyValueManager.get(IKeys.HOST)
                + ":" + KeyValueManager.get(IKeys.PORT) + "/JPS_SHIP";
        String shipKbURL = baseURL + KeyValueManager.get(IKeys.PATH_KNOWLEDGEBASE_SHIPS);
        shipKbURL= "http://www.theworldavatar.com/kb/ships/";
        String iri = null;
        String mmsi = null;

        try {
            iri = joforrec.optString("reactionmechanism");
            mmsi = joforrec.getJSONObject("ship").get("mmsi").toString(); //only get the mmsi instead of full iri
        } catch (JSONException e1) {
            e1.printStackTrace();
        }

        ArrayList<String> cpirilist2 = new ArrayList<>();

        OntModel jenaOwlModel = getModel(mmsi, shipKbURL);

        JSONObject jo = new JSONObject();


        cpirilist2.add(shipKbURL + "Engine-001.owl#Engine-001");


        String iriOfChimney = shipKbURL + mmsi + "/" + OWL_CHIMNEY + "#" + CHIMNEY;

        String wasteStreamInfo = "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> " +
                "PREFIX j7:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> " +
                "SELECT ?wasteStream \r\n" +
                "WHERE " +
                "{ ?chimney j7:hasOutput ?wasteStream ." +
                "  ?wasteStream a j4:NonReusableWasteProduct ." +
                "}";

        ResultSet rs_chimney = query(wasteStreamInfo, jenaOwlModel);
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

    private OntModel getModel(String mmsi, String shipKbURL) throws IOException {
        OntModel jenaOwlModel = ModelFactory.createOntologyModel();//OntModelSpec.OWL_MEM);
        String shipKbDir = KeyValueManager.get(IKeys.ABSDIR_ROOT) + KeyValueManager.get(IKeys.PATH_KNOWLEDGEBASE_SHIPS);

        File source = new File(shipKbDir + "/" + CHIMNEY + "-temp.owl");
        Charset charset = StandardCharsets.UTF_8;
        String content = new String(Files.readAllBytes(source.toPath()), charset);

        content = content.replaceAll("http://www.theworldavatar.com/kb/ships/" + OWL_CHIMNEY,
                shipKbURL + mmsi + "/" + OWL_CHIMNEY);
        //content = content.replaceAll(shipKbURL + OWL_CHIMNEY, shipKbURL + mmsi + "/" + OWL_CHIMNEY);
        byte[] contentBytes = content.getBytes(charset);

        jenaOwlModel.read(new ByteArrayInputStream(contentBytes), null);


        return jenaOwlModel;

    }
}


