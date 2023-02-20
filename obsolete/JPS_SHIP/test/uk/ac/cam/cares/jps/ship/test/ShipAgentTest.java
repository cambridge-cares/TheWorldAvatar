package uk.ac.cam.cares.jps.ship.test;


import org.apache.jena.ontology.*;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.rdf.model.*;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;


import org.mockito.stubbing.Answer;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.listener.BaseChimneyOntologyModelManager;
import uk.ac.cam.cares.jps.base.listener.BaseOntologyModelManager;
import uk.ac.cam.cares.jps.ship.ShipAgent;
import uk.ac.cam.cares.jps.ship.listener.LocalOntologyModelManager;
import org.mockito.*;

import javax.ws.rs.BadRequestException;


import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.io.*;

import java.util.*;


public class ShipAgentTest {

    private static final String CHIMNEY = "Chimney-1";
    private static final String OWL_CHIMNEY = CHIMNEY + ".owl";
    private static final String P001 = "Particulate-001";
    private static final String EM_RATE = "_EmissionRate";


    @Test
    public void testCheckReactionMechanism() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ShipAgent shipAgent= new ShipAgent();
        Method checkReactionMechanism=shipAgent.getClass().getDeclaredMethod("checkReactionMechanism", JSONObject.class);
        checkReactionMechanism.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkReactionMechanism.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        requestParams.put("reactionmechanism","");
        val=checkReactionMechanism.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("reactionmechanism",JSONObject.NULL);
        val= checkReactionMechanism.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        requestParams.put("reactionmechanism","testReactionMechanism");
        val= checkReactionMechanism.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckShip() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ShipAgent shipAgent= new ShipAgent();
        Method checkShip=shipAgent.getClass().getDeclaredMethod("checkShip", JSONObject.class);
        checkShip.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        String val= checkShip.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty string value
        JSONObject ship=new JSONObject();
        requestParams.put("ship",ship);
        val=checkShip.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        requestParams.put("ship",JSONObject.NULL);
        val= checkShip.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with a valid key:value pair
        ship.put("key","value");
        requestParams.put("ship",ship);
        val= checkShip.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckMMSI() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ShipAgent shipAgent= new ShipAgent();
        Method checkMMSI=shipAgent.getClass().getDeclaredMethod("checkMMSI", JSONObject.class);
        checkMMSI.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject ship= new JSONObject();
        requestParams.put("ship",ship);
        String val= checkMMSI.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null value
        ship.put("mmsi",JSONObject.NULL);
        requestParams.put("ship",ship);
        val=checkMMSI.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty value
        ship.remove("mmsi");
        ship.put("mmsi","");
        requestParams.put("ship",ship);
        val=checkMMSI.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        ship.remove("mmsi");
        ship.put("mmsi","testMMSI");
        requestParams.put("ship",ship);
        val=checkMMSI.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckType() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ShipAgent shipAgent= new ShipAgent();
        Method checkType=shipAgent.getClass().getDeclaredMethod("checkType", JSONObject.class);
        checkType.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject ship= new JSONObject();
        requestParams.put("ship",ship);
        String val= checkType.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty key
        ship.put("type","");
        requestParams.put("ship",ship);
        val= checkType.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null key
        ship.remove("type");
        ship.put("type",JSONObject.NULL);
        requestParams.put("ship",ship);
        val= checkType.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        ship.remove("type");
        ship.put("type","testType");
        requestParams.put("ship",ship);
        val= checkType.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testCheckSS() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ShipAgent shipAgent= new ShipAgent();
        Method checkSS=shipAgent.getClass().getDeclaredMethod("checkSS", JSONObject.class);
        checkSS.setAccessible(true);

        //check case with no key
        JSONObject requestParams= new JSONObject();
        JSONObject ship= new JSONObject();
        requestParams.put("ship",ship);
        String val= checkSS.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with null key
        ship.put("ss",JSONObject.NULL);
        requestParams.put("ship",ship);
        val=checkSS.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with empty key
        ship.remove("ss");
        ship.put("ss","");
        requestParams.put("ship",ship);
        val=checkSS.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("false",val);

        //check case with valid key:value pair
        ship.put("ss",23.d);
        requestParams.put("ship",ship);
        val=checkSS.invoke(shipAgent,requestParams).toString();
        Assert.assertEquals("true",val);
    }

    @Test
    public void testValidateInput(){
        ShipAgent shipAgent= new ShipAgent();
        JSONObject requestParams = new JSONObject();
        //start with empty requestParam
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("RequestParam is empty", e.getMessage());
        }
        //test the block associated with the key:reactionmechanism
        requestParams.put("reactionmechanism","");
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the requestParam object either the key:reactionmechanism is not present or it is null or it is empty", e.getMessage());
        }
        requestParams.put("reactionmechanism",JSONObject.NULL);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the requestParam object either the key:reactionmechanism is not present or it is null or it is empty", e.getMessage());
        }
        //test the block associated with the key:ship
        requestParams.put("reactionmechanism","testReactionMechanism");
        JSONObject ship= new JSONObject();
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the requestParam object either key:ship is not present or it is null or it is empty", e.getMessage());
        }
        requestParams.put("ship",JSONObject.NULL);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the requestParam object either key:ship is not present or it is null or it is empty", e.getMessage());
        }
        //test the block associated with the key:mmsi
        ship.put("mmsi",JSONObject.NULL);
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:mmsi is missing or it is null or it is empty", e.getMessage());
        }
        ship.put("mmsi","");
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:mmsi is missing or it is null or it is empty", e.getMessage());
        }
        //test the block associated with the key:ss
        ship.put("mmsi","testMMSI");
        ship.put("ss",JSONObject.NULL);
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:ss is missing or it is null", e.getMessage());
        }
        ship.put("ss","");
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:ss is missing or it is null", e.getMessage());
        }
        //test the block associated with the key:type
        ship.put("ss","42");
        ship.put("type","");
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:type is missing or it is null or it is empty", e.getMessage());
        }
        ship.put("type",JSONObject.NULL);
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:type is missing or it is null or it is empty", e.getMessage());
        }
        //finally test for missing keys
        ship.put("type","testType");
        //missing key:reactionmechanism
        requestParams.remove("reactionmechanism");
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the requestParam object either the key:reactionmechanism is not present or it is null or it is empty", e.getMessage());
        }
        requestParams.put("reactionmechanism","testReactionMechanism");
        //missing key:ship
        requestParams.remove("ship");
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the requestParam object either key:ship is not present or it is null or it is empty", e.getMessage());
        }
        //missing key:mmsi
        ship.remove("mmsi");
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:mmsi is missing or it is null or it is empty", e.getMessage());
        }
        //missing key:ss
        ship.put("mmsi","testMMSI");
        ship.remove("ss");
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:ss is missing or it is null", e.getMessage());
        }
        //missing key:type
        ship.put("ss","42");
        ship.remove("type");
        requestParams.put("ship",ship);
        try {
            shipAgent.validateInput(requestParams);
            Assert.fail();
        } catch (BadRequestException e) {
            Assert.assertEquals("In the ship object either the key:type is missing or it is null or it is empty", e.getMessage());
        }
    }
    @Test
    public void testResetEmissionRate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent= new ShipAgent();
        Method resetEmissionRate=shipAgent.getClass().getDeclaredMethod("resetEmissionRate", Map.class, OntModel.class, String.class);
        resetEmissionRate.setAccessible(true);

        //create test arguments to be passed
        HashMap<String,String> hmap=new HashMap<>();
        hmap.put("key1","value1");
        hmap.put("key2","value2");
        String iriofchimney="http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel=ModelFactory.createOntologyModel();

        OntClass ontClass= ontModel.createClass("http://test.org/testClass");
        Individual ind1= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "value1" + EM_RATE);
        Individual ind2= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "value2" + EM_RATE);
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");

        ind1.addProperty(pCPT_NUMVAL,"4");
        ind2.addProperty(pCPT_NUMVAL,"5");

        try(MockedStatic<BaseOntologyModelManager> baseOntModMan=Mockito.mockStatic(BaseOntologyModelManager.class)){
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
            resetEmissionRate.invoke(shipAgent,hmap,ontModel,iriofchimney);
            Double actual1=ind1.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double actual2=ind2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Assert.assertEquals(actual1,0d,1e-6);
            Assert.assertEquals(actual2,0d,1e-6);
        }
    }

    @Test
    public void testSetPropertyValues() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent= new ShipAgent();
        Method setPropertyValues=shipAgent.getClass().getDeclaredMethod("setPropertyValues", JSONObject.class, String.class, OntModel.class);
        setPropertyValues.setAccessible(true);

        //create test arguments to be passed
        JSONObject jsonObject= new JSONObject();
        JSONObject mixture= new JSONObject();
        JSONObject molmass= new JSONObject();
        JSONObject cp= new JSONObject();
        JSONObject temperature= new JSONObject();
        JSONObject massflux= new JSONObject();
        JSONObject density= new JSONObject();

        density.put("value",12d);
        massflux.put("value",10d);
        temperature.put("value",500d);
        cp.put("value",15d);
        molmass.put("value",3d);

        mixture.put("molmass",molmass);
        mixture.put("cp",cp);
        mixture.put("temperature",temperature);
        mixture.put("massflux",massflux);
        mixture.put("density",density);
        jsonObject.put("mixture",mixture);

        String iriofchimney="http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel=ModelFactory.createOntologyModel();

        OntClass ontClass= ontModel.createClass("http://test.org/testClass");
        Individual valueofmassflowrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_massF_WasteStream-001");
        Individual valueofdensityrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_MaterialInWasteStream-001");
        Individual valueoftemperature=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Temperature_MaterialInWasteStream-001");
        Individual valueofcombinedmolecularmass=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_ChemSpecies_Combined_MolecularMass");
        Individual valueofcombinedheatcapacity=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Cp_MaterialInWasteStream-001");

        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");

        try(MockedStatic<BaseOntologyModelManager> baseOntModMan=Mockito.mockStatic(BaseOntologyModelManager.class)){
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
            setPropertyValues.invoke(shipAgent,jsonObject,iriofchimney,ontModel);
            Double molecularvalue=valueofcombinedmolecularmass.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double Cpvalue=valueofcombinedheatcapacity.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double temperaturevalue=valueoftemperature.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double massfluxvalue=valueofmassflowrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double densityvalue=valueofdensityrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

            Assert.assertEquals(molecularvalue,3000,1e-6);
            Assert.assertEquals(Cpvalue,15,1e-6);
            Assert.assertEquals(temperaturevalue,226.85,1e-6);
            Assert.assertEquals(massfluxvalue,10,1e-6);
            Assert.assertEquals(densityvalue,12,1e-6);
        }
    }

    //case when particulate1 and particulaterate1 is not null
    @Test
    public void testAddPropertiesToParticulate1NotNull() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent = new ShipAgent();
        Method addPropertiesToParticulate1 = shipAgent.getClass().getDeclaredMethod("addPropertiesToParticulate1", OntModel.class, String.class, Individual.class, Individual.class);
        addPropertiesToParticulate1.setAccessible(true);


        //construct the mock arguments to pass
        String iriofchimney = "http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel = ModelFactory.createOntologyModel();
        OntClass ontClass = ontModel.createClass("http://test.org/testClass");
        Individual particulate1 = ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001);
        Individual particleratevalue1 = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE);
        Individual particulaterate1 = ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001 + EM_RATE);

        Property pCPT_HASPROP = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP
        Property pCPT_HASVAL = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_NUMVAL = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        particleratevalue1.setPropertyValue(pCPT_NUMVAL, ontModel.createTypedLiteral(Double.valueOf(0)));

        Resource rCPT_SI_GPS = ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");//CPT_SI_GPS

        try (MockedStatic<BaseOntologyModelManager> baseOntModMan = Mockito.mockStatic(BaseOntologyModelManager.class)) {
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_GPS)).thenReturn(rCPT_SI_GPS);

            addPropertiesToParticulate1.invoke(shipAgent, ontModel, iriofchimney, particulate1, particleratevalue1);
            Object expectedValue = particleratevalue1.getPropertyValue(pCPT_HASUOM);
            Assert.assertEquals(expectedValue, rCPT_SI_GPS);
        }
    }

    //case when particulate1 and particulaterate1 is null
    @Test
    public void testAddPropertiesToParticulate1Null() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        //allow access to the method
        ShipAgent shipAgent = new ShipAgent();
        Method addPropertiesToParticulate1 = shipAgent.getClass().getDeclaredMethod("addPropertiesToParticulate1", OntModel.class, String.class, Individual.class, Individual.class);
        addPropertiesToParticulate1.setAccessible(true);

        //construct the mock arguments to pass
        String iriofchimney = "http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel=ModelFactory.createOntologyModel();
        OntClass ontClass= ontModel.createClass("http://test.org/testClass");
        Individual material2 = ontClass.createIndividual(iriofchimney.split("#")[0] + "#GeneralizedAmount_WasteStreamOfChimney-1");
        Individual particleratevalue1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE);
        Property pCPT_NUMVAL=ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        particleratevalue1.setPropertyValue(pCPT_NUMVAL,ontModel.createTypedLiteral(Double.valueOf(0)));

        Property pCPT_CONTAINS=ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");//CPT_CONTAINS
        Property pCPT_HASPROP=ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP
        Property pCPT_HASVAL=ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM=ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM

        Resource rCPT_PMAMT=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ParticulateMaterialAmount");//CPT_PMAMT
        Resource rCPT_CONVMFLR=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");//CPT_CONVMFLR
        Resource rCPT_SCLVAL=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");//CPT_SCLVAL
        Resource rCPT_SI_GPS=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");//CPT_SI_GPS

        try(MockedStatic<BaseOntologyModelManager> baseOntModMan=Mockito.mockStatic(BaseOntologyModelManager.class)){
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_CONTAINS)).thenReturn(pCPT_CONTAINS);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_PMAMT)).thenReturn(rCPT_PMAMT);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_CONVMFLR)).thenReturn(rCPT_CONVMFLR);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SCLVAL)).thenReturn(rCPT_SCLVAL);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_GPS)).thenReturn(rCPT_SI_GPS);

            addPropertiesToParticulate1.invoke(shipAgent,ontModel, iriofchimney,null,particleratevalue1);

            Object expectedValue= particleratevalue1.getPropertyValue(pCPT_HASUOM);
            Assert.assertEquals(expectedValue,rCPT_SI_GPS);
        }
    }

    @Test
    public void testUpdateTotalParticleEmission() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent= new ShipAgent();
        Method totalParticleEmission=shipAgent.getClass().getDeclaredMethod("updateTotalParticleEmission", JSONObject.class, int.class);
        totalParticleEmission.setAccessible(true);

        //construct the mock JSONObject
        JSONObject jsonObject= new JSONObject();
        JSONObject jsonObject1= new JSONObject();
        JSONObject jsonObject2= new JSONObject();

        JSONArray jsonArray= new JSONArray();
        JSONObject emm1= new JSONObject();
        emm1.put("value",23.4d);
        JSONObject emm2= new JSONObject();
        emm2.put("value",2d);
        jsonObject1.put("emission_rate",emm1);
        jsonObject2.put("emission_rate",emm2);
        jsonArray.put(jsonObject1);
        jsonArray.put(jsonObject2);
        jsonObject.put("particle",jsonArray);

        double totalParticleEmissions= (Double) totalParticleEmission.invoke(shipAgent,jsonObject, jsonObject.getJSONArray("particle").length());
        Assert.assertEquals(25.4d,totalParticleEmissions,1e-6);
    }

    //first case where individual partialparticulate is not null
    @Test
    public void testCreateParticulateIndividuals1() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent = new ShipAgent();
        Method createParticulateIndividuals = shipAgent.getClass().getDeclaredMethod("createParticulateIndividuals", OntModel.class, String.class, String.class, int.class, double.class, int.class, Individual.class);
        createParticulateIndividuals.setAccessible(true);

        //construct the mock arguments to pass
        int a = 1;
        double valueofdiameter = 1d;
        int valueofdensity = 4;
        String valueofmassfraction = "0.6";
        String iriofchimney = "http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel = ModelFactory.createOntologyModel();
        OntClass ontClass = ontModel.createClass("http://test.org/testClass");
        Individual partialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" + P001);
        Individual diameterpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001);
        Individual diametervaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
        Individual densitypartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001);
        Individual densityvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
        Individual massfractionpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001);
        Individual massfractionvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);

        Individual particulate1 = ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001);

        Property pCPT_HASREPRPART = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");//CPT_HASREPRPART
        Property pCPT_HASLEN = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");//CPT_HASLEN
        Property pCPT_HASVAL = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_NUMVAL = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        Property pCPT_HASDENS = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");//CPT_HASDENS
        Property pCPT_HASPROP = ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP

        Resource rCPT_SI_M = ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");//CPT_SI_M
        Resource rCPT_SI_KGPCM = ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");//CPT_SI_KGPCM

        try (MockedStatic<BaseOntologyModelManager> baseOntModMan = Mockito.mockStatic(BaseOntologyModelManager.class)) {
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASREPRPART)).thenReturn(pCPT_HASREPRPART);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN)).thenReturn(pCPT_HASLEN);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS)).thenReturn(pCPT_HASDENS);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);

            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M)).thenReturn(rCPT_SI_M);
            baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM)).thenReturn(rCPT_SI_KGPCM);

            createParticulateIndividuals.invoke(shipAgent, ontModel, iriofchimney, valueofmassfraction, valueofdensity, valueofdiameter, a, particulate1);

            Double expectedDiameterValue = diametervaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double expectedDensityValue = densityvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            String expectedMassfractionvalue = massfractionvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getString();

            Assert.assertEquals(expectedDiameterValue, 1e-9, 1e-6);
            Assert.assertEquals(expectedDensityValue, 4d, 1e-6);
            Assert.assertEquals(expectedMassfractionvalue, "0.6");
        }
    }

    //second case where individual partialparticulate is null
    @Test
    public void testCreateParticulateIndividuals2() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent = new ShipAgent();
        Method createParticulateIndividuals = shipAgent.getClass().getDeclaredMethod("createParticulateIndividuals", OntModel.class, String.class, String.class, int.class, double.class, int.class, Individual.class);
        createParticulateIndividuals.setAccessible(true);

        //construct the mock arguments to pass
        int a = 1;
        double valueofdiameter = 1d;
        int valueofdensity = 4;
        String valueofmassfraction = "0.6";
        String iriofchimney = "http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel=ModelFactory.createOntologyModel();
        OntClass ontClass=ontModel.createClass("http://test.org/testClass");
        Individual particulate1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001);
        Individual diametervaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
        Individual densityvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
        Individual massfractionvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);


        Resource rCPT_SINGPART=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#SingleParticle");//CPT_SINGPART
        Resource rCPT_DIAM=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Diameter");//CPT_DIAM
        Resource rCPT_SCLVAL=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");//CPT_SCLVAL
        Resource rCPT_DENS=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Density");//CPT_DENS
        Resource rCPT_MASSFR=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MassFraction");//CPT_MASSFR

        Property pCPT_HASREPRPART= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");//CPT_HASREPRPART
        Property pCPT_HASLEN= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");//CPT_HASLEN
        Property pCPT_HASVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        Property pCPT_HASDENS= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");//CPT_HASDENS
        Property pCPT_HASPROP= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP

        Resource rCPT_SI_M=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");//CPT_SI_M
        Resource rCPT_SI_KGPCM=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");//CPT_SI_KGPCM

        try(MockedStatic<BaseOntologyModelManager> baseOntModMan=Mockito.mockStatic(BaseOntologyModelManager.class)){
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASREPRPART)).thenReturn(pCPT_HASREPRPART);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN)).thenReturn(pCPT_HASLEN);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS)).thenReturn(pCPT_HASDENS);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);

            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M)).thenReturn(rCPT_SI_M);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM)).thenReturn(rCPT_SI_KGPCM);

            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SINGPART)).thenReturn(rCPT_SINGPART);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_DIAM)).thenReturn(rCPT_DIAM);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SCLVAL)).thenReturn(rCPT_SCLVAL);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_DENS)).thenReturn(rCPT_DENS);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_MASSFR)).thenReturn(rCPT_MASSFR);

            createParticulateIndividuals.invoke(shipAgent,ontModel,iriofchimney, valueofmassfraction, valueofdensity, valueofdiameter,a,particulate1);

            Double expectedDiameterValue=diametervaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double expectedDensityValue=densityvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            String expectedMassfractionvalue=massfractionvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getString();

            Assert.assertEquals(expectedDiameterValue,1e-9,1e-6);
            Assert.assertEquals(expectedDensityValue,4d,1e-6);
            Assert.assertEquals(expectedMassfractionvalue,"0.6");
        }
    }

    @Test
    public void testSetPartialParticulateProperties() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent= new ShipAgent();
        Method setPartialParticulateProperties=shipAgent.getClass().getDeclaredMethod("setPartialParticulateProperties", OntModel.class, Individual.class, Individual.class, Individual.class, Individual.class, Individual.class, Individual.class, Individual.class, double.class, int.class, String.class);
        setPartialParticulateProperties.setAccessible(true);

        //construct the mock arguments to pass
        int a=1;
        double valueofdiameter=1d;
        int valueofdensity=4;
        String valueofmassfraction="0.6";
        String iriofchimney="http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel=ModelFactory.createOntologyModel();
        OntClass ontClass=ontModel.createClass("http://test.org/testClass");
        Individual partialparticulate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" +P001);
        Individual diameterpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001);
        Individual diametervaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
        Individual densitypartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001);
        Individual densityvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
        Individual massfractionpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001);
        Individual massfractionvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);

        Property pCPT_HASLEN= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");//CPT_HASLEN
        Property pCPT_HASVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        Property pCPT_HASDENS= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");//CPT_HASDENS
        Property pCPT_HASPROP= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP

        Resource rCPT_SI_M=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");//CPT_SI_M
        Resource rCPT_SI_KGPCM=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");//CPT_SI_KGPCM

        try(MockedStatic<BaseOntologyModelManager> baseOntModMan=Mockito.mockStatic(BaseOntologyModelManager.class)){
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN)).thenReturn(pCPT_HASLEN);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS)).thenReturn(pCPT_HASDENS);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);

            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M)).thenReturn(rCPT_SI_M);
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM)).thenReturn(rCPT_SI_KGPCM);

            setPartialParticulateProperties.invoke(shipAgent,ontModel,partialparticulate, diameterpartialparticulate, diametervaluepartialparticulate,
                                                    densitypartialparticulate, densityvaluepartialparticulate, massfractionpartialparticulate,
                                                    massfractionvaluepartialparticulate, valueofdiameter, valueofdensity,valueofmassfraction);

            Double expectedDiameterValue=diametervaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double expectedDensityValue=densityvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            String expectedMassfractionvalue=massfractionvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getString();

            Assert.assertEquals(expectedDiameterValue,1e-9,1e-6);
            Assert.assertEquals(expectedDensityValue,4d,1e-6);
            Assert.assertEquals(expectedMassfractionvalue,"0.6");
        }
    }

    @Test
    public void testSetNOXPollutantValues() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException {
        //allow access to the method
        ShipAgent shipAgent= new ShipAgent();
        Method setNOXPollutantValues=shipAgent.getClass().getDeclaredMethod("setNOXPollutantValues", Map.class, JSONObject.class, OntModel.class, String.class, int.class);
        setNOXPollutantValues.setAccessible(true);

        //construct the mock arguments to pass
        JSONObject jsonObject =new JSONObject();
        JSONArray jsonArray= new JSONArray();
        JSONObject no= new JSONObject();
        JSONObject no2= new JSONObject();
        no.put("name","NO");
        no.put("value",2d);
        no2.put("name","NO2");
        no2.put("value",4d);
        jsonArray.put(no);
        jsonArray.put(no2);
        jsonObject.put("pollutants",jsonArray);

        HashMap<String,String> hmap=new HashMap<>();
        hmap.put("NO","NOvalue");
        hmap.put("NO2","NO2value");
        hmap.put("NOX","NOXvalue");
        String iriofchimney="http://prefix.org/chimneyTestIRIPrefix#blahblah";
        OntModel ontModel=ModelFactory.createOntologyModel();
        OntClass ontClass= ontModel.createClass("http://test.org/testClass");
        Individual NO=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(no.getString("name")) + EM_RATE);
        Individual NO2=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(no2.getString("name")) + EM_RATE);
        Individual NOX=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + EM_RATE);
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");

        try(MockedStatic<BaseOntologyModelManager> baseOntModMan=Mockito.mockStatic(BaseOntologyModelManager.class)){
            baseOntModMan.when(()->BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
            setNOXPollutantValues.invoke(shipAgent,hmap,jsonObject,ontModel,iriofchimney,2);
            Double actualNOEmmValue=NO.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double actualNO2EmmValue=NO2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
            Double actualNOXEmmValue=NOX.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

            Assert.assertEquals(2000,actualNOEmmValue,1e-6);
            Assert.assertEquals(4000,actualNO2EmmValue,1e-6);
            Assert.assertEquals(6000,actualNOXEmmValue,1e-6);
        }
    }

    @Test
    public void testDoConversion() throws NoSuchMethodException,InvocationTargetException, IllegalAccessException{
        ShipAgent shipAgent= new ShipAgent();
        Method doConversion=shipAgent.getClass().getDeclaredMethod("doConversion", OntModel.class, String.class, JSONObject.class);
        doConversion.setAccessible(true);

        //create test arguments to be passed
        JSONObject jsonObject =new JSONObject();
        JSONArray pollutants= new JSONArray();
        JSONArray particle= new JSONArray();

        JSONObject emissionRate= new JSONObject();
        emissionRate.put("value",23.4d);

        JSONObject no2= new JSONObject();
        JSONObject mass_fraction= new JSONObject();
        JSONObject particle1= new JSONObject();
        JSONObject diameter= new JSONObject();
        JSONObject pdensity= new JSONObject();

        JSONObject mixture= new JSONObject();
        JSONObject molmass= new JSONObject();
        JSONObject cp= new JSONObject();
        JSONObject temperature= new JSONObject();
        JSONObject massflux= new JSONObject();
        JSONObject density= new JSONObject();


        mass_fraction.put("value","0.6");
        diameter.put("value",1d);
        pdensity.put("value",4);

        particle1.put("mass_fraction",mass_fraction);
        particle1.put("diameter",diameter);
        particle1.put("density",pdensity);
        particle1.put("emission_rate",emissionRate);
        particle.put(particle1);

        no2.put("name","NO2");
        no2.put("value",4d);
        pollutants.put(no2);

        density.put("value",12d);
        massflux.put("value",10d);
        temperature.put("value",500d);
        cp.put("value",15d);
        molmass.put("value",3d);

        mixture.put("molmass",molmass);
        mixture.put("cp",cp);
        mixture.put("temperature",temperature);
        mixture.put("massflux",massflux);
        mixture.put("density",density);

        jsonObject.put("mixture",mixture);
        jsonObject.put("pollutants",pollutants);
        jsonObject.put("particle",particle);

        HashMap<String,String> hmap=new HashMap<>();
        hmap.put("NO2","NO2value");
        hmap.put("NOx","NOXvalue");
        String iriofchimney="http://prefix.org/chimneyTestIRIPrefix#blahblah";

        OntModel ontModel=ModelFactory.createOntologyModel();

        OntClass ontClass= ontModel.createClass("http://test.org/testClass");

        //individuals for resetEmissions method
        Individual resetInd1= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "NO2value" + EM_RATE);
        Individual resetInd2= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "NOXvalue" + EM_RATE);

        //individuals for addPropertiesToParticulate1 method

        Individual particulate1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001);
        Individual particleratevalue1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE);
        Individual particulaterate1 =ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001 + EM_RATE);

        //individuals for setPropertyValues

        Individual valueofmassflowrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_massF_WasteStream-001");
        Individual valueofdensityrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_MaterialInWasteStream-001");
        Individual valueoftemperature=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Temperature_MaterialInWasteStream-001");
        Individual valueofcombinedmolecularmass=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_ChemSpecies_Combined_MolecularMass");
        Individual valueofcombinedheatcapacity=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Cp_MaterialInWasteStream-001");

        //individuals used by createParticulateIndividuals and setPartialParticulateProperties
        int a=0;
        Individual partialparticulate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" +P001);
        Individual diameterpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001);
        Individual diametervaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
        Individual densitypartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001);
        Individual densityvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
        Individual massfractionpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001);
        Individual massfractionvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);

        //individuals for NOXEmissions method
        Individual NO2=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(no2.getString("name")) + EM_RATE);
        Individual NOX=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + EM_RATE);

        //all properties
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        Property pCPT_HASREPRPART= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");//CPT_HASREPRPART
        Property pCPT_HASLEN= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");//CPT_HASLEN
        Property pCPT_HASVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_HASDENS= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");//CPT_HASDENS
        Property pCPT_HASPROP= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP

        //all resources
        Resource rCPT_SI_M=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");//CPT_SI_M
        Resource rCPT_SI_KGPCM=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");//CPT_SI_KGPCM
        Resource rCPT_SI_GPS=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");//CPT_SI_GPS

        try (MockedStatic<BaseChimneyOntologyModelManager> baseChimOntModMan = Mockito.mockStatic(BaseChimneyOntologyModelManager.class)) {
                try(MockedStatic<BaseOntologyModelManager> baseOntModMan = Mockito.mockStatic(BaseOntologyModelManager.class)) {
                    baseChimOntModMan.when(()->BaseChimneyOntologyModelManager.getSpeciesMap()).thenReturn(hmap);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS)).thenReturn(pCPT_HASDENS);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN)).thenReturn(pCPT_HASLEN);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASREPRPART)).thenReturn(pCPT_HASREPRPART);

                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_GPS)).thenReturn(rCPT_SI_GPS);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M)).thenReturn(rCPT_SI_M);
                    baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM)).thenReturn(rCPT_SI_KGPCM);


                    doConversion.invoke(shipAgent, ontModel, iriofchimney, jsonObject);


                    //assertion checks for the resetEmissions method
                    // Since the numericalValue of these individual is overwritten later on in the NOXEmission method
                    // the assertion value being tested is not 0 but the value of NO2.
                    Double actualReset1 = resetInd1.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Double actualReset2 = resetInd2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                    Assert.assertEquals(actualReset1, 4000d, 1e-6);
                    Assert.assertEquals(actualReset2, 4000d, 1e-6);

                    //assertion checks for setPropertyValues method

                    Double molecularvalue = valueofcombinedmolecularmass.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Double Cpvalue = valueofcombinedheatcapacity.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Double temperaturevalue = valueoftemperature.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Double massfluxvalue = valueofmassflowrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Double densityvalue = valueofdensityrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                    Assert.assertEquals(molecularvalue, 3000, 1e-6);
                    Assert.assertEquals(Cpvalue, 15, 1e-6);
                    Assert.assertEquals(temperaturevalue, 226.85, 1e-6);
                    Assert.assertEquals(massfluxvalue, 10, 1e-6);
                    Assert.assertEquals(densityvalue, 12, 1e-6);

                    //assertion checks for addPropertiesToParticulate1 method
                    Object expectedValue = particleratevalue1.getPropertyValue(pCPT_HASUOM);
                    Assert.assertEquals(expectedValue, rCPT_SI_GPS);

                    //assertion checks for updateTotalParticleEmission method
                    Double expectedEmissionsValue = particleratevalue1.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Assert.assertEquals(expectedEmissionsValue, 23400, 1e-6);//taking into account the multiplication with 1000 later on in the code.

                    //assertion checks for the createParticulateIndividuals method and  setPartialParticulateProperties method
                    Double expectedDiameterValue = diametervaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Double expectedDensityValue = densityvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    String expectedMassfractionvalue = massfractionvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getString();

                    Assert.assertEquals(expectedDiameterValue, 1e-9, 1e-6);
                    Assert.assertEquals(expectedDensityValue, 4d, 1e-6);
                    Assert.assertEquals(expectedMassfractionvalue, "0.6");


                    //assertion checks for the NOXEmission method
                    Double actualNO2EmmValue = NO2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                    Double actualNOXEmmValue = NOX.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                    Assert.assertEquals(4000, actualNO2EmmValue, 1e-6);
                    Assert.assertEquals(4000, actualNOXEmmValue, 1e-6);
                }
            }
    }

    @Test
    public void testStartConversion()  throws NoSuchMethodException,InvocationTargetException, IllegalAccessException,IOException{
        ShipAgent shipAgent=new ShipAgent();
        Method startConversion=shipAgent.getClass().getDeclaredMethod("startConversion", OntModel.class, String.class, JSONObject.class, String.class);
        startConversion.setAccessible(true);

        JSONObject jsonObject =new JSONObject();
        JSONArray pollutants= new JSONArray();
        JSONArray particle= new JSONArray();

        JSONObject emissionRate= new JSONObject();
        emissionRate.put("value",23.4d);

        JSONObject no2= new JSONObject();
        JSONObject mass_fraction= new JSONObject();
        JSONObject particle1= new JSONObject();
        JSONObject diameter= new JSONObject();
        JSONObject pdensity= new JSONObject();

        JSONObject mixture= new JSONObject();
        JSONObject molmass= new JSONObject();
        JSONObject cp= new JSONObject();
        JSONObject temperature= new JSONObject();
        JSONObject massflux= new JSONObject();
        JSONObject density= new JSONObject();

        mass_fraction.put("value","0.6");
        diameter.put("value",1d);
        pdensity.put("value",4);

        particle1.put("mass_fraction",mass_fraction);
        particle1.put("diameter",diameter);
        particle1.put("density",pdensity);
        particle1.put("emission_rate",emissionRate);
        particle.put(particle1);

        no2.put("name","NO2");
        no2.put("value",4d);
        pollutants.put(no2);

        density.put("value",12d);
        massflux.put("value",10d);
        temperature.put("value",500d);
        cp.put("value",15d);
        molmass.put("value",3d);

        mixture.put("molmass",molmass);
        mixture.put("cp",cp);
        mixture.put("temperature",temperature);
        mixture.put("massflux",massflux);
        mixture.put("density",density);

        jsonObject.put("mixture",mixture);
        jsonObject.put("pollutants",pollutants);
        jsonObject.put("particle",particle);

        HashMap<String,String> hmap=new HashMap<>();
        hmap.put("NO2","NO2value");
        hmap.put("NOx","NOXvalue");
        String iriofchimney="http://prefix.org/chimneyTestIRIPrefix#blahblah";
        String mmsi="123";

        OntModel ontModel=ModelFactory.createOntologyModel();
        OntClass ontClass= ontModel.createClass("http://test.org/testClass");

        //individuals for resetEmissions method
        Individual resetInd1= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "NO2value" + EM_RATE);
        Individual resetInd2= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "NOXvalue" + EM_RATE);

        //individuals for addPropertiesToParticulate1 method

        Individual particulate1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001);
        Individual particleratevalue1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE);
        Individual particulaterate1 =ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001 + EM_RATE);

        //individuals for setPropertyValues

        Individual valueofmassflowrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_massF_WasteStream-001");
        Individual valueofdensityrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_MaterialInWasteStream-001");
        Individual valueoftemperature=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Temperature_MaterialInWasteStream-001");
        Individual valueofcombinedmolecularmass=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_ChemSpecies_Combined_MolecularMass");
        Individual valueofcombinedheatcapacity=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Cp_MaterialInWasteStream-001");

        //individuals used by createParticulateIndividuals and setPartialParticulateProperties
        int a=0;
        Individual partialparticulate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" +P001);
        Individual diameterpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001);
        Individual diametervaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
        Individual densitypartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001);
        Individual densityvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
        Individual massfractionpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001);
        Individual massfractionvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);

        //individuals for NOXEmissions method
        Individual NO2=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(no2.getString("name")) + EM_RATE);
        Individual NOX=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + EM_RATE);

        //all properties
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        Property pCPT_HASREPRPART= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");//CPT_HASREPRPART
        Property pCPT_HASLEN= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");//CPT_HASLEN
        Property pCPT_HASVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_HASDENS= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");//CPT_HASDENS
        Property pCPT_HASPROP= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP

        //all resources
        Resource rCPT_SI_M=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");//CPT_SI_M
        Resource rCPT_SI_KGPCM=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");//CPT_SI_KGPCM
        Resource rCPT_SI_GPS=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");//CPT_SI_GPS

        try (MockedStatic<BaseChimneyOntologyModelManager> baseChimOntModMan = Mockito.mockStatic(BaseChimneyOntologyModelManager.class)) {
            try (MockedStatic<BaseOntologyModelManager> baseOntModMan = Mockito.mockStatic(BaseOntologyModelManager.class)) {

                //add stuff for doConversion
                baseChimOntModMan.when(()->BaseChimneyOntologyModelManager.getSpeciesMap()).thenReturn(hmap);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS)).thenReturn(pCPT_HASDENS);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN)).thenReturn(pCPT_HASLEN);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASREPRPART)).thenReturn(pCPT_HASREPRPART);

                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_GPS)).thenReturn(rCPT_SI_GPS);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M)).thenReturn(rCPT_SI_M);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM)).thenReturn(rCPT_SI_KGPCM);

                //workaround for doNothing.when(mockedClass).mockedMethod(arguments) when using mockito static
                baseOntModMan.when(() -> LocalOntologyModelManager.save(ontModel, iriofchimney, mmsi)).thenAnswer((Answer<Void>) invocation -> null);
                startConversion.invoke(shipAgent, ontModel, iriofchimney, jsonObject, mmsi);

                //assertion checks for the resetEmissions method
                // Since the numericalValue of these individual is overwritten later on in the NOXEmission method
                // the assertion value being tested is not 0 but the value of NO2.
                Double actualReset1 = resetInd1.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double actualReset2 = resetInd2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                Assert.assertEquals(actualReset1, 4000d, 1e-6);
                Assert.assertEquals(actualReset2, 4000d, 1e-6);

                //assertion checks for setPropertyValues method

                Double molecularvalue = valueofcombinedmolecularmass.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double Cpvalue = valueofcombinedheatcapacity.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double temperaturevalue = valueoftemperature.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double massfluxvalue = valueofmassflowrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double densityvalue = valueofdensityrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                Assert.assertEquals(molecularvalue, 3000, 1e-6);
                Assert.assertEquals(Cpvalue, 15, 1e-6);
                Assert.assertEquals(temperaturevalue, 226.85, 1e-6);
                Assert.assertEquals(massfluxvalue, 10, 1e-6);
                Assert.assertEquals(densityvalue, 12, 1e-6);

                //assertion checks for addPropertiesToParticulate1 method
                Object expectedValue = particleratevalue1.getPropertyValue(pCPT_HASUOM);
                Assert.assertEquals(expectedValue, rCPT_SI_GPS);

                //assertion checks for updateTotalParticleEmission method
                Double expectedEmissionsValue = particleratevalue1.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Assert.assertEquals(expectedEmissionsValue, 23400, 1e-6);//taking into account the multiplication with 1000 later on in the code.

                //assertion checks for the createParticulateIndividuals method and  setPartialParticulateProperties method
                Double expectedDiameterValue = diametervaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double expectedDensityValue = densityvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                String expectedMassfractionvalue = massfractionvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getString();

                Assert.assertEquals(expectedDiameterValue, 1e-9, 1e-6);
                Assert.assertEquals(expectedDensityValue, 4d, 1e-6);
                Assert.assertEquals(expectedMassfractionvalue, "0.6");


                //assertion checks for the NOXEmission method
                Double actualNO2EmmValue = NO2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double actualNOXEmmValue = NOX.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                Assert.assertEquals(4000, actualNO2EmmValue, 1e-6);
                Assert.assertEquals(4000, actualNOXEmmValue, 1e-6);
            }
        }
    }

    //createResultObject with iri containing "theworldavatar"
    @Test
    public void testCreateResultObject1() throws NoSuchMethodException , InvocationTargetException, IllegalAccessException{
        ShipAgent shipAgent= new ShipAgent();
        Method createResultObject=shipAgent.getClass().getDeclaredMethod("createResultObject", JSONObject.class, String.class, ArrayList.class, JSONObject.class);
        createResultObject.setAccessible(true);

        //create mock arguments to pass
        JSONObject joforrec= new JSONObject();
        JSONObject ship= new JSONObject();
        ship.put("ss",42.1d);
        ship.put("speed",23.1d);
        ship.put("mmsi","123");
        ship.put("type","testType");
        joforrec.put("ship",ship);
        joforrec.put("reactionmechanism","testReactionMechanismtheworldavatar");

        String shipKbURL= "testurl";
        String iri = joforrec.optString("reactionmechanism");
        String mmsi = joforrec.getJSONObject("ship").get("mmsi").toString(); //only get the mmsi instead of full iri

        ArrayList<String> cpirilist2 = new ArrayList<>();
        cpirilist2.add(shipKbURL+"Engine-001.owl#Engine-001");

        JSONObject dataSet=new JSONObject();
        dataSet.put("reactionmechanism", iri);
        dataSet.put("engine", cpirilist2.get(0));
        dataSet.put("source", "ship");
        JSONObject jo2= new JSONObject();

        jo2.put("speed", joforrec.getJSONObject("ship").getDouble("ss"));
        jo2.put("type", joforrec.getJSONObject("ship").get("type").toString().replace("+", " "));

        String resultjson= "{key:value,results:{key1:value1,key2:value2}}";
        JSONObject expected= (JSONObject) new JSONObject(resultjson).get("results");

        try(MockedStatic<AgentCaller>agentCaller=Mockito.mockStatic(AgentCaller.class)){
            agentCaller.when(()->AgentCaller.executeGet("JPS/SRMAgent", "query", dataSet.toString())).thenReturn(resultjson);
            JSONObject actual=(JSONObject)createResultObject.invoke(shipAgent,dataSet,iri,cpirilist2,joforrec);
            Assert.assertEquals(expected.toString(),actual.toString());
        }
    }

    //createResultObject with iri not containing "theworldavatar"
    @Test
    public void testCreateResultObject2() throws NoSuchMethodException , InvocationTargetException, IllegalAccessException{
        ShipAgent shipAgent= new ShipAgent();
        Method createResultObject=shipAgent.getClass().getDeclaredMethod("createResultObject", JSONObject.class, String.class, ArrayList.class, JSONObject.class);
        createResultObject.setAccessible(true);

        //create mock arguments to pass
        JSONObject joforrec= new JSONObject();
        JSONObject ship= new JSONObject();
        ship.put("ss",42.1d);
        ship.put("speed",23.1d);
        ship.put("mmsi","123");
        ship.put("type","testType");
        joforrec.put("ship",ship);
        joforrec.put("reactionmechanism","testReactionMechanism");

        String shipKbURL= "testurl";
        String iri = joforrec.optString("reactionmechanism");
        String mmsi = joforrec.getJSONObject("ship").get("mmsi").toString(); //only get the mmsi instead of full iri
        String iriOfChimney = shipKbURL + mmsi + "/" + OWL_CHIMNEY + "#" + CHIMNEY;
        ArrayList<String> cpirilist2 = new ArrayList<>();
        cpirilist2.add(shipKbURL+"Engine-001.owl#Engine-001");

        JSONObject dataSet=new JSONObject();

        JSONObject jo2= new JSONObject();

        jo2.put("speed", joforrec.getJSONObject("ship").getDouble("ss"));
        jo2.put("type", joforrec.getJSONObject("ship").get("type").toString().replace("+", " "));

        String resultjson= "{key1:value1,key2:value2}";
        Object actual= new JSONObject(resultjson);

        try(MockedStatic<AgentCaller>agentCaller=Mockito.mockStatic(AgentCaller.class)){
            agentCaller.when(()->AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SLMAgent", jo2.toString())).thenReturn(resultjson);
            Object expected=createResultObject.invoke(shipAgent,dataSet,iri,cpirilist2,joforrec);
            Assert.assertEquals(expected.toString(),actual.toString());
        }
    }

    @Test
    public void testPerformConversion() throws NoSuchMethodException , InvocationTargetException, IllegalAccessException{
        ShipAgent shipAgent=new ShipAgent();
        Method performConversion=shipAgent.getClass().getDeclaredMethod("performConversion", JSONObject.class, String.class, ArrayList.class, JSONObject.class, String.class, String.class);
        performConversion.setAccessible(true);

        //create test arguments to be passed
        JSONObject jsonObject =new JSONObject();
        JSONArray pollutants= new JSONArray();
        JSONArray particle= new JSONArray();

        JSONObject emissionRate= new JSONObject();
        emissionRate.put("value",23.4d);

        JSONObject no2= new JSONObject();
        JSONObject mass_fraction= new JSONObject();
        JSONObject particle1= new JSONObject();
        JSONObject diameter= new JSONObject();
        JSONObject pdensity= new JSONObject();

        JSONObject mixture= new JSONObject();
        JSONObject molmass= new JSONObject();
        JSONObject cp= new JSONObject();
        JSONObject temperature= new JSONObject();
        JSONObject massflux= new JSONObject();
        JSONObject density= new JSONObject();


        mass_fraction.put("value","0.6");
        diameter.put("value",1d);
        pdensity.put("value",4);

        particle1.put("mass_fraction",mass_fraction);
        particle1.put("diameter",diameter);
        particle1.put("density",pdensity);
        particle1.put("emission_rate",emissionRate);
        particle.put(particle1);

        no2.put("name","NO2");
        no2.put("value",4d);
        pollutants.put(no2);

        density.put("value",12d);
        massflux.put("value",10d);
        temperature.put("value",500d);
        cp.put("value",15d);
        molmass.put("value",3d);

        mixture.put("molmass",molmass);
        mixture.put("cp",cp);
        mixture.put("temperature",temperature);
        mixture.put("massflux",massflux);
        mixture.put("density",density);

        jsonObject.put("mixture",mixture);
        jsonObject.put("pollutants",pollutants);
        jsonObject.put("particle",particle);

        HashMap<String,String> hmap=new HashMap<>();
        hmap.put("NO2","NO2value");
        hmap.put("NOx","NOXvalue");
        JSONObject joforrec= new JSONObject();
        JSONObject ship= new JSONObject();
        ship.put("ss",42.1d);
        ship.put("speed",23.1d);
        ship.put("mmsi","123");
        ship.put("type","testType");
        joforrec.put("ship",ship);
        joforrec.put("reactionmechanism","testReactionMechanism");

        String shipKbURL= "testurl";
        String iri = joforrec.optString("reactionmechanism");
        String mmsi = joforrec.getJSONObject("ship").get("mmsi").toString(); //only get the mmsi instead of full iri
        String iriofchimney = shipKbURL + mmsi + "/" + OWL_CHIMNEY + "#" + CHIMNEY;
        ArrayList<String> cpirilist2 = new ArrayList<>();
        cpirilist2.add(shipKbURL+"Engine-001.owl#Engine-001");
        JSONObject dataSet=new JSONObject();
        String resultjson= jsonObject.toString();

        JSONObject jo2= new JSONObject();

        jo2.put("speed", joforrec.getJSONObject("ship").getDouble("ss"));
        jo2.put("type", joforrec.getJSONObject("ship").get("type").toString().replace("+", " "));

        Object actual= new JSONObject(resultjson);

        OntModel ontModel=ModelFactory.createOntologyModel();
        OntClass ontClass= ontModel.createClass("http://test.org/testClass");

        //individuals for resetEmissions method
        Individual resetInd1= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "NO2value" + EM_RATE);
        Individual resetInd2= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + "NOXvalue" + EM_RATE);

        //individuals for addPropertiesToParticulate1 method

        Individual particulate1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001);
        Individual particleratevalue1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE);
        Individual particulaterate1 =ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001 + EM_RATE);

        //individuals for setPropertyValues

        Individual valueofmassflowrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_massF_WasteStream-001");
        Individual valueofdensityrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_MaterialInWasteStream-001");
        Individual valueoftemperature=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Temperature_MaterialInWasteStream-001");
        Individual valueofcombinedmolecularmass=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_ChemSpecies_Combined_MolecularMass");
        Individual valueofcombinedheatcapacity=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Cp_MaterialInWasteStream-001");

        //individuals used by createParticulateIndividuals and setPartialParticulateProperties
        int a=0;
        Individual partialparticulate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" +P001);
        Individual diameterpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001);
        Individual diametervaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
        Individual densitypartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001);
        Individual densityvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
        Individual massfractionpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001);
        Individual massfractionvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);

        //individuals for NOXEmissions method
        Individual NO2=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(no2.getString("name")) + EM_RATE);
        Individual NOX=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + EM_RATE);

        //all properties
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        Property pCPT_HASREPRPART= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");//CPT_HASREPRPART
        Property pCPT_HASLEN= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");//CPT_HASLEN
        Property pCPT_HASVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_HASDENS= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");//CPT_HASDENS
        Property pCPT_HASPROP= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP

        //all resources
        Resource rCPT_SI_M=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");//CPT_SI_M
        Resource rCPT_SI_KGPCM=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");//CPT_SI_KGPCM
        Resource rCPT_SI_GPS=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");//CPT_SI_GPS

        try (MockedStatic<BaseChimneyOntologyModelManager> baseChimOntModMan = Mockito.mockStatic(BaseChimneyOntologyModelManager.class)) {
            try(MockedStatic<BaseOntologyModelManager> baseOntModMan = Mockito.mockStatic(BaseOntologyModelManager.class)) {
                MockedStatic<LocalOntologyModelManager> localOntModMan = Mockito.mockStatic(LocalOntologyModelManager.class);
                localOntModMan.when(()->LocalOntologyModelManager.createChimneyModelForMMSI(mmsi)).thenReturn(ontModel);

                MockedStatic<AgentCaller>agentCaller=Mockito.mockStatic(AgentCaller.class);
                agentCaller.when(()->AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SLMAgent", jo2.toString())).thenReturn(resultjson);


                baseChimOntModMan.when(()->BaseChimneyOntologyModelManager.getSpeciesMap()).thenReturn(hmap);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS)).thenReturn(pCPT_HASDENS);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN)).thenReturn(pCPT_HASLEN);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASREPRPART)).thenReturn(pCPT_HASREPRPART);

                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_GPS)).thenReturn(rCPT_SI_GPS);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M)).thenReturn(rCPT_SI_M);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM)).thenReturn(rCPT_SI_KGPCM);
                baseOntModMan.when(()->LocalOntologyModelManager.save(ontModel,iriofchimney,mmsi)).thenAnswer((Answer<Void>) invocation -> null);

                performConversion.invoke(shipAgent, dataSet,iri,cpirilist2,joforrec, mmsi, iriofchimney);


                //assertion checks for the resetEmissions method
                // Since the numericalValue of these individual is overwritten later on in the NOXEmission method
                // the assertion value being tested is not 0 but the value of NO2.
                Double actualReset1 = resetInd1.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double actualReset2 = resetInd2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                Assert.assertEquals(actualReset1, 4000d, 1e-6);
                Assert.assertEquals(actualReset2, 4000d, 1e-6);

                //assertion checks for setPropertyValues method

                Double molecularvalue = valueofcombinedmolecularmass.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double Cpvalue = valueofcombinedheatcapacity.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double temperaturevalue = valueoftemperature.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double massfluxvalue = valueofmassflowrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double densityvalue = valueofdensityrate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                Assert.assertEquals(molecularvalue, 3000, 1e-6);
                Assert.assertEquals(Cpvalue, 15, 1e-6);
                Assert.assertEquals(temperaturevalue, 226.85, 1e-6);
                Assert.assertEquals(massfluxvalue, 10, 1e-6);
                Assert.assertEquals(densityvalue, 12, 1e-6);

                //assertion checks for addPropertiesToParticulate1 method
                Object expectedValue = particleratevalue1.getPropertyValue(pCPT_HASUOM);
                Assert.assertEquals(expectedValue, rCPT_SI_GPS);

                //assertion checks for updateTotalParticleEmission method
                Double expectedEmissionsValue = particleratevalue1.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Assert.assertEquals(expectedEmissionsValue, 23400, 1e-6);//taking into account the multiplication with 1000 later on in the code.

                //assertion checks for the createParticulateIndividuals method and  setPartialParticulateProperties method
                Double expectedDiameterValue = diametervaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double expectedDensityValue = densityvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                String expectedMassfractionvalue = massfractionvaluepartialparticulate.getPropertyValue(pCPT_NUMVAL).asLiteral().getString();

                Assert.assertEquals(expectedDiameterValue, 1e-9, 1e-6);
                Assert.assertEquals(expectedDensityValue, 4d, 1e-6);
                Assert.assertEquals(expectedMassfractionvalue, "0.6");


                //assertion checks for the NOXEmission method
                Double actualNO2EmmValue = NO2.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();
                Double actualNOXEmmValue = NOX.getPropertyValue(pCPT_NUMVAL).asLiteral().getDouble();

                Assert.assertEquals(4000, actualNO2EmmValue, 1e-6);
                Assert.assertEquals(4000, actualNOXEmmValue, 1e-6);
            }
        }
    }

    @Test
    public void testCreateWasteStreamIRI() throws NoSuchMethodException , InvocationTargetException, IllegalAccessException{
        ShipAgent shipAgent = new ShipAgent();
        Method createWasteStreamIRI = shipAgent.getClass().getDeclaredMethod("createWasteStreamIRI", String.class);
        createWasteStreamIRI.setAccessible(true);
        String expected="http://example.org/wasteStreamTest";

        //Creating the input stream of  which is the format of the SPARQL result set format in JSON
        String sourceString = "{" +" \"head\": { \"vars\": [ \"wasteStream\"  ]} ,"+ "\"results\": {"+ "\"bindings\": ["+ "{ \"wasteStream\": { \"type\": \"uri\" , \"value\": \"http://example.org/wasteStreamTest\" } } ] } }";
        byte[] byteArray = sourceString.getBytes();
        InputStream inputStream = new ByteArrayInputStream(byteArray);
        ResultSet rs=ResultSetFactory.fromJSON(inputStream);//mock ResultSet argument to return when the getWasteStreamInfo method is called
        OntModel ontModel = ModelFactory.createOntologyModel();
        String mmsi="123";
        try(MockedStatic<BaseChimneyOntologyModelManager> baseChimOntModMan = Mockito.mockStatic(BaseChimneyOntologyModelManager.class)) {
            try(MockedStatic<LocalOntologyModelManager> localOntModMan = Mockito.mockStatic(LocalOntologyModelManager.class)){
                localOntModMan.when(()->LocalOntologyModelManager.createChimneyModelForMMSI(mmsi)).thenReturn(ontModel);
                baseChimOntModMan.when(()->BaseChimneyOntologyModelManager.getWasteStreamInfo(ontModel)).thenReturn(rs);
                String actual=(String) createWasteStreamIRI.invoke(shipAgent,mmsi);
                Assert.assertEquals(expected,actual);
            }
        }
    }

    @Test
    public void testProcessRequestParameters(){
        ShipAgent shipAgent= new ShipAgent();

        JSONObject joforrec= new JSONObject();
        JSONObject ship= new JSONObject();
        ship.put("ss",42.1d);
        ship.put("speed",23.1d);
        ship.put("mmsi","123");
        ship.put("type","testType");
        joforrec.put("ship",ship);
        joforrec.put("reactionmechanism","testReactionMechanism");

        String baseURLpt1="http://testUrlScheme";
        String baseURLpt2="/testHost";
        String port="4567";
        String shipPath="/myShipPath";

        String baseURL=baseURLpt1+baseURLpt2+":"+port;
        String shipKbURL=baseURL+shipPath;
        String iri = joforrec.optString("reactionmechanism");
        String mmsi = joforrec.getJSONObject("ship").get("mmsi").toString(); //only get the mmsi instead of full iri
        String iriofchimney = shipKbURL + mmsi + "/" + OWL_CHIMNEY + "#" + CHIMNEY;


        JSONObject jsonObject =new JSONObject();
        JSONArray pollutants= new JSONArray();
        JSONArray particle= new JSONArray();

        JSONObject emissionRate= new JSONObject();
        emissionRate.put("value",23.4d);

        JSONObject no2= new JSONObject();
        JSONObject mass_fraction= new JSONObject();
        JSONObject particle1= new JSONObject();
        JSONObject diameter= new JSONObject();
        JSONObject pdensity= new JSONObject();

        JSONObject mixture= new JSONObject();
        JSONObject molmass= new JSONObject();
        JSONObject cp= new JSONObject();
        JSONObject temperature= new JSONObject();
        JSONObject massflux= new JSONObject();
        JSONObject density= new JSONObject();


        mass_fraction.put("value","0.6");
        diameter.put("value",1d);
        pdensity.put("value",4);

        particle1.put("mass_fraction",mass_fraction);
        particle1.put("diameter",diameter);
        particle1.put("density",pdensity);
        particle1.put("emission_rate",emissionRate);
        particle.put(particle1);

        no2.put("name","NO2");
        no2.put("value",4d);
        pollutants.put(no2);

        density.put("value",12d);
        massflux.put("value",10d);
        temperature.put("value",500d);
        cp.put("value",15d);
        molmass.put("value",3d);

        mixture.put("molmass",molmass);
        mixture.put("cp",cp);
        mixture.put("temperature",temperature);
        mixture.put("massflux",massflux);
        mixture.put("density",density);

        jsonObject.put("mixture",mixture);
        jsonObject.put("pollutants",pollutants);
        jsonObject.put("particle",particle);

        HashMap<String,String> hmap=new HashMap<>();
        hmap.put("NO2","NO2value");
        hmap.put("NOx","NOXvalue");

        String resultjson= jsonObject.toString();

        JSONObject jo2= new JSONObject();

        jo2.put("speed", joforrec.getJSONObject("ship").getDouble("ss"));
        jo2.put("type", joforrec.getJSONObject("ship").get("type").toString().replace("+", " "));

        Object actual= new JSONObject(resultjson);

        OntModel ontModel=ModelFactory.createOntologyModel();
        OntClass ontClass= ontModel.createClass(baseURLpt1+baseURLpt2);

        //individuals for resetEmissions method
        Individual resetInd1= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(hmap.keySet().toArray()[0].toString()) + EM_RATE);
        Individual resetInd2= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(hmap.keySet().toArray()[1].toString()) + EM_RATE);

        //individuals for addPropertiesToParticulate1 method

        Individual particulate1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001);
        Individual particleratevalue1=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + P001 + EM_RATE);
        Individual particulaterate1 =ontClass.createIndividual(iriofchimney.split("#")[0] + "#" + P001 + EM_RATE);

        //individuals for setPropertyValues

        Individual valueofmassflowrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_massF_WasteStream-001");
        Individual valueofdensityrate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_MaterialInWasteStream-001");
        Individual valueoftemperature=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Temperature_MaterialInWasteStream-001");
        Individual valueofcombinedmolecularmass=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_ChemSpecies_Combined_MolecularMass");
        Individual valueofcombinedheatcapacity=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Cp_MaterialInWasteStream-001");

        //individuals used by createParticulateIndividuals and setPartialParticulateProperties
        int a=0;
        Individual partialparticulate= ontClass.createIndividual(iriofchimney.split("#")[0] + "#Partial-" + a + "Of" +P001);
        Individual diameterpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Diameter_Partial-" + a + "Of" + P001);
        Individual diametervaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Diameter_Partial-" + a + "Of" + P001);
        Individual densitypartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#Density_Partial-" + a + "Of" + P001);
        Individual densityvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_Density_Partial-" + a + "Of" + P001);
        Individual massfractionpartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#MassFraction_Partial-" + a + "Of" + P001);
        Individual massfractionvaluepartialparticulate = ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_MassFraction_Partial-" + a + "Of" + P001);

        //individuals for NOXEmissions method
        Individual NO2=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(no2.getString("name")) + EM_RATE);
        Individual NOX=ontClass.createIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get("NOx") + EM_RATE);

        //all properties
        Property pCPT_NUMVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");//CPT_NUMVAL
        Property pCPT_HASREPRPART= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle");//CPT_HASREPRPART
        Property pCPT_HASLEN= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");//CPT_HASLEN
        Property pCPT_HASVAL= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");//CPT_HASVAL
        Property pCPT_HASUOM= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");//CPT_HASUOM
        Property pCPT_HASDENS= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density");//CPT_HASDENS
        Property pCPT_HASPROP= ontModel.createProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");//CPT_HASPROP

        //all resources
        Resource rCPT_SI_M=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");//CPT_SI_M
        Resource rCPT_SI_KGPCM=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");//CPT_SI_KGPCM
        Resource rCPT_SI_GPS=ontModel.createResource("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");//CPT_SI_GPS

        JSONObject expectedReqParam= new JSONObject();
        expectedReqParam.put("waste","http://example.org/wasteStreamTest");

        //Creating the input stream of  which is the format of the SPARQL result set format in JSON
        String sourceString = "{" +" \"head\": { \"vars\": [ \"wasteStream\"  ]} ,"+ "\"results\": {"+ "\"bindings\": ["+ "{ \"wasteStream\": { \"type\": \"uri\" , \"value\": \"http://example.org/wasteStreamTest\" } } ] } }";
        byte[] byteArray = sourceString.getBytes();
        InputStream inputStream = new ByteArrayInputStream(byteArray);
        ResultSet rs=ResultSetFactory.fromJSON(inputStream);

        try (MockedStatic<BaseChimneyOntologyModelManager> baseChimOntModMan = Mockito.mockStatic(BaseChimneyOntologyModelManager.class)) {
            try (MockedStatic<BaseOntologyModelManager> baseOntModMan = Mockito.mockStatic(BaseOntologyModelManager.class)) {
                MockedStatic<LocalOntologyModelManager> localOntModMan = Mockito.mockStatic(LocalOntologyModelManager.class);
                localOntModMan.when(() -> LocalOntologyModelManager.createChimneyModelForMMSI(mmsi)).thenReturn(ontModel);


                MockedStatic<AgentCaller> agentCaller = Mockito.mockStatic(AgentCaller.class);
                agentCaller.when(() -> AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SLMAgent", jo2.toString())).thenReturn(resultjson);

                MockedStatic<KeyValueManager> keyValueMan= Mockito.mockStatic(KeyValueManager.class);
                keyValueMan.when(()->KeyValueManager.get(IKeys.URL_SCHEME)).thenReturn(baseURLpt1);
                keyValueMan.when(()->KeyValueManager.get(IKeys.HOST)).thenReturn(baseURLpt2);
                keyValueMan.when(()->KeyValueManager.get(IKeys.PATH_KNOWLEDGEBASE_SHIPS)).thenReturn(shipPath);
                keyValueMan.when(()->KeyValueManager.get(IKeys.PORT)).thenReturn(port);

                baseChimOntModMan.when(()->BaseChimneyOntologyModelManager.getWasteStreamInfo(ontModel)).thenReturn(rs);
                baseChimOntModMan.when(() -> BaseChimneyOntologyModelManager.getSpeciesMap()).thenReturn(hmap);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).thenReturn(pCPT_NUMVAL);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASVAL)).thenReturn(pCPT_HASVAL);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASUOM)).thenReturn(pCPT_HASUOM);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASDENS)).thenReturn(pCPT_HASDENS);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASPROP)).thenReturn(pCPT_HASPROP);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASLEN)).thenReturn(pCPT_HASLEN);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_HASREPRPART)).thenReturn(pCPT_HASREPRPART);

                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_GPS)).thenReturn(rCPT_SI_GPS);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_M)).thenReturn(rCPT_SI_M);
                baseOntModMan.when(() -> BaseOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_SI_KGPCM)).thenReturn(rCPT_SI_KGPCM);
                baseOntModMan.when(() -> LocalOntologyModelManager.save(ontModel, iriofchimney, mmsi)).thenAnswer((Answer<Void>) invocation -> null);

                Object actualReqParam=shipAgent.processRequestParameters(joforrec);
                Assert.assertEquals(expectedReqParam.toString(),actualReqParam.toString());

            }
        }
    }
}
