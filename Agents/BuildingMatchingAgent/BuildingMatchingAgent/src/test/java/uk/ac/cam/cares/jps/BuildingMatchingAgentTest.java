package uk.ac.cam.cares.jps;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.logging.log4j.LogManager;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Polygon;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.*;


public class BuildingMatchingAgentTest {

    @Test
    public void testNewBuildingMatchingAgent(){

        BuildingMatchingAgent buildingMatchingAgent;

        try {
            buildingMatchingAgent = new BuildingMatchingAgent();
            assertNotNull(buildingMatchingAgent);
        } catch (Exception e){
            fail();
        }
    }

    @Test
    public void testNewBuildingMatchingAgentFields(){

        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();
        assertEquals(30, buildingMatchingAgent.getClass().getDeclaredFields().length);

        try {
            assertEquals("/match", buildingMatchingAgent.getClass().getDeclaredField("URI_LISTEN").get(buildingMatchingAgent));
            assertEquals("ocgml", buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent));
            assertEquals("obe", buildingMatchingAgent.getClass().getDeclaredField("KEY_OBE").get(buildingMatchingAgent));
            assertEquals("osid", buildingMatchingAgent.getClass().getDeclaredField("KEY_OSID").get(buildingMatchingAgent));
            assertEquals("prefixIRI", buildingMatchingAgent.getClass().getDeclaredField("KEY_PREFIXIRI").get(buildingMatchingAgent));

            Field LOGGER = buildingMatchingAgent.getClass().getDeclaredField("LOGGER");
            LOGGER.setAccessible(true);
            assertEquals(LOGGER.get(buildingMatchingAgent), LogManager.getLogger(BuildingMatchingAgent.class));

            Field REQUEST_RECEIVED_MSG = buildingMatchingAgent.getClass().getDeclaredField("REQUEST_RECEIVED_MSG");
            REQUEST_RECEIVED_MSG.setAccessible(true);
            assertEquals("Request received.", REQUEST_RECEIVED_MSG.get(buildingMatchingAgent));

            Field ocgmlUri = BuildingMatchingAgent.class.getDeclaredField("ocgmlUri");
            ocgmlUri.setAccessible(true);
            assertEquals("http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#", ocgmlUri.get(BuildingMatchingAgent.class));

            Field osidUri = BuildingMatchingAgent.class.getDeclaredField("osidUri");
            osidUri.setAccessible(true);
            assertEquals("http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoOSID.owl#", osidUri.get(BuildingMatchingAgent.class));

            Field obeUri = BuildingMatchingAgent.class.getDeclaredField("obeUri");
            obeUri.setAccessible(true);
            assertEquals("https://www.theworldavatar.com/kg/ontobuiltenv/", obeUri.get(BuildingMatchingAgent.class));

            Field dabgeoUri = BuildingMatchingAgent.class.getDeclaredField("dabgeoUri");
            dabgeoUri.setAccessible(true);
            assertEquals("http://www.purl.org/oema/infrastructure/", dabgeoUri.get(BuildingMatchingAgent.class));

            Field kbUri = BuildingMatchingAgent.class.getDeclaredField("kbUri");
            kbUri.setAccessible(true);
            assertEquals("https://www.theworldavatar.com/kg/ontobuiltenv/", kbUri.get(BuildingMatchingAgent.class));

            Field BLDG = BuildingMatchingAgent.class.getDeclaredField("BLDG");
            BLDG.setAccessible(true);
            assertEquals("bldg", BLDG.get(BuildingMatchingAgent.class));

            Field uprn = BuildingMatchingAgent.class.getDeclaredField("uprn");
            uprn.setAccessible(true);
            assertEquals("uprn", uprn.get(BuildingMatchingAgent.class));

            Field CITYOBJ = BuildingMatchingAgent.class.getDeclaredField("CITYOBJ");
            CITYOBJ.setAccessible(true);
            assertEquals("cityobj", CITYOBJ.get(BuildingMatchingAgent.class));

            Field QM = BuildingMatchingAgent.class.getDeclaredField("QM");
            QM.setAccessible(true);
            assertEquals("?", QM.get(BuildingMatchingAgent.class));

            Field FLAT = BuildingMatchingAgent.class.getDeclaredField("FLAT");
            FLAT.setAccessible(true);
            assertEquals("flat", FLAT.get(BuildingMatchingAgent.class));

            Field SURF_GEOM = BuildingMatchingAgent.class.getDeclaredField("SURF_GEOM");
            SURF_GEOM.setAccessible(true);
            assertEquals("surf_geom", SURF_GEOM.get(BuildingMatchingAgent.class));

            Field SURFACE_GEOMETRY = BuildingMatchingAgent.class.getDeclaredField("SURFACE_GEOMETRY");
            SURFACE_GEOMETRY.setAccessible(true);
            assertEquals("surfaceGeometry", SURFACE_GEOMETRY.get(BuildingMatchingAgent.class));

            Field GEOMETRY_TYPE = BuildingMatchingAgent.class.getDeclaredField("GEOMETRY_TYPE");
            GEOMETRY_TYPE.setAccessible(true);
            assertEquals("geometryType", GEOMETRY_TYPE.get(BuildingMatchingAgent.class));

            Field DATATYPE = BuildingMatchingAgent.class.getDeclaredField("DATATYPE");
            DATATYPE.setAccessible(true);
            assertEquals("datatype", DATATYPE.get(BuildingMatchingAgent.class));

            Field VALUE_DELIMITER = BuildingMatchingAgent.class.getDeclaredField("VALUE_DELIMITER");
            VALUE_DELIMITER.setAccessible(true);
            assertEquals("#", VALUE_DELIMITER.get(BuildingMatchingAgent.class));

            Field STRUCTURE_PREFIX = BuildingMatchingAgent.class.getDeclaredField("STRUCTURE_PREFIX");
            STRUCTURE_PREFIX.setAccessible(true);
            assertEquals("POLYGON-", STRUCTURE_PREFIX.get(BuildingMatchingAgent.class));

            Field STRUCTURE_DELIMITER = BuildingMatchingAgent.class.getDeclaredField("STRUCTURE_DELIMITER");
            STRUCTURE_DELIMITER.setAccessible(true);
            assertEquals("-", STRUCTURE_DELIMITER.get(BuildingMatchingAgent.class));

            assertEquals(BuildingMatchingAgent.class.getDeclaredField("factory").get(buildingMatchingAgent).getClass(), GeometryFactory.class);

            Field targetResourceId_ocgml = BuildingMatchingAgent.class.getDeclaredField("targetResourceId_ocgml");
            targetResourceId_ocgml.setAccessible(true);
            assertNull(targetResourceId_ocgml.get(buildingMatchingAgent));

            Field targetResourceId_obe = BuildingMatchingAgent.class.getDeclaredField("targetResourceId_obe");
            targetResourceId_obe.setAccessible(true);
            assertNull(targetResourceId_obe.get(buildingMatchingAgent));

            Field bldgGraph = BuildingMatchingAgent.class.getDeclaredField("bldgGraph");
            bldgGraph.setAccessible(true);
            assertNull(bldgGraph.get(buildingMatchingAgent));

            Field surfGeomGraph = BuildingMatchingAgent.class.getDeclaredField("surfGeomGraph");
            surfGeomGraph.setAccessible(true);
            assertNull(surfGeomGraph.get(buildingMatchingAgent));

            Field identifiersGraph = BuildingMatchingAgent.class.getDeclaredField("identifiersGraph");
            identifiersGraph.setAccessible(true);
            assertNull(identifiersGraph.get(buildingMatchingAgent));

        } catch (IllegalAccessException | NoSuchFieldException e) {
            fail();
        }
    }

    @Test
    public void testNewBuildingMatchingAgentMethods(){

        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();
        assertEquals(12, buildingMatchingAgent.getClass().getDeclaredMethods().length);

    }


    @Test
    public void testocgmlQueryBuilder(){
        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();

        try {
            Field ocgmlUri = BuildingMatchingAgent.class.getDeclaredField("ocgmlUri");
            ocgmlUri.setAccessible(true);
            Field QM = BuildingMatchingAgent.class.getDeclaredField("QM");
            QM.setAccessible(true);
            Field BLDG = BuildingMatchingAgent.class.getDeclaredField("BLDG");
            BLDG.setAccessible(true);
            Field osidUri = BuildingMatchingAgent.class.getDeclaredField("osidUri");
            osidUri.setAccessible(true);
            Field uprn = BuildingMatchingAgent.class.getDeclaredField("uprn");
            uprn.setAccessible(true);
            Field CITYOBJ = BuildingMatchingAgent.class.getDeclaredField("CITYOBJ");
            CITYOBJ.setAccessible(true);

            Field identifiersGraph = buildingMatchingAgent.getClass().getDeclaredField("identifiersGraph");
            identifiersGraph.setAccessible(true);
            Field bldgGraph = buildingMatchingAgent.getClass().getDeclaredField("bldgGraph");
            bldgGraph.setAccessible(true);
            bldgGraph.set(buildingMatchingAgent, "http://www.theworldavatar.com:83/citieskg/namespace/kings-lynn/sparql/building");
            identifiersGraph.set(buildingMatchingAgent, "http://www.theworldavatar.com:83/citieskg/namespace/kings-lynn/sparql/identifiers");

            WhereBuilder where = new WhereBuilder().addPrefix(buildingMatchingAgent.getClass().getDeclaredField("KEY_OSID").get(buildingMatchingAgent).toString(), osidUri.get(buildingMatchingAgent).toString()).addWhere(QM.get(buildingMatchingAgent).toString() + CITYOBJ.get(buildingMatchingAgent).toString(), "^"+buildingMatchingAgent.getClass().getDeclaredField("KEY_OSID").get(buildingMatchingAgent).toString() + ":intersectsFeature/"+buildingMatchingAgent.getClass().getDeclaredField("KEY_OSID").get(buildingMatchingAgent).toString()+":hasValue", QM.get(buildingMatchingAgent).toString()+uprn.get(buildingMatchingAgent).toString());
            Method ocgmlQueryBuilder = BuildingMatchingAgent.class.getDeclaredMethod("ocgmlQueryBuilder");
            ocgmlQueryBuilder.setAccessible(true);
            assertEquals(ocgmlQueryBuilder.invoke(BuildingMatchingAgent.class).toString(), new SelectBuilder().setDistinct(true)
                    .addPrefix(buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString(), ocgmlUri.get(buildingMatchingAgent).toString())
                    .addVar(QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString()).addVar(QM.get(buildingMatchingAgent).toString()+uprn.get(buildingMatchingAgent).toString())
                    .addGraph(NodeFactory.createURI(identifiersGraph.get(buildingMatchingAgent).toString()), where)
                    .addBind("IRI(REPLACE(str("+QM.get(buildingMatchingAgent).toString()+CITYOBJ.get(buildingMatchingAgent).toString()+"), \"cityobject\", \"building\"))", QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString())
                    .addGraph(NodeFactory.createURI(bldgGraph.get(buildingMatchingAgent).toString()), QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString(), buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString()+":objectClassId", "26")
                    .addOrderBy(QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString()).buildString());
            bldgGraph.set(buildingMatchingAgent, null);
            identifiersGraph.set(buildingMatchingAgent, null);
        } catch (IllegalAccessException | NoSuchFieldException | ParseException | InvocationTargetException | NoSuchMethodException e) {
            fail();
        }
    }

    @Test
    public void testobeQueryBuilder(){
        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();

        try {
            Field obeUri = BuildingMatchingAgent.class.getDeclaredField("obeUri");
            obeUri.setAccessible(true);
            Field dabgeoUri = BuildingMatchingAgent.class.getDeclaredField("dabgeoUri");
            dabgeoUri.setAccessible(true);
            Field kbUri = BuildingMatchingAgent.class.getDeclaredField("kbUri");
            kbUri.setAccessible(true);
            Field QM = BuildingMatchingAgent.class.getDeclaredField("QM");
            QM.setAccessible(true);
            Field BLDG = BuildingMatchingAgent.class.getDeclaredField("BLDG");
            BLDG.setAccessible(true);
            Field FLAT = BuildingMatchingAgent.class.getDeclaredField("FLAT");
            FLAT.setAccessible(true);
            Field uprn = BuildingMatchingAgent.class.getDeclaredField("uprn");
            uprn.setAccessible(true);

            WhereBuilder inner_where = new WhereBuilder()
                    .addPrefix("dabgeo", dabgeoUri.get(buildingMatchingAgent).toString()).addPrefix((String) buildingMatchingAgent.getClass().getDeclaredField("KEY_OBE").get(buildingMatchingAgent), obeUri.get(buildingMatchingAgent).toString())
                    .addWhere(QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString(), "a", "dabgeo:Building")
                    .addWhere(QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString(), "^obe:isIn ", QM.get(buildingMatchingAgent).toString()+FLAT.get(buildingMatchingAgent).toString())
                    .addWhere(QM.get(buildingMatchingAgent).toString()+FLAT.get(buildingMatchingAgent).toString(), "obe:hasIdentifier", QM.get(buildingMatchingAgent).toString()+uprn.get(buildingMatchingAgent).toString());

            WhereBuilder where = new WhereBuilder()
                    .addPrefix("dabgeo", dabgeoUri.get(buildingMatchingAgent).toString()).addPrefix((String) buildingMatchingAgent.getClass().getDeclaredField("KEY_OBE").get(buildingMatchingAgent), obeUri.get(buildingMatchingAgent).toString())
                    .addWhere(QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString(), "a", "dabgeo:Building")
                    .addFilter("NOT EXISTS {"+QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString()+" ^obe:isIn "+QM.get(buildingMatchingAgent).toString()+FLAT.get(buildingMatchingAgent).toString()+" }")
                    .addWhere(QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString(), "obe:hasIdentifier", QM.get(buildingMatchingAgent).toString()+uprn.get(buildingMatchingAgent).toString())
                    .addUnion(inner_where);

            Method obeQueryBuilder = BuildingMatchingAgent.class.getDeclaredMethod("obeQueryBuilder");
            obeQueryBuilder.setAccessible(true);
            assertEquals(obeQueryBuilder.invoke(BuildingMatchingAgent.class).toString(), new SelectBuilder()
                    .addPrefix("obe", obeUri.get(buildingMatchingAgent).toString()).addPrefix("dabgeo", dabgeoUri.get(buildingMatchingAgent).toString()).addPrefix("kb", kbUri.get(buildingMatchingAgent).toString())
                    .addVar(QM.get(buildingMatchingAgent).toString()+BLDG.get(buildingMatchingAgent).toString()).addVar(QM.get(buildingMatchingAgent).toString()+uprn.get(buildingMatchingAgent).toString())
                    .addWhere(where).buildString());
        } catch (IllegalAccessException | NoSuchFieldException | ParseException | InvocationTargetException | NoSuchMethodException e) {
            fail();
        }
    }

    @Test
    public void testgetGeomQuery(){
        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();

        try {
            Field QM = BuildingMatchingAgent.class.getDeclaredField("QM");
            QM.setAccessible(true);
            Field SURF_GEOM = BuildingMatchingAgent.class.getDeclaredField("SURF_GEOM");
            SURF_GEOM.setAccessible(true);
            Field SURFACE_GEOMETRY = BuildingMatchingAgent.class.getDeclaredField("SURFACE_GEOMETRY");
            SURFACE_GEOMETRY.setAccessible(true);
            Field GEOMETRY_TYPE = BuildingMatchingAgent.class.getDeclaredField("GEOMETRY_TYPE");
            GEOMETRY_TYPE.setAccessible(true);
            Field DATATYPE = BuildingMatchingAgent.class.getDeclaredField("DATATYPE");
            DATATYPE.setAccessible(true);
            Field ocgmlUri = BuildingMatchingAgent.class.getDeclaredField("ocgmlUri");
            ocgmlUri.setAccessible(true);
            String bldg = "http://www.theworldavatar.com:83/citieskg/namespace/kings-lynn/sparql/building/UUID_1/";
            Field bldgGraph = buildingMatchingAgent.getClass().getDeclaredField("bldgGraph");
            bldgGraph.setAccessible(true);
            bldgGraph.set(buildingMatchingAgent, "http://www.theworldavatar.com:83/citieskg/namespace/kings-lynn/sparql/building");
            Field surfGeomGraph = buildingMatchingAgent.getClass().getDeclaredField("surfGeomGraph");
            surfGeomGraph.setAccessible(true);
            surfGeomGraph.set(buildingMatchingAgent, "http://www.theworldavatar.com:83/citieskg/namespace/kings-lynn/sparql/surfacegeometry");

            WhereBuilder innerwhere = new WhereBuilder().addPrefix(buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString(), ocgmlUri.get(buildingMatchingAgent).toString())
                    .addWhere(QM.get(buildingMatchingAgent).toString()+SURF_GEOM.get(buildingMatchingAgent).toString(), buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString()+":parentId", QM.get(buildingMatchingAgent).toString()+SURFACE_GEOMETRY.get(buildingMatchingAgent).toString())
                    .addWhere(QM.get(buildingMatchingAgent).toString()+SURF_GEOM.get(buildingMatchingAgent).toString(), buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString()+":GeometryType", QM.get(buildingMatchingAgent).toString()+GEOMETRY_TYPE.get(buildingMatchingAgent).toString());
            WhereBuilder where1 = new WhereBuilder().addPrefix(buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString(), ocgmlUri.get(buildingMatchingAgent).toString())
                    .addGraph(NodeFactory.createURI(bldgGraph.get(buildingMatchingAgent).toString()), "<" + bldg + ">", buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString()+":lod0FootprintId", QM.get(buildingMatchingAgent).toString()+SURFACE_GEOMETRY.get(buildingMatchingAgent).toString())
                    .addGraph(NodeFactory.createURI(surfGeomGraph.get(buildingMatchingAgent).toString()), innerwhere);

            Method getGeomQuery = BuildingMatchingAgent.class.getDeclaredMethod("getGeomQuery", String.class);
            getGeomQuery.setAccessible(true);
            assertEquals(getGeomQuery.invoke(BuildingMatchingAgent.class, bldg).toString(), new SelectBuilder()
                    .addPrefix(buildingMatchingAgent.getClass().getDeclaredField("KEY_OCGML").get(buildingMatchingAgent).toString(), ocgmlUri.get(buildingMatchingAgent).toString())
                    .addVar(QM.get(buildingMatchingAgent).toString()+SURF_GEOM.get(buildingMatchingAgent).toString()).addVar(QM.get(buildingMatchingAgent).toString()+GEOMETRY_TYPE.get(buildingMatchingAgent).toString()).addVar(QM.get(buildingMatchingAgent).toString()+DATATYPE.get(buildingMatchingAgent).toString())
                    .addWhere(where1)
                    .addBind("(DATATYPE("+QM.get(buildingMatchingAgent).toString()+ GEOMETRY_TYPE.get(buildingMatchingAgent).toString()+"))", QM.get(buildingMatchingAgent).toString()+DATATYPE.get(buildingMatchingAgent).toString()).buildString());

            bldgGraph.set(buildingMatchingAgent, null);
            surfGeomGraph.set(buildingMatchingAgent, null);

        } catch (IllegalAccessException | NoSuchFieldException | ParseException | InvocationTargetException | NoSuchMethodException e) {
            fail();
        }
    }

    @Test
    public void testcreateOcgmlMapping(){

        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();
        try{
            Field BLDG = BuildingMatchingAgent.class.getDeclaredField("BLDG");
            BLDG.setAccessible(true);
            Field uprn = BuildingMatchingAgent.class.getDeclaredField("uprn");
            uprn.setAccessible(true);
            Method createOcgmlMapping = buildingMatchingAgent.getClass().getDeclaredMethod("createOcgmlMapping", JSONArray.class);
            createOcgmlMapping.setAccessible(true);
            JSONArray jsonArray = new JSONArray()
                    .put(new JSONObject().put("uprn", "uprn_1").put("bldg", "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building/UUID_1"))
                    .put(new JSONObject().put("uprn", "uprn_2").put("bldg", "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building/UUID_1/"))
                    .put(new JSONObject().put("uprn", "uprn_3").put("bldg", "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building/UUID_2/"))
                    .put(new JSONObject().put("uprn", "uprn_4").put("bldg", "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building/UUID_2/"))
                    .put(new JSONObject().put("uprn", "uprn_5").put("bldg", "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building/UUID_2/"));

            HashMap<String, String> ontocitygml = new HashMap<>();
            for (int i = 0; i < jsonArray.length(); i++) {
                String bldg = jsonArray.getJSONObject(i).getString(BLDG.get(buildingMatchingAgent).toString());
                String UPRN = jsonArray.getJSONObject(i).getString(uprn.get(buildingMatchingAgent).toString());
                ontocitygml.put(UPRN, bldg);
            }
            assertEquals(ontocitygml, createOcgmlMapping.invoke(buildingMatchingAgent, jsonArray));
        }catch (NoSuchMethodException | NoSuchFieldException | IllegalAccessException | InvocationTargetException e){
            fail();
        }
    }

    @Test
    public void testcreateObeMapping(){

        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();
        try{
            Field BLDG = BuildingMatchingAgent.class.getDeclaredField("BLDG");
            BLDG.setAccessible(true);
            Field uprn = BuildingMatchingAgent.class.getDeclaredField("uprn");
            uprn.setAccessible(true);
            Method createObeMapping = buildingMatchingAgent.getClass().getDeclaredMethod("createObeMapping", JSONArray.class);
            createObeMapping.setAccessible(true);
            JSONArray jsonArray = new JSONArray().
                    put(new JSONObject().put("uprn", "uprn_1").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_1"))
                    .put(new JSONObject().put("uprn", "uprn_2").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_2"))
                    .put(new JSONObject().put("uprn", "uprn_3").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_2"))
                    .put(new JSONObject().put("uprn", "uprn_4").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_3"))
                    .put(new JSONObject().put("uprn", "uprn_5").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_3"))
                    .put(new JSONObject().put("uprn", "uprn_6").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_3"))
                    .put(new JSONObject().put("uprn", "uprn_7").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_3"))
                    .put(new JSONObject().put("uprn", "uprn_8").put("bldg", "https://www.theworldavatar.com/kg/ontobuiltenv/Building_4"));

            HashMap<String, ArrayList> ontobuilenv = new HashMap<>();
            ArrayList<String> UPRNS_2 = new ArrayList<>();

            String subject_bldg = jsonArray.getJSONObject(0).getString(BLDG.get(buildingMatchingAgent).toString());
            UPRNS_2.add(jsonArray.getJSONObject(0).getString(uprn.get(buildingMatchingAgent).toString()));
            for (int i = 1; i < jsonArray.length(); i++) {
                String next_subject_bldg = jsonArray.getJSONObject(i).getString(BLDG.get(buildingMatchingAgent).toString());
                if (next_subject_bldg.equals(subject_bldg)) {
                    UPRNS_2.add(jsonArray.getJSONObject(i).getString(uprn.get(buildingMatchingAgent).toString()));
                }
                else {
                    ontobuilenv.put(subject_bldg, UPRNS_2);
                    UPRNS_2 = new ArrayList<>();
                    subject_bldg = next_subject_bldg;
                    UPRNS_2.add(jsonArray.getJSONObject(i).getString(uprn.get(buildingMatchingAgent).toString()));
                }
            }
            ontobuilenv.put(subject_bldg, UPRNS_2);
            assertEquals(ontobuilenv, createObeMapping.invoke(buildingMatchingAgent, jsonArray));
        }catch (NoSuchMethodException | NoSuchFieldException | IllegalAccessException | InvocationTargetException e){
            fail();
        }
    }

    @Test
    public void testgetPolygon(){

        String data = "561550.1500003919#320303.2299987612#4.5#561550.0000002965#320302.29999893194#4.5#561554.5000002959#320301.29999893234#4.5#561555.0500004868#320300.99999874254#4.5#561555.6000002006#320300.649998837#4.5#561556.0000002957#320300.3499986451#4.5#561556.8500002004#320299.3499986452#4.5#561557.400000391#320298.39999883645#4.5#561557.8840001201#320297.4659988211#4.5#561558.1000002001#320297.0499989318#4.5#561559.0500004861#320294.9499985496#4.5#561561.9100001425#320294.3299986633#4.5#561561.2190001576#320291.14699857717#4.5#561561.1000001995#320290.5999986445#4.5#561560.2000001043#320290.19999855064#4.5#561559.3500001994#320286.3999988363#4.5#561541.6500003916#320290.2599989708#4.5#561544.9000003922#320304.4499985524#4.5";
        String datatype = "http://localhost/blazegraph/literals/POLYGON-3-57";
        Polygon polygon;
        Method getPolygon = null;
        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();
        try {
            getPolygon = buildingMatchingAgent.getClass().getDeclaredMethod("getPolygon", String.class, String.class);
            getPolygon.setAccessible(true);
        }catch (NoSuchMethodException e){
           fail();
        }
        try{
            polygon = (Polygon) getPolygon.invoke(buildingMatchingAgent, data, datatype);
        }catch(Exception e){
            assertTrue(e instanceof InvocationTargetException);
        }

        data = "561579.0000002944#320304.0999986442#4.7#561575.8000004847#320291.84999864345#4.7#561569.5500004853#320293.34999864496#4.7#561570.7000001039#320298.4499985493#4.7#561558.9910004712#320301.1739985283#4.7#561550.1500003919#320303.2299987612#4.7#561544.9000003922#320304.4499985524#4.7#561541.6500003916#320290.2599989708#4.7#561533.9000003922#320291.94999855274#4.7#561536.5200002784#320303.9599987809#4.7#561538.9000003934#320314.8499986472#4.7#561542.8500002022#320313.89999883977#4.7#561542.7500002976#320313.5499989335#4.7#561545.1000002022#320312.9499985522#4.7#561544.9000003927#320312.1499988366#4.7#561550.0500004876#320310.79999893304#4.7#561579.0000002944#320304.0999986442#4.7";
        datatype = "http://localhost/blazegraph/literals/POLYGON-3-51";
        String[] valueStrings;
        try {
            GeometryFactory factory = (GeometryFactory) buildingMatchingAgent.getClass().getDeclaredField("factory").get(buildingMatchingAgent);
            Field VALUE_DELIMITER = buildingMatchingAgent.getClass().getDeclaredField("VALUE_DELIMITER");
            VALUE_DELIMITER.setAccessible(true);
            Field STRUCTURE_PREFIX = buildingMatchingAgent.getClass().getDeclaredField("STRUCTURE_PREFIX");
            STRUCTURE_PREFIX.setAccessible(true);
            Field STRUCTURE_DELIMITER = buildingMatchingAgent.getClass().getDeclaredField("STRUCTURE_DELIMITER");
            STRUCTURE_DELIMITER.setAccessible(true);
            valueStrings = data.split((String) VALUE_DELIMITER.get(buildingMatchingAgent));
            double[] values = new double[valueStrings.length];
            for (int i = 0; i < valueStrings.length; i++)
                values[i] = Double.parseDouble(valueStrings[i]);
            String[] splitDatatypeString = datatype.split((String) STRUCTURE_PREFIX.get(buildingMatchingAgent)); // ["...", "3-24-15-15"]
            String[] splitStructureString = splitDatatypeString[splitDatatypeString.length-1].split((String) STRUCTURE_DELIMITER.get(buildingMatchingAgent)); // ["3", "24", "15", "15"]
            String[] ringSizes = Arrays.copyOfRange(splitStructureString, 1, splitStructureString.length); // // ["24", "15", "15"]
            LinearRing exterior = null;
            LinearRing[] holes = new LinearRing[ringSizes.length - 1];
            int k = 0;
            try {
                for (int i = 0; i < ringSizes.length; i++) {
                    int ringSize = Integer.parseInt(ringSizes[i]) / 3;
                    Coordinate[] coords = new Coordinate[ringSize];
                    for (int j = 0; j < ringSize; j++)
                        coords[j] = new Coordinate(values[k++], values[k++], values[k++]);
                    if (i == 0) exterior = factory.createLinearRing(coords);
                    else holes[i - 1] = factory.createLinearRing(coords);
                }
            } catch (IndexOutOfBoundsException e) {
                throw new JPSRuntimeException(e);
            }
            polygon = factory.createPolygon(exterior, holes);
            assertEquals(polygon, getPolygon.invoke(buildingMatchingAgent, data, datatype));
        } catch (IllegalAccessException | NoSuchFieldException | InvocationTargetException e) {
            fail();
        }
    }

    @Test
    public void testcomputeCentroid(){
        String data = "561579.0000002944#320304.0999986442#4.7#561575.8000004847#320291.84999864345#4.7#561569.5500004853#320293.34999864496#4.7#561570.7000001039#320298.4499985493#4.7#561558.9910004712#320301.1739985283#4.7#561550.1500003919#320303.2299987612#4.7#561544.9000003922#320304.4499985524#4.7#561541.6500003916#320290.2599989708#4.7#561533.9000003922#320291.94999855274#4.7#561536.5200002784#320303.9599987809#4.7#561538.9000003934#320314.8499986472#4.7#561542.8500002022#320313.89999883977#4.7#561542.7500002976#320313.5499989335#4.7#561545.1000002022#320312.9499985522#4.7#561544.9000003927#320312.1499988366#4.7#561550.0500004876#320310.79999893304#4.7#561579.0000002944#320304.0999986442#4.7";
        String datatype = "http://localhost/blazegraph/literals/POLYGON-3-51";
        BuildingMatchingAgent buildingMatchingAgent = new BuildingMatchingAgent();
        try{
            Method getPolygon = buildingMatchingAgent.getClass().getDeclaredMethod("getPolygon", String.class, String.class);
            getPolygon.setAccessible(true);
            Polygon polygon = (Polygon) getPolygon.invoke(buildingMatchingAgent, data, datatype);
            Method computeCentroid = buildingMatchingAgent.getClass().getDeclaredMethod("computeCentroid", Coordinate[].class, boolean.class);
            computeCentroid.setAccessible(true);

            //skiplast = true
            double x = 0;
            double y = 0;
            double z = 0;
            int length = polygon.getExteriorRing().getCoordinates().length - 1;
            for (int i = 0; i < length; i++) {
                x += polygon.getExteriorRing().getCoordinates()[i].getX();
                y += polygon.getExteriorRing().getCoordinates()[i].getY();
                z += polygon.getExteriorRing().getCoordinates()[i].getZ();
            }
            assertEquals(new Coordinate(x / length, y / length, z / length), computeCentroid.invoke(buildingMatchingAgent, polygon.getExteriorRing().getCoordinates(), true));

            //skiplast = false
            x = 0;
            y = 0;
            z = 0;
            length = polygon.getExteriorRing().getCoordinates().length;
            for (int i = 0; i < length; i++) {
                x += polygon.getExteriorRing().getCoordinates()[i].getX();
                y += polygon.getExteriorRing().getCoordinates()[i].getY();
                z += polygon.getExteriorRing().getCoordinates()[i].getZ();
            }
            assertEquals(new Coordinate(x / length, y / length, z / length), computeCentroid.invoke(buildingMatchingAgent, polygon.getExteriorRing().getCoordinates(), false));
        } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e){
            fail();
        }
    }

//    @Test
//    public void test() throws ParseException {
//        //Prefix Iris
//        String ocgmlUri = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";;
//        String osidUri = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoOSID.owl#";
//
//        //Query labels
//        final String BLDG = "bldg";
//        final String uprn = "uprn";
//        final String CITYOBJ = "cityobj";
//        String ATTR = "attr";
//       String QM ="?";
//
//
//        String KEY_OCGML = "ocgml";
//        String identifiersGraph = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/identifiers";
//        String bldgGraph = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building";
//
//        WhereBuilder where1 = new WhereBuilder().addPrefix("osid", osidUri).addWhere("?cityobj", "^osid:intersectsFeature/osid:hasValue",  "?uprn");
//
//        SelectBuilder query =  new SelectBuilder().setDistinct(true)
//                .addPrefix(KEY_OCGML, ocgmlUri)
//                .addVar(QM+BLDG).addVar(QM+uprn)
//                .addGraph(NodeFactory.createURI(identifiersGraph), where1)
//                .addBind("IRI(REPLACE(str("+QM+"cityobj"+"), \"cityobject\", \"building\"))", QM+BLDG)
//                .addGraph(NodeFactory.createURI(bldgGraph+"/"), QM+BLDG, KEY_OCGML+":objectClassId", "26")
//                .addOrderBy(QM+BLDG);
//
//        String obeUri = "https://www.theworldavatar.com/kg/ontobuiltenv/";
//        String dabgeoUri = "http://www.purl.org/oema/infrastructure/";
//        String KEY_OBE = "obe";
//        String FLAT = "flat";
//        String kbUri = "https://www.theworldavatar.com/kg/ontobuiltenv/";
//        WhereBuilder inner_where = new WhereBuilder()
//                .addPrefix("dabgeo", dabgeoUri).addPrefix(KEY_OBE, obeUri)
//                .addWhere(QM+BLDG, "a", "dabgeo:Building")
//                .addWhere(QM+BLDG, "^obe:isIn ", QM+FLAT)
//                .addWhere(QM+FLAT, "obe:hasIdentifier", QM+uprn);
//
//        WhereBuilder where = new WhereBuilder()
//                .addPrefix("dabgeo", dabgeoUri).addPrefix(KEY_OBE, obeUri)
//                .addWhere(QM+BLDG, "a", "dabgeo:Building")
//                .addFilter("NOT EXISTS {"+QM+BLDG+" ^obe:isIn "+QM+FLAT+" }")
//                .addWhere(QM+BLDG, "obe:hasIdentifier", QM+uprn)
//                .addUnion(inner_where);
//
//       SelectBuilder query2 =  new SelectBuilder()
//                .addPrefix("obe", obeUri).addPrefix("dabgeo", dabgeoUri).addPrefix("kb", kbUri)
//                .addVar(QM+BLDG).addVar(QM+uprn)
//                .addWhere(where);
//    }
   }
