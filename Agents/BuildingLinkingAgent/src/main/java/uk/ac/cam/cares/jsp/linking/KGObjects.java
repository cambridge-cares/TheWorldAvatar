package uk.ac.cam.cares.jsp.linking;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.nio.file.Path;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class KGObjects {
    private static final Logger LOGGER = LogManager.getLogger(KGObjects.class);
    private static final Path configPath = Path.of("/inputs/config");
    static String ontoj = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#";
    static Prefix p_j1 = SparqlBuilder.prefix("j1",iri(ontoj));
    static String ontoPowsys = "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#";
    static Prefix p_powsys = SparqlBuilder.prefix("powsys",iri(ontoPowsys));
    static String bot = "https://w3id.org/bot#";
    static Prefix p_bot = SparqlBuilder.prefix("bot",iri(bot));
    static String ontobim = "https://www.theworldavatar.com/kg/ontobim/";
    static Prefix p_ontobim = SparqlBuilder.prefix("ontobim",iri(ontobim));
    RemoteStoreClient kgClient;
    private String objectIri;
    private String objectName;
    private String geoObjectId;
    private KGAddress address;
    static String envOnto = "https://www.theworldavatar.com/kg/ontobuiltenv/";
    static Prefix p_env = SparqlBuilder.prefix("env",iri(envOnto));
    static Iri hasOntoCityGML = p_env.iri("hasOntoCityGMLRepresentation");
    static Iri busNode = p_j1.iri("BusNode");
    static Iri hasBusNode = p_powsys.iri("hasBusNode");
    static Iri botBuilding = p_bot.iri("Building");
    static Iri hasIfcRepresentation = p_ontobim.iri("hasIfcRepresentation");
    static Iri IfcBuildingRepresentation = p_ontobim.iri("IfcBuildingRepresentation");
    public static final String RDFS_URL = "http://www.w3.org/2000/01/rdf-schema#";
    static Prefix rdfs = SparqlBuilder.prefix("rdfs",iri(RDFS_URL));
    static Iri labels = rdfs.iri("label");

    KGObjects(){}
    KGObjects(RemoteStoreClient kgClient, String objectIri, String objectName, String geoObjectId, KGAddress address){
        this.objectName = objectName;
        this.objectIri = objectIri;
        this.geoObjectId = geoObjectId;
        this.kgClient = kgClient;
        this.address = address;
    }

    public String getObjectIri () {
        return this.objectIri;
    }

    public String getObjectName () {return this.objectName;}
    public String getGeoObjectId () {return this.geoObjectId;}
    public void setObjectId (String objectId) {
        this.geoObjectId = objectId;
    }

    public KGAddress getAddress() {
        return this.address;
    }

    public void updateOntoCityGML () {
        if(this.getGeoObjectId() != null){
            InsertDataQuery modify = Queries.INSERT_DATA();
            Iri buildingIri = iri(this.getObjectIri());
            modify.insertData(buildingIri.has(hasOntoCityGML, this.getGeoObjectId()));
            modify.prefix(p_env);
            this.kgClient.executeUpdate(modify.getQueryString());
        }

    }
    public List<KGObjects> getAllObjects(Map<String, String> parameters) throws SQLException {
        PreparedStatement psQuery = null;
        ResultSet rs = null;
        List<KGObjects> allObjects = new ArrayList<>();

        String prefix1 = parameters.get("prefix1");
        String prefix2 = parameters.get("prefix2");
        String isA = parameters.get("isA");
        String has = parameters.get("has");

        p_j1 = SparqlBuilder.prefix("j1",iri(prefix1));
        p_powsys = SparqlBuilder.prefix("powsys",iri(prefix2));
        Iri hasProperty = p_powsys.iri(has);
        Iri isANode = p_j1.iri(isA);

        SelectQuery query = Queries.SELECT();

        Variable building = query.var();
        Variable name = query.var();
        Variable entity = query.var();
        TriplePattern[] queryPattern;
        queryPattern = new TriplePattern[]{building.isA(isANode),
                        building.has(hasProperty, entity),
                        entity.has(labels, name)};
                query.prefix(p_j1,p_powsys,rdfs).where(queryPattern).select(building,name);

//        switch (type) {
//            case "power":
//                queryPattern = new TriplePattern[]{entity.isA(busNode),
//                        building.has(hasBusNode, entity),
//                        building.has(labels, name)};
//                query.prefix(p_j1,p_powsys,rdfs).where(queryPattern).select(building,name);
//                break;
//            case "bim":
//                queryPattern = new TriplePattern[]{building.isA(botBuilding),
//                        building.has(hasIfcRepresentation, entity),
//                        entity.isA(IfcBuildingRepresentation),
//                        entity.has(labels, name)};
//                query.prefix(p_bot,p_ontobim,rdfs).where(queryPattern).select(building,name);
//                break;
//            default:

//        }

        if(kgClient == null){
            kgClient = new RemoteStoreClient();
        }
        JSONArray queryResult = this.kgClient.executeQuery(query.getQueryString());
        for(int i = 0; i< queryResult.length(); i++){
            KGObjects obj = new KGObjects();
            KGAddress address = new KGAddress();
            obj.objectName = queryResult.getJSONObject(i).toMap().get(name.getQueryString().substring(1)).toString();
            obj.objectIri =  queryResult.getJSONObject(i).toMap().get(building.getQueryString().substring(1)).toString();
            obj.kgClient = kgClient;
            obj.address = address.queryAddress(kgClient, obj.objectIri);
            allObjects.add(obj);
        }
        return allObjects;
    }
}
