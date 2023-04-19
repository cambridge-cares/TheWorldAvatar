package uk.ac.cam.cares.jsp.integration;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.semanticweb.owlapi.model.IRI;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.predicateObjectList;

public class KGObjects {
    private static final Logger LOGGER = LogManager.getLogger(KGObjects.class);

    static String ontoj = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#";
    static Prefix p_j1 = SparqlBuilder.prefix("j1",iri(ontoj));
    static String ontoPowsys = "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#";
    static Prefix p_powsys = SparqlBuilder.prefix("powsys",iri(ontoPowsys));
    RemoteStoreClient kgClient;
    private String objectIri;
    private String objectName;
    private String geoObjectId;
    static String envOnto = "http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl";
    static Prefix p_env = SparqlBuilder.prefix("env",iri(envOnto));
    static Iri hasOntoCityGML = p_env.iri("hasOntoCityGMLRepresentation");
    static Iri busNode = p_j1.iri("BusNode");
    static Iri hasBusNode = p_powsys.iri("hasBusNode");
    public static final String RDFS_URL = "http://www.w3.org/2000/01/rdf-schema#";
    static Prefix rdfs = SparqlBuilder.prefix("rdfs",iri(RDFS_URL));
    static Iri labels = rdfs.iri("label");

    KGObjects(){}
    KGObjects(RemoteStoreClient kgClient, String objectIri, String objectName, String geoObjectId){
        this.objectName = objectName;
        this.objectIri = objectIri;
        this.geoObjectId = geoObjectId;
        this.kgClient = kgClient;
    }

    public String getObjectIri () {
        return this.objectIri;
    }

    public String getObjectName () {return this.objectName;}
    public String getGeoObjectId () {return this.geoObjectId;}
    public void setObjectId (String objectId) {
        this.geoObjectId = objectId;
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
    public List<KGObjects> getAllObjects() throws SQLException {
        PreparedStatement psQuery = null;
        ResultSet rs = null;
        List<KGObjects> allObjects = new ArrayList<>();

        SelectQuery query = Queries.SELECT();

        Variable building = query.var();
        Variable name = query.var();
        Variable entity = query.var();

        TriplePattern[] queryPattern = {entity.isA(busNode),
                building.has(hasBusNode,entity),
                building.has(labels,name)};

        query.prefix(p_j1,p_powsys,rdfs).where(queryPattern).select(building,name);
        if(kgClient == null){
            kgClient = new RemoteStoreClient();
        }
        JSONArray queryResult = this.kgClient.executeQuery(query.getQueryString());
        for(int i = 0; i< queryResult.length(); i++){
            KGObjects obj = new KGObjects();
            obj.objectName = queryResult.getJSONObject(i).toMap().get(name.getQueryString().substring(1)).toString();
            obj.objectIri =  queryResult.getJSONObject(i).toMap().get(building.getQueryString().substring(1)).toString();
            obj.kgClient = kgClient;
            allObjects.add(obj);
        }
        return allObjects;
    }
}
