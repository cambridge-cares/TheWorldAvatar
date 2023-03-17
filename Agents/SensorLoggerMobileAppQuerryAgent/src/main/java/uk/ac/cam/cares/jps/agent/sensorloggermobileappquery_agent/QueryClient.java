package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent;

import it.unibz.inf.ontop.model.vocabulary.GEO;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.GEOF;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.io.ParseException;
import org.postgis.Point;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent.objects.PersonGPSPoint;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.*;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontopStoreClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private TimeSeriesClient<OffsetDateTime> tsClient;
    private TimeSeriesClient<Instant> tsClientInstant;

    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));

    // Location type
    private static final Iri HAS_GEOMETRY = P_GEO.iri("hasGeometry");
    private static final Iri AS_WKT = P_GEO.iri("asWKT");


    static final String SLMA = "https://www.theworldavatar.com/kg/sensorloggerapp/";
    static final String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    static final String SF ="http://www.opengis.net/ont/sf#";
    static final String ONTODEVICE = "https://www.theworldavatar.com/kg/ontodevice/";
    static final String SAREF="https://saref.etsi.org/core/";
    static final String RDF="http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    final static String str_o = "o";
    final static Var VAR_O = Var.alloc(str_o);


    QueryClient(RemoteStoreClient storeClient, RemoteStoreClient ontopStoreClient, RemoteRDBStoreClient rdbStoreClient) {
        this.storeClient = storeClient;
        this.ontopStoreClient = ontopStoreClient;
        this.tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class);
        this.tsClientInstant = new TimeSeriesClient<>(storeClient, Instant.class);
        this.rdbStoreClient = rdbStoreClient;
    }


    List<PersonGPSPoint> getPersonGPSPointsWithinTimeAndScopeViaTSClient(Node smartphoneIRI, OffsetDateTime LowerBound, OffsetDateTime UpperBound, Geometry scope) {

//        Map<String,String> measureToShipMap = getMeasureToShipMap();
        List<String> measures = new ArrayList<>();
        measures.add(getIRIfromJSONarray(getPointIRIArray(smartphoneIRI)));

        List<PersonGPSPoint> personGPSPoints = new ArrayList<>();
        try (Connection conn = rdbStoreClient.getConnection()) {
            measures.stream().forEach(measure -> {

                TimeSeries<OffsetDateTime> ts = tsClient.getTimeSeriesWithinBounds(List.of(measure), LowerBound, UpperBound, conn);

                if (ts.getValuesAsPoint(measure).size() > 1) {
                    LOGGER.warn("More than 1 point within this time interval");
                } else if (ts.getValuesAsPoint(measure).isEmpty()) {
                    return;
                }

                try {
                    for (int i=0; i<ts.getValuesAsPoint(measure).size(); i++){
                        // this is to convert from org.postgis.Point to the Geometry class
                        Point postgisPoint = ts.getValuesAsPoint(measure).get(i);
                        String wktLiteral = postgisPoint.getTypeString() + postgisPoint.getValue();
                        OffsetDateTime timestamp = ts.getTimes().get(i);

                        Geometry point = new org.locationtech.jts.io.WKTReader().read(wktLiteral);

                        if (scope.covers(point)) {
                            // measureToShipMap.get(measure) gives the iri
                            PersonGPSPoint personGPSPoint = new PersonGPSPoint(measure);
                            personGPSPoint.setLocation(postgisPoint);
                            personGPSPoint.setTime(timestamp);
                            personGPSPoints.add(personGPSPoint);
                            

                        }
                    }
                } catch (ParseException e) {
                    LOGGER.error("Failed to parse WKT literal of point");
                    LOGGER.error(e.getMessage());
                    return;
                }

            });

        } catch (SQLException e) {
            LOGGER.error("Probably failed at closing connection");
            LOGGER.error(e.getMessage());
        }

        return personGPSPoints;
    }

    



    /**
     * the result is the geo:wktLiteral type with IRI of SRID in front
     * @param scopeIri
     * @return
     */
    Polygon getScopeFromOntop(String scopeIri) {
        SelectQuery query = Queries.SELECT();
        Variable scope = query.var();

        query.prefix(P_GEO).where(iri(scopeIri).has(PropertyPaths.path(HAS_GEOMETRY, AS_WKT), scope));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        String wktLiteral = queryResult.getJSONObject(0).getString(scope.getQueryString().substring(1));
        Geometry scopePolygon = WKTReader.extract(wktLiteral).getGeometry();
        scopePolygon.setSRID(4326);
        return (Polygon) scopePolygon;
    }

    String getIRIfromJSONarray(JSONArray jsonArray){
        if(jsonArray.length()!=1){System.out.println("There is more than 1 data in this JSONArray, only the first one is returned");}
        return jsonArray.getJSONObject(0).get(str_o).toString();
    }

    JSONArray getSmartPhoneIRI(String deviceID) throws org.apache.jena.sparql.lang.sparql_11.ParseException {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addWhere(VAR_O, "rdf:type", "ontodevice:Smartphone")
                .addFilter(deviceID);

        SelectBuilder sb = new SelectBuilder()
                .setDistinct(true)
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getSmartPhoneIRI()  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addWhere(VAR_O, "rdf:type", "ontodevice:Smartphone");

        SelectBuilder sb = new SelectBuilder()
                .setDistinct(true)
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }


    JSONArray getPersonIRI(String smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addWhere(VAR_O, "slma:hasA", smartphoneIRI);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getAccel_xIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?accelerometer")
                .addWhere("?accelerometer","rdf:type","ontodevice:Accelerometer")
                .addWhere("?accelerometer","ontodevice:measures","?slma_accel_x")
                .addWhere("?slma_accel_x","rdf:type","slma:Accel_x")
                .addWhere("?slma_accel_x", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getAccel_yIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?accelerometer")
                .addWhere("?accelerometer","rdf:type","ontodevice:Accelerometer")
                .addWhere("?accelerometer","ontodevice:measures","?slma_accel_y")
                .addWhere("?slma_accel_y","rdf:type","slma:Accel_y")
                .addWhere("?slma_accel_y", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getAccel_zIRIArray(Node smartphoneIRI)  {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?accelerometer")
                .addWhere("?accelerometer","rdf:type","ontodevice:Accelerometer")
                .addWhere("?accelerometer","ontodevice:measures","?slma_accel_z")
                .addWhere("?slma_accel_z","rdf:type","slma:Accel_z")
                .addWhere("?slma_accel_z", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }


    JSONArray getGravity_xIRIArray(Node smartphoneIRI)  {


        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?gravitySensor")
                .addWhere("?gravitySensor","rdf:type","ontodevice:GravitySensor")
                .addWhere("?gravitySensor","ontodevice:measures","?slma_gravity_x")
                .addWhere("?slma_gravity_x","rdf:type","slma:Gravity_x")
                .addWhere("?slma_gravity_x", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getGravity_yIRIArray(Node smartphoneIRI)  {


        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?gravitySensor")
                .addWhere("?gravitySensor","rdf:type","ontodevice:GravitySensor")
                .addWhere("?gravitySensor","ontodevice:measures","?slma_gravity_y")
                .addWhere("?slma_gravity_y","rdf:type","slma:Gravity_y")
                .addWhere("?slma_gravity_y", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getGravity_zIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?gravitySensor")
                .addWhere("?gravitySensor","rdf:type","ontodevice:GravitySensor")
                .addWhere("?gravitySensor","ontodevice:measures","?slma_gravity_z")
                .addWhere("?slma_gravity_z","rdf:type","slma:Gravity_z")
                .addWhere("?slma_gravity_z", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getMagnetometer_xIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?magnetometer")
                .addWhere("?magnetometer","rdf:type","ontodevice:Magnetometer")
                .addWhere("?magnetometer","ontodevice:measures","?slma_magnetometer_x")
                .addWhere("?slma_magnetometer_x","rdf:type","slma:Magnetometer_x")
                .addWhere("?slma_magnetometer_x", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getMagnetometer_yIRIArray(Node smartphoneIRI)  {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?magnetometer")
                .addWhere("?magnetometer","rdf:type","ontodevice:Magnetometer")
                .addWhere("?magnetometer","ontodevice:measures","?slma_magnetometer_y")
                .addWhere("?slma_magnetometer_y","rdf:type","slma:Magnetometer_y")
                .addWhere("?slma_magnetometer_y", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getMagnetometer_zIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?magnetometer")
                .addWhere("?magnetometer","rdf:type","ontodevice:Magnetometer")
                .addWhere("?magnetometer","ontodevice:measures","?slma_magnetometer_z")
                .addWhere("?slma_magnetometer_z","rdf:type","slma:Magnetometer_z")
                .addWhere("?slma_magnetometer_z", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getBearingIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                .addWhere("?GPSDevice","ontodevice:measures","?Bearing")
                .addWhere("?Bearing","rdf:type","slma:Bearing")
                .addWhere("?Bearing", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getAltitudeIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                .addWhere("?GPSDevice","ontodevice:measures","?Altitude")
                .addWhere("?Altitude","rdf:type","slma:Altitude")
                .addWhere("?Altitude", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getSpeedIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                .addWhere("?GPSDevice","ontodevice:measures","?Speed")
                .addWhere("?Speed","rdf:type","om:Speed")
                .addWhere("?Speed", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }
    JSONArray getPointIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addPrefix("sf",SF)
                .addWhere(smartphoneIRI,"saref:consistsOf","?GPSDevice")
                .addWhere("?GPSDevice","rdf:type","ontodevice:GPSDevice")
                .addWhere("?GPSDevice","ontodevice:hasGeoLocation",VAR_O)
                .addWhere(VAR_O,"rdf:type","sf:Point");

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getSoundPressureLevelIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?microphone")
                .addWhere("?microphone","rdf:type","ontodevice:Microphone")
                .addWhere("?microphone","ontodevice:measures","?om_soundPressureLevel")
                .addWhere("?om_soundPressureLevel", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getIlluminanceIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"saref:consistsOf","?camera")
                .addWhere("?camera","rdf:type","ontodevice:Camera")
                .addWhere("?camera","ontodevice:measures","?om_illuminance")
                .addWhere("?om_illuminance", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getRelativeBrightnessIRIArray(Node smartphoneIRI)  {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma",SLMA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRI,"ontodevice:hasScreenBrightness","?relativeBrightness")
                .addWhere("?relativeBrightness","rdf:type","ontodevice:RelativeBrightness")
                .addWhere("?relativeBrightness","rdf:type","?om_ratio")
                .addWhere("?om_ratio", "rdf:type", "om:Ratio")
                .addWhere("?om_ratio","om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        return queryResult;
    }











}

