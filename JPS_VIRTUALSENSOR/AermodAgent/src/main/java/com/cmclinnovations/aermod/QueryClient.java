package com.cmclinnovations.aermod;

import org.eclipse.rdf4j.model.vocabulary.GEOF;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.postgis.Point;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;

import com.cmclinnovations.aermod.sparqlbuilder.GeoSPARQL;
import com.cmclinnovations.aermod.sparqlbuilder.ValuesPattern;

import it.unibz.inf.ontop.model.vocabulary.GEO;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontopStoreClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private TimeSeriesClient<Long> tsClient;

    // prefixes
    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(Constants.PREFIX_DISP));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));
    private static final Prefix P_GEOF = SparqlBuilder.prefix("geof", iri(GEOF.NAMESPACE));
    
    // classes
    private static final Iri SHIP = P_DISP.iri("Ship");

    // properties
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri HAS_LOCATION = P_DISP.iri("hasLocation");
    private static final Iri HAS_GEOMETRY = P_GEO.iri("hasGeometry");
    private static final Iri AS_WKT = P_GEO.iri("asWKT");

    public QueryClient(RemoteStoreClient storeClient, RemoteStoreClient ontopStoreClient, RemoteRDBStoreClient rdbStoreClient) {
        this.storeClient = storeClient;
        this.ontopStoreClient = ontopStoreClient;
        this.tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        this.rdbStoreClient = rdbStoreClient;
    }

    int getMeasureValueAsInt(String instance) {
        SelectQuery query = Queries.SELECT().prefix(P_OM);
        Variable value = query.var();
        GraphPattern gp = iri(instance).has(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), value);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return Integer.parseInt(queryResult.getJSONObject(0).getString(value.getQueryString().substring(1)));
    }

    long getMeasureValueAsLong(String instance) {
        SelectQuery query = Queries.SELECT().prefix(P_OM);
        Variable value = query.var();
        GraphPattern gp = iri(instance).has(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), value);
        query.where(gp);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        return Long.parseLong(queryResult.getJSONObject(0).getString(value.getQueryString().substring(1)));
    }

    List<String> getShipsWithinTimeAndScopeViaTsClient(long simulationTime, Geometry scope) {
        long simTimeUpperBound = simulationTime + 1800; // +30 minutes
        long simTimeLowerBound = simulationTime - 1800; // -30 minutes

        Map<String,String> measureToShipMap = getMeasureToShipMap();
        List<String> measures = new ArrayList<>(measureToShipMap.keySet());

        List<String> ships = new ArrayList<>();
        try (Connection conn = rdbStoreClient.getConnection()) {
            measures.stream().forEach(measure -> {
                TimeSeries<Long> ts = tsClient.getTimeSeriesWithinBounds(List.of(measure), simTimeLowerBound, simTimeUpperBound, conn);
                if (ts.getValuesAsPoint(measure).size() > 1) {
                    LOGGER.warn("More than 1 point within this time inverval");
                } else if (ts.getValuesAsPoint(measure).isEmpty()) {
                    return;
                }

                try {
                    Point postgisPoint = ts.getValuesAsPoint(measure).get(0);
                    String wktLiteral = postgisPoint.getTypeString() + postgisPoint.getValue();

                    Geometry point = new org.locationtech.jts.io.WKTReader().read(wktLiteral);
                    
                    if (scope.contains(point)) {
                        ships.add(measureToShipMap.get(measure));
                    }
                } catch (ParseException e) {
                    LOGGER.error("Failed to parse WKT literal of point");
                    LOGGER.error(e.getMessage());
                }
                
            });
        } catch (SQLException e) {
            LOGGER.error("Probably failed at closing connection");
            LOGGER.error(e.getMessage());
        }

        return ships;
    }

    /**
     * the result is the geo:wktLiteral type with IRI of SRID in front
     * @param scopeIri
     * @return
     */
    Geometry getScopeFromOntop(String scopeIri) {
        SelectQuery query = Queries.SELECT();
        Variable scope = query.var();
        
        query.prefix(P_GEO).where(iri(scopeIri).has(PropertyPaths.path(HAS_GEOMETRY, AS_WKT), scope));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        String wktLiteral = queryResult.getJSONObject(0).getString(scope.getQueryString().substring(1));
        return WKTReader.extract(wktLiteral).getGeometry();
    }

    Map<String,String> getMeasureToShipMap() {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();
        Variable locationMeasure = query.var();
        GraphPattern gp = ship.isA(SHIP).andHas(PropertyPaths.path(HAS_LOCATION,HAS_VALUE), locationMeasure);

        query.where(gp).prefix(P_OM,P_DISP);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        Map<String,String> locationMeasureToShipMap = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String locationMeasureIri = queryResult.getJSONObject(i).getString(locationMeasure.getQueryString().substring(1));
            String shipIri = queryResult.getJSONObject(i).getString(ship.getQueryString().substring(1));
            locationMeasureToShipMap.put(locationMeasureIri, shipIri);
        }

        return locationMeasureToShipMap;
    }

    @Deprecated
    List<String> getShipsWithinTimeAndScopeViaKG(long simulationTime, String scopeIri) {
        long simTimeUpperBound = simulationTime + 1800; // +30 minutes
        long simTimeLowerBound = simulationTime - 1800; // -30 minutes

        Map<String,String> measureToShipMap = getMeasureToShipMap();
        List<String> locationMeasures = new ArrayList<>(measureToShipMap.keySet());

        Map<String, String> geometryToMeasureMap = getGeometryToMeasureMap(locationMeasures);
        List<String> geometryIrisWithinTimeBounds = getGeometriesWithinTimeBounds(new ArrayList<>(geometryToMeasureMap.keySet()), simTimeLowerBound, simTimeUpperBound);
        String scopeGeometry = getScopeGeometry(scopeIri);
        List<String> geometriesWithinScopeAndTime = getGeometriesWithinScope(scopeGeometry, geometryIrisWithinTimeBounds);

        List<String> shipsWithinTimeAndScope = new ArrayList<>();
        geometriesWithinScopeAndTime.forEach(g -> 
            shipsWithinTimeAndScope.add(measureToShipMap.get(geometryToMeasureMap.get(g)))
        );

        return shipsWithinTimeAndScope;
    }

    /**
     * to ontop
     * @param scopeIri
     */
    @Deprecated
    String getScopeGeometry(String scopeIri) {
        SelectQuery query = Queries.SELECT();
        Variable scopeGeometry = query.var();
        query.prefix(P_GEO).where(iri(scopeIri).has(HAS_GEOMETRY, scopeGeometry));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        return queryResult.getJSONObject(0).getString(scopeGeometry.getQueryString().substring(1));
    }

    /**
     * from ontop
     * @param locationMeasures
     * @return
     */
    @Deprecated
    Map<String, String> getGeometryToMeasureMap(List<String> locationMeasures) {
        SelectQuery query = Queries.SELECT();
        Variable locationMeasure = query.var();
        Variable geometry = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(locationMeasure, locationMeasures.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = locationMeasure.has(HAS_GEOMETRY,geometry);

        query.prefix(P_GEO).where(vp,gp);

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());

        Map<String, String> geometryToMeasureMap = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String geometryIri = queryResult.getJSONObject(i).getString(geometry.getQueryString().substring(1));
            String measureIri = queryResult.getJSONObject(i).getString(locationMeasure.getQueryString().substring(1));
            geometryToMeasureMap.put(geometryIri, measureIri);
        }

        return geometryToMeasureMap;
    }

    /**
     * from ontop
     * @param geometryIris
     * @param simTimeLowerBound
     * @param simTimeUpperBound
     */
    @Deprecated
    List<String> getGeometriesWithinTimeBounds(List<String> geometryIris, long simTimeLowerBound, long simTimeUpperBound) {
        SelectQuery query = Queries.SELECT();

        Variable shipTime = query.var();
        Variable geometry = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(geometry, geometryIris.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = GraphPatterns.and(vp,geometry.has(P_DISP.iri("hasTime"), shipTime));

        query.prefix(P_GEO,P_GEOF,P_OM,P_DISP).
        where(gp.filter
        (Expressions.and(Expressions.gt(shipTime, simTimeLowerBound), Expressions.lt(shipTime, simTimeUpperBound))));

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        List<String> geometriesWithinTimeBounds = new ArrayList<>();

        for (int i = 0; i < queryResult.length(); i++) {
            geometriesWithinTimeBounds.add(queryResult.getJSONObject(i).getString(geometry.getQueryString().substring(1)));
        }

        return geometriesWithinTimeBounds;
    }

    List<String> getDerivationsOfShips(List<String> ships) {
        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable ship = query.var();
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        ValuesPattern<Iri> vp = new ValuesPattern<>(ship, ships.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        GraphPattern gp = derivation.has(isDerivedFrom, ship);

        query.where(gp,vp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        
        List<String> derivations = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            derivations.add(queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1)));
        }

        return derivations;
    }

    @Deprecated
    List<String> getGeometriesWithinScope(String scopeGeometry, List<String> shipGeometriesWithinTimeBounds) {
        SelectQuery query = Queries.SELECT();
        Variable shipGeometry = query.var();
        Variable scopeWkt = query.var();
        Variable shipWkt = query.var();

        ValuesPattern<Iri> vp = new ValuesPattern<>(shipGeometry, shipGeometriesWithinTimeBounds.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);

        GraphPattern gp = GraphPatterns.and(vp, shipGeometry.has(AS_WKT, shipWkt), iri(scopeGeometry).has(AS_WKT, scopeWkt));

        query.where(gp.filter(Expressions.and(GeoSPARQL.sfIntersects(shipWkt, scopeWkt)))).prefix(P_GEO,P_GEOF);

        JSONArray queryResult = ontopStoreClient.executeQuery(query.getQueryString());
        List<String> geometriesWithinScope = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            geometriesWithinScope.add(queryResult.getJSONObject(i).getString(shipGeometry.getQueryString().substring(1)));
        }
        return geometriesWithinScope;
    }
}
