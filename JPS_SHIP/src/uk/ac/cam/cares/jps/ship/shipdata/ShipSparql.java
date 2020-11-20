package uk.ac.cam.cares.jps.ship.shipdata;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.OrderCondition;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatternNotTriples;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.region.Scope;

public class ShipSparql {
    String ship_endpoint = KeyValueManager.get(IKeys.URL_SHIPSQUERY);

    private static Prefix p_ship = SparqlBuilder.prefix("ship",iri("http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#"));
    private static Prefix p_system = SparqlBuilder.prefix("system",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"));
    private static Prefix p_derived_SI_unit = SparqlBuilder.prefix("derived_SI_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#"));
    private static Prefix p_space_time_extended = SparqlBuilder.prefix("space_time_extended",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"));
    private static Prefix p_space_time = SparqlBuilder.prefix("space_time",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#"));
    private static Prefix p_coordsys = SparqlBuilder.prefix("coordsys",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#"));
    private static Prefix p_time = SparqlBuilder.prefix("time",iri("http://www.w3.org/2006/time#"));
    private static Prefix p_SI_unit = SparqlBuilder.prefix("si_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#"));

    private static Iri unit_degree = p_derived_SI_unit.iri("degree");
    private Iri unit_knot = p_derived_SI_unit.iri("knot"); // not SI unit, change to m/s soon
    private static Iri unit_m = p_SI_unit.iri("m");

    /**
     * Creates a ship instance on ship_endpoint, Blazegraph is highly recommended because of its performance
     * @param i
     * @param mmsi
     * @param type
     * @param al
     * @param aw
     * @param ss
     * @param cu
     * @param lat
     * @param lon
     * @param timestamp
     */
    public void createShip(int i, int mmsi, String type, int al, int aw, double ss, double cu,
            double lat, double lon, int timestamp) {
        String ship_name = "ship"+i;
        Iri ship_iri = p_ship.iri(ship_name);
        Iri mmsi_iri = p_ship.iri(ship_name+"_MMSI");
        Iri vmmsi_iri = p_ship.iri(ship_name+"_vMMSI");

        Iri type_iri = p_ship.iri(ship_name+"_ShipType");
        Iri vtype_iri = p_ship.iri(ship_name+"_vShipType");

        Iri speed_iri = p_ship.iri(ship_name+"_SpeedOverGround");
        Iri vspeed_iri = p_ship.iri(ship_name+"_vSpeedOverGround");

        Iri course_iri = p_ship.iri(ship_name+"_CourseOverGround");
        Iri vcourse_iri = p_ship.iri(ship_name+"_vCourseOverGround");

        Iri length_iri = p_ship.iri(ship_name+"_ShipLength");
        Iri vlength_iri = p_ship.iri(ship_name+"_vShipLength");

        Iri beam_iri = p_ship.iri(ship_name+"_ShipBeam");
        Iri vbeam_iri = p_ship.iri(ship_name+"_vShipBeam");

        Iri time_iri = p_ship.iri(ship_name+"_time");

        Iri coordinates_iri = p_ship.iri(ship_name+"_coordinates");
        Iri xcoord = p_ship.iri(ship_name+"_xcoord");
        Iri ycoord = p_ship.iri(ship_name+"_ycoord");
        Iri vxcoord = p_ship.iri(ship_name+"_vxcoord");
        Iri vycoord = p_ship.iri(ship_name+"_vycoord");

        TriplePattern ship_tp = ship_iri.isA(p_ship.iri("Ship")).andHas(p_ship.iri("hasMMSI"),mmsi_iri)
                .andHas(p_ship.iri("hasShipType"),type_iri).andHas(p_ship.iri("hasSOG"),speed_iri)
                .andHas(p_ship.iri("hasCOG"), course_iri).andHas(p_system.iri("hasDimension"),length_iri)
                .andHas(p_system.iri("hasDimension"),beam_iri)
                .andHas(p_space_time_extended.iri("hasGISCoordinateSystem"),coordinates_iri);

        TriplePattern mmsi_tp = mmsi_iri.isA(p_ship.iri("MMSI")).andHas(p_system.iri("hasValue"), vmmsi_iri);
        TriplePattern vmmsi_tp = vmmsi_iri.isA(p_system.iri("Property")).andHas(p_system.iri("numericalValue"), mmsi);

        TriplePattern type_tp = type_iri.isA(p_ship.iri("ShipType")).andHas(p_system.iri("hasValue"), vtype_iri);
        TriplePattern vtype_tp = vtype_iri.isA(p_system.iri("Property")).andHas(p_system.iri("Value"),type);

        TriplePattern length_tp = length_iri.isA(p_ship.iri("ShipLength")).andHas(p_system.iri("hasValue"), vlength_iri);
        TriplePattern vlength_tp = vlength_iri.isA(p_system.iri("PhysicalDimension")).andHas(p_system.iri("QuantitativeValue"), al)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_m);

        TriplePattern beam_tp = beam_iri.isA(p_ship.iri("ShipBeam")).andHas(p_system.iri("hasValue"), vbeam_iri);
        TriplePattern vbeam_tp = vbeam_iri.isA(p_system.iri("PhysicalDimension")).andHas(p_system.iri("QuantitativeValue"), aw)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_m);

        TriplePattern speed_tp = speed_iri.isA(p_ship.iri("SpeedOverGround")).andHas(p_system.iri("hasValue"), vspeed_iri);
        TriplePattern vspeed_tp = vspeed_iri.isA(p_system.iri("ScalarQuantity")).andHas(p_system.iri("numericalValue"), ss)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_knot).andHas(p_time.iri("hasTime"),time_iri);

        TriplePattern course_tp = course_iri.isA(p_ship.iri("CourseOverGround")).andHas(p_system.iri("hasValue"), vcourse_iri);
        TriplePattern vcourse_tp = vcourse_iri.isA(p_system.iri("ScalarQuantity")).andHas(p_system.iri("numericalValue"), cu)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_degree).andHas(p_time.iri("hasTime"),time_iri);

        TriplePattern projected_gp = coordinates_iri.isA(p_space_time_extended.iri("ProjectedCoordinateSystem"))
                .andHas(p_space_time_extended.iri("hasProjectedCoordinate_x"),xcoord)
                .andHas(p_space_time_extended.iri("hasProjectedCoordinate_y"),ycoord);

        TriplePattern xcoord_tp = xcoord.isA(p_space_time.iri("AngularCoordinate")).andHas(p_system.iri("hasValue"),vxcoord);
        TriplePattern ycoord_tp = ycoord.isA(p_space_time.iri("AngularCoordinate")).andHas(p_system.iri("hasValue"),vycoord);

        TriplePattern vxcoord_tp  = vxcoord.isA(p_coordsys.iri("CoordinateValue")).andHas(p_time.iri("hasTime"),time_iri)
                .andHas(p_system.iri("numericalValue"), lon).andHas(p_system.iri("hasUnitOfMeasure"), unit_degree);
        TriplePattern vycoord_tp = vycoord.isA(p_coordsys.iri("CoordinateValue")).andHas(p_time.iri("hasTime"),time_iri)
                .andHas(p_system.iri("numericalValue"), lat).andHas(p_system.iri("hasUnitOfMeasure"), unit_degree);

        TriplePattern time_tp = time_iri.isA(p_time.iri("Instant")).andHas(p_time.iri("inTimePosition"),timestamp);

        TriplePattern [] combined_tp = {ship_tp,mmsi_tp,vmmsi_tp,type_tp,vtype_tp,length_tp,vlength_tp,beam_tp,vbeam_tp,
                speed_tp,vspeed_tp,course_tp,vcourse_tp,time_tp,projected_gp,xcoord_tp,ycoord_tp,vxcoord_tp,vycoord_tp};

        Prefix [] prefix_list = {p_ship,p_system,p_derived_SI_unit,p_space_time_extended,p_space_time,p_coordsys,p_time,p_SI_unit};
        ModifyQuery modify = Queries.MODIFY();
        modify.prefix(prefix_list).insert(combined_tp).where();

        performUpdate(ship_endpoint, modify.getQueryString());
    }

    /** 
     * Returns 300 ships located within the provided scope
     * @param sc
     * @return
     */
    public JSONArray queryShipWithinScope(Scope sc) {
        SelectQuery query = Queries.SELECT();

        // values we want to obtain
        Variable ship = SparqlBuilder.var("ship");
        Variable mmsi_value = SparqlBuilder.var("mmsi");
        Variable type = SparqlBuilder.var("type");
        Variable al = SparqlBuilder.var("al");
        Variable aw = SparqlBuilder.var("aw");
        Variable ss = SparqlBuilder.var("ss");
        Variable cu = SparqlBuilder.var("cu");
        Variable lon = SparqlBuilder.var("lon");
        Variable lat = SparqlBuilder.var("lat");

        // variables below are IRIs that we don't need
        Variable mmsi = query.var(); Variable vmmsi = query.var();
        Variable shiptype = query.var(); Variable vshiptype = query.var();
        Variable length = query.var(); Variable vlength = query.var();
        Variable beam = query.var(); Variable vbeam = query.var();
        Variable speed = query.var(); Variable vspeed = query.var();
        Variable course = query.var(); Variable vcourse = query.var();

        Variable coord = query.var();
        Variable xcoord = query.var();Variable ycoord = query.var();
        Variable vxcoord = query.var();Variable vycoord = query.var();

        GraphPattern ship_gp = GraphPatterns.and(ship.has(p_ship.iri("hasMMSI"),mmsi).andHas(p_ship.iri("hasShipType"),shiptype)
                .andHas(p_ship.iri("hasSOG"),speed).andHas(p_ship.iri("hasCOG"), course)
                .andHas(p_system.iri("hasDimension"),length).andHas(p_system.iri("hasDimension"),beam)
                .andHas(p_space_time_extended.iri("hasGISCoordinateSystem"),coord));
        
        // patterns to match for each quantity we want to query
        GraphPattern mmsi_gp = GraphPatterns.and(mmsi.has(p_system.iri("hasValue"), vmmsi),
                vmmsi.has(p_system.iri("numericalValue"), mmsi_value));

        GraphPattern type_gp = GraphPatterns.and(shiptype.has(p_system.iri("hasValue"), vshiptype),
                vshiptype.has(p_system.iri("Value"),type));

        // because ShipLength and ShipBeam both use hasDimension, isA ShipLength and isA ShipBeam are necessary here
        GraphPattern length_gp = GraphPatterns.and(length.isA(p_ship.iri("ShipLength")).andHas(p_system.iri("hasValue"),vlength),
                vlength.has(p_system.iri("QuantitativeValue"), al));

        GraphPattern beam_gp = GraphPatterns.and(beam.isA(p_ship.iri("ShipBeam")).andHas(p_system.iri("hasValue"),vbeam),
                vbeam.has(p_system.iri("QuantitativeValue"), aw));

        GraphPattern speed_gp = GraphPatterns.and(speed.has(p_system.iri("hasValue"), vspeed),
                vspeed.has(p_system.iri("numericalValue"), ss));
        
        GraphPattern course_gp = GraphPatterns.and(course.has(p_system.iri("hasValue"), vcourse),
                vcourse.has(p_system.iri("numericalValue"), cu));

        GraphPattern coordinates_gp = GraphPatterns.and(coord.has(p_space_time_extended.iri("hasProjectedCoordinate_x"),xcoord)
                .andHas(p_space_time_extended.iri("hasProjectedCoordinate_y"),ycoord),
                xcoord.has(p_system.iri("hasValue"),vxcoord),
                ycoord.has(p_system.iri("hasValue"),vycoord),
                vxcoord.has(p_system.iri("numericalValue"), lon),
                vycoord.has(p_system.iri("numericalValue"), lat));

        // constraint to ships within scope
        // ship coordinates are in EPSG:4326
        sc.transform("EPSG:4326");
        Expression<?> xconstraint = Expressions.and(Expressions.lt(lon, sc.getUpperx()),Expressions.gt(lon, sc.getLowerx()));
        Expression<?> yconstraint = Expressions.and(Expressions.lt(lat, sc.getUppery()),Expressions.gt(lat, sc.getLowery()));
        Expression<?> overallconstraint = Expressions.and(xconstraint,yconstraint);

        GraphPatternNotTriples querypattern = GraphPatterns.and(ship_gp,mmsi_gp,type_gp,length_gp,beam_gp,speed_gp,course_gp,coordinates_gp)
                .filter(overallconstraint);

        // prioritise ships that are moving and the large ones
        OrderCondition speedDesc = SparqlBuilder.desc(ss);
        OrderCondition alDesc = SparqlBuilder.desc(al);
        OrderCondition awDesc = SparqlBuilder.desc(aw);

        query.prefix(p_ship,p_space_time_extended, p_system).select(mmsi_value,type,al,aw,ss,cu,lon,lat).where(querypattern)
        .orderBy(speedDesc).orderBy(alDesc).orderBy(awDesc).limit(300);

        JSONArray result = performQuery(ship_endpoint,query.getQueryString());

        return result;
    }

    private void performUpdate(String queryEndPoint, String query) {
        RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setUpdateEndpoint(queryEndPoint);
        kbClient.setQuery(query);
        System.out.println("kbClient.executeUpdate():"+kbClient.executeUpdate());
    }

    private JSONArray performQuery(String queryEndPoint, String query) {
        RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setQueryEndpoint(queryEndPoint);
        kbClient.setQuery(query);
        JSONArray result = null;
        result = kbClient.executeQuery(); 
        return result;
    }

    public void clearEndpoint() {
        String update = "clear all";
        performUpdate(ship_endpoint, update);
    }
}