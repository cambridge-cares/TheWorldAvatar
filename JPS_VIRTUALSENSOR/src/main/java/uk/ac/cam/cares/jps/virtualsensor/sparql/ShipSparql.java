package uk.ac.cam.cares.jps.virtualsensor.sparql;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.From;
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
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.configuration.SparqlAuthentication;

public class ShipSparql {
    private static String endpoint = KeyValueManager.get(IKeys.URL_VIRTUALSENSOR);

    private static Prefix p_ship = SparqlBuilder.prefix("ship",iri("http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#"));
    private static Prefix p_system = SparqlBuilder.prefix("system",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"));
    private static Prefix p_derived_SI_unit = SparqlBuilder.prefix("derived_SI_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#"));
    private static Prefix p_space_time_extended = SparqlBuilder.prefix("space_time_extended",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"));
    private static Prefix p_space_time = SparqlBuilder.prefix("space_time",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#"));
    private static Prefix p_coordsys = SparqlBuilder.prefix("coordsys",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#"));
    private static Prefix p_time = SparqlBuilder.prefix("time",iri("http://www.w3.org/2006/time#"));
    private static Prefix p_SI_unit = SparqlBuilder.prefix("si_unit",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#"));
    private static Prefix p_plant = SparqlBuilder.prefix("plant",iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#"));
    private static Prefix p_technical = SparqlBuilder.prefix("technical",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#"));
    private static Prefix p_process = SparqlBuilder.prefix("process",iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#"));
    private static Prefix p_topology = SparqlBuilder.prefix("topology",iri("http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#"));
    private static Prefix p_chemprocess = SparqlBuilder.prefix("chemprocess",iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#"));
    private static Prefix p_behaviour = SparqlBuilder.prefix("behaviour",iri("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#"));
    private static Prefix p_phase_system = SparqlBuilder.prefix("phase",iri("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#"));
    private static Prefix p_geometry = SparqlBuilder.prefix("geometry",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#"));
    private static Prefix p_material = SparqlBuilder.prefix("material",iri("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#"));
    private static Prefix p_chemspecies = SparqlBuilder.prefix("species",iri("http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#"));
    private static Prefix p_pseudo = SparqlBuilder.prefix("pseudo",iri("http://www.theworldavatar.com/ontology/ontocape/material/substance/pseudocomponent.owl#"));
    private static Prefix p_substance = SparqlBuilder.prefix("substance", iri("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#"));
    
    // units
    private static Iri unit_degree = p_derived_SI_unit.iri("degree");
    private static Iri unit_knot = p_derived_SI_unit.iri("knot"); // not SI unit, change to m/s soon
    private static Iri unit_m = p_SI_unit.iri("m");
    private static Iri unit_JperkgK = p_derived_SI_unit.iri("J_per_kg.K"); 
    private static Iri unit_celcius = p_derived_SI_unit.iri("Celsius");
    private static Iri unit_bar = p_derived_SI_unit.iri("bar");
    private static Iri unit_g_per_s = p_derived_SI_unit.iri("kg_per_s"); 
    private static Iri unit_kg_per_mol = p_derived_SI_unit.iri("kg_per_mol");

    // chemical species
    private static Iri NO2 = p_chemspecies.iri("Nitrogen_dioxide");
    private static Iri NOx = p_pseudo.iri("Nitrogen__oxides");
    private static Iri uHC = p_pseudo.iri("Unburned_Hydrocarbon");
    private static Iri CO = p_chemspecies.iri("Carbon__monoxide");
    private static Iri CO2 = p_chemspecies.iri("Carbon__dioxide");
    private static Iri Ozone = p_chemspecies.iri("Ozone");
    private static Iri SO2 = p_chemspecies.iri("Sulfur__dioxide");
    private static Iri[] species = {NO2,NOx,uHC,CO,CO2,Ozone,SO2};
    
    private static Iri ship_graph = p_ship.iri("Ships");
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
    public static void createShip(int i, int mmsi, String type, int al, int aw, double ss, double cu,
            double lat, double lon, int timestamp) {
        String ship_name = "ship"+i;
        Iri ship_iri = p_ship.iri(ship_name);
        Iri chimney_iri = p_ship.iri(ship_name+"_Chimney");
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
                .andHas(p_space_time_extended.iri("hasGISCoordinateSystem"),coordinates_iri)
                .andHas(p_system.iri("hasSubsystem"),chimney_iri);
        
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

        combined_tp = ArrayUtils.addAll(combined_tp, GetChimneyTP(ship_name,chimney_iri));
        
        Prefix [] prefix_list = {p_ship,p_system,p_derived_SI_unit,p_space_time_extended,p_space_time,p_coordsys,p_time,p_SI_unit,p_plant,p_technical,
        		p_process,p_topology,p_chemprocess,p_behaviour,p_phase_system,p_geometry,p_material,p_chemspecies,p_pseudo,p_substance};
        ModifyQuery modify = Queries.MODIFY();
        modify.prefix(prefix_list).insert(combined_tp).where().with(ship_graph);

        performUpdate(modify);
    }

    /** 
     * Returns 300 ships located within the provided scope
     * @param sc
     * @return
     */
    public static JSONArray queryShipWithinScope(Scope sc) {
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

        // named graph
        From queryGraph = SparqlBuilder.from(ship_graph);
        
        query.from(queryGraph).prefix(p_ship,p_space_time_extended, p_system).select(mmsi_value,type,al,aw,ss,cu,lon,lat).where(querypattern)
        .orderBy(speedDesc).orderBy(alDesc).orderBy(awDesc).limit(300);

        JSONArray result = performQuery(query);

        return result;
    }

    public static JSONObject GetShipIriWithinScope(Scope sc) {
    	SelectQuery query = Queries.SELECT();

    	String shipkey = "ship";
        // ship IRI to obtain
        Variable ship = SparqlBuilder.var(shipkey);
        
        // for coordinates
        Variable lon = SparqlBuilder.var("lon"); Variable lat = SparqlBuilder.var("lat");
        Variable coord = query.var();
        
        // ship dimensions and speed for sorting
        Variable al = SparqlBuilder.var("al");
        Variable aw = SparqlBuilder.var("aw");
        Variable ss = SparqlBuilder.var("ss");
        
        GraphPattern ship_gp = ship.has(p_space_time_extended.iri("hasGISCoordinateSystem"),coord);
        
        Iri hasProjectedCoordinate_x = p_space_time_extended.iri("hasProjectedCoordinate_x");
        Iri hasProjectedCoordinate_y = p_space_time_extended.iri("hasProjectedCoordinate_y");
        Iri hasValue = p_system.iri("hasValue");
        Iri numericalValue = p_system.iri("numericalValue");
        
        // for coordinates
        Iri[] XcoordPredicates = {hasProjectedCoordinate_x,hasValue,numericalValue};
        GraphPattern xcoord_gp = SparqlPatternGenerator.GetQueryGraphPattern(query, XcoordPredicates, null, coord, lon);
        
        Iri[] YcoordPredicates = {hasProjectedCoordinate_y,hasValue,numericalValue};
        GraphPattern ycoord_gp = SparqlPatternGenerator.GetQueryGraphPattern(query, YcoordPredicates, null, coord, lat);
        
        // length query
        Iri hasDimension = p_system.iri("hasDimension");
        Iri QuantitativeValue = p_system.iri("QuantitativeValue");
        Iri[] LengthQueryPredicates = {hasDimension, hasValue, QuantitativeValue};
        Iri[] LengthQueryRdfTypes = {null, p_ship.iri("ShipLength"), null};
        GraphPattern length_gp = SparqlPatternGenerator.GetQueryGraphPattern(query, LengthQueryPredicates, LengthQueryRdfTypes, ship, al);
        
        // beam query
        Iri[] BeamQueryPredicates = {hasDimension,hasValue,QuantitativeValue};
        Iri[] BeamQueryRdfTypes = {null, p_ship.iri("ShipBeam"), null};
        GraphPattern beam_gp = SparqlPatternGenerator.GetQueryGraphPattern(query, BeamQueryPredicates, BeamQueryRdfTypes, ship, aw);
        
        // speed query
        Iri hasSOG = p_ship.iri("hasSOG");
        Iri[] SpeedQueryPredicates = {hasSOG,hasValue,numericalValue};
        GraphPattern speed_gp = SparqlPatternGenerator.GetQueryGraphPattern(query, SpeedQueryPredicates, null, ship, ss);
        
        // named graph
        From queryGraph = SparqlBuilder.from(ship_graph);
        
        // constraint to get ships within provided scope
        sc.transform("EPSG:4326");
        Expression<?> xconstraint = Expressions.and(Expressions.lt(lon, sc.getUpperx()),Expressions.gt(lon, sc.getLowerx()));
        Expression<?> yconstraint = Expressions.and(Expressions.lt(lat, sc.getUppery()),Expressions.gt(lat, sc.getLowery()));
        Expression<?> overallconstraint = Expressions.and(xconstraint,yconstraint);
       
        // prioritise ships that are moving and large ones
        OrderCondition alDesc = SparqlBuilder.desc(al);
        OrderCondition awDesc = SparqlBuilder.desc(aw);
        OrderCondition ssDesc = SparqlBuilder.desc(ss);
        
        GraphPattern queryPattern = GraphPatterns.and(ship_gp,xcoord_gp,ycoord_gp,length_gp,beam_gp,speed_gp).filter(overallconstraint);
        
        query.from(queryGraph).prefix(p_ship,p_space_time_extended, p_system).select(ship).where(queryPattern)
        .orderBy(ssDesc).orderBy(alDesc).orderBy(awDesc).limit(300);

        JSONArray queryresult = performQuery(query);
        
        JSONArray shipIRI = new JSONArray();
        for (int i=0; i < queryresult.length(); i++) {
        	shipIRI.put(queryresult.getJSONObject(i).getString(shipkey));
        }
        
        JSONObject response = new JSONObject();
        response.put(shipkey, shipIRI);
        
        return response;
    }
    
    public static JSONObject queryShipProperties(String ship_iri_string) {
    	SelectQuery query = Queries.SELECT();
        Iri ship_iri = iri(ship_iri_string);
        
        Variable type = SparqlBuilder.var("type");
        Variable ss = SparqlBuilder.var("ss");
        
        // ship type query
        Iri hasShipType = p_ship.iri("hasShipType");
        Iri hasValue = p_system.iri("hasValue");
        Iri Value = p_system.iri("Value");
        Iri[] TypeQueryPredicates = {hasShipType,hasValue,Value};
        GraphPattern type_gp = SparqlPatternGenerator.GetQueryGraphPattern(query, TypeQueryPredicates, null, ship_iri, type);
        
        // speed query
        Iri hasSOG = p_ship.iri("hasSOG");
        Iri numericalValue = p_system.iri("numericalValue");
        Iri[] SpeedQueryPredicates = {hasSOG,hasValue,numericalValue};
        GraphPattern speed_gp = SparqlPatternGenerator.GetQueryGraphPattern(query, SpeedQueryPredicates, null, ship_iri, ss);
        
        // named graph
        From queryGraph = SparqlBuilder.from(ship_graph);
        
        query.from(queryGraph).prefix(p_ship,p_system).select(type,ss).where(type_gp,speed_gp);
        JSONArray queryresult = performQuery(query);
        
        JSONObject response = queryresult.getJSONObject(0);
        return response;
    }
    
    private static TriplePattern[] GetChimneyTP(String ship_name, Iri chimney_iri) {
    	String chimney = "_Chimney";
    	Iri diameter = p_ship.iri(ship_name+chimney+"Diameter");
    	Iri v_diameter = p_ship.iri(ship_name+chimney+"DiameterValue");
    	Iri height = p_ship.iri(ship_name+chimney+"Height");
    	Iri v_height = p_ship.iri(ship_name+chimney+"HeightValue");
    	Iri release = p_ship.iri(ship_name+chimney+"ReleaseEmission");
    	Iri wasteproduct = p_ship.iri(ship_name+chimney+"WasteProduct");
    	Iri generalizedamount = p_ship.iri(ship_name+chimney+"GeneralizedAmount");
    	Iri particulateamount = p_ship.iri(ship_name+chimney+"Particulate");
    	Iri materialamount = p_ship.iri(ship_name+chimney+"MaterialAmount");
    	Iri massflow = p_ship.iri(ship_name+chimney+"MassFlow");
    	
    	Iri material = p_ship.iri(ship_name+chimney+"Material");
    	Iri singlephase = p_ship.iri(ship_name+chimney+"MaterialSinglePhase");
    	Iri cp = p_ship.iri(ship_name+chimney+"MaterialCp");
    	Iri cp_value = p_ship.iri(ship_name+chimney+"MaterialCpValue");
    	Iri temperature = p_ship.iri(ship_name+chimney+"MaterialTemperature");
    	Iri temperature_value = p_ship.iri(ship_name+chimney+"MaterialTemperatureValue");
    	Iri pressure = p_ship.iri(ship_name+chimney+"MaterialPressure");
    	Iri pressure_value = p_ship.iri(ship_name+chimney+"MaterialPressureValue");
    	Iri molWeight = p_ship.iri(ship_name+chimney+"MolecularWeight");
    	Iri molWeightValue = p_ship.iri(ship_name+chimney+"MolecularWeightValue");
    	
    	TriplePattern pipe_tp = chimney_iri.isA(p_plant.iri("Pipe"))
    			.andHas(p_plant.iri("hasInsideDiameter"),diameter)
    			.andHas(p_plant.iri("hasHeight"),height)
    			.andHas(p_technical.iri("realizes"),release);
    	
    	TriplePattern diameter_tp = diameter.isA(p_ship.iri("DimensionOfBow"));
    	TriplePattern[] v_diameter_tp = SparqlPatternGenerator.GetScalarTP(diameter,v_diameter,1.0,unit_m);
    	
    	TriplePattern height_tp = height.isA(p_ship.iri("DimensionOfBow"));
    	TriplePattern[] v_height_tp = SparqlPatternGenerator.GetScalarTP(height,v_height,20.0,unit_m);
    	
    	TriplePattern release_tp = release.isA(p_process.iri("ReleaseEmission")).andHas(p_topology.iri("hasOutput"),wasteproduct);
    	TriplePattern waste_tp = wasteproduct.isA(p_process.iri("NonReusableWasteProduct")).andHas(p_chemprocess.iri("refersToGeneralizedAmount"),generalizedamount);
    	
    	TriplePattern amount_tp = generalizedamount.isA(p_behaviour.iri("GeneralizedAmount"))
    			.andHas(p_system.iri("contains"),particulateamount)
    			.andHas(p_system.iri("hasSubsystem"),materialamount)
    			.andHas(p_system.iri("hasProperty"),massflow);
    	
    	TriplePattern materialamount_tp = materialamount.isA(p_behaviour.iri("MaterialAmount"))
    			.andHas(p_behaviour.iri("refersToMaterial"),material);
    	TriplePattern material_tp = material.isA(p_material.iri("Material")).andHas(p_material.iri("thermodynamicBehaviour"),singlephase);
    	TriplePattern singlephase_tp = singlephase.isA(p_phase_system.iri("SinglePhase"))
    			.andHas(p_system.iri("hasProperty"),cp)
    			.andHas(p_phase_system.iri("has_temperature"),temperature)
    			.andHas(p_phase_system.iri("has_pressure"),pressure)
    			.andHas(p_system.iri("hasProperty"),molWeight);
    	
    	TriplePattern cp_tp = cp.isA(p_phase_system.iri("ThermodynamicStateProperty"));
    	TriplePattern[] cpvalue_tp = SparqlPatternGenerator.GetScalarTP(cp,cp_value,0,unit_JperkgK);
    	
    	TriplePattern temperature_tp = temperature.isA(p_phase_system.iri("Temperature"));
    	TriplePattern[] tempvalue_tp = SparqlPatternGenerator.GetScalarTP(temperature,temperature_value,0,unit_celcius);
    	
    	TriplePattern pressure_tp = pressure.isA(p_phase_system.iri("Pressure"));
    	TriplePattern[] pressurevalue_tp = SparqlPatternGenerator.GetScalarTP(pressure,pressure_value,0,unit_bar);
    	
    	TriplePattern molWeight_tp = molWeight.isA(p_substance.iri("MolecularWeight"));
    	TriplePattern[] molWeightValue_tp = SparqlPatternGenerator.GetScalarTP(molWeight, molWeightValue, 0, unit_kg_per_mol);
    	
    	TriplePattern [] combined_tp = {pipe_tp,diameter_tp,height_tp,release_tp,waste_tp,
    			amount_tp,materialamount_tp,material_tp,singlephase_tp,cp_tp,temperature_tp,pressure_tp,molWeight_tp};
    	
    	// the values triples are arrays
    	combined_tp = ArrayUtils.addAll(combined_tp, v_height_tp);
    	combined_tp = ArrayUtils.addAll(combined_tp, v_diameter_tp);
    	combined_tp = ArrayUtils.addAll(combined_tp, cpvalue_tp);
    	combined_tp = ArrayUtils.addAll(combined_tp, tempvalue_tp);
    	combined_tp = ArrayUtils.addAll(combined_tp, pressurevalue_tp);
    	combined_tp = ArrayUtils.addAll(combined_tp, molWeightValue_tp);
    	
    	// 1 mixture for each species
    	for (int i=0; i < 7; i++) {
    		Iri mixture = p_ship.iri(ship_name+chimney+"Mixture"+String.valueOf(i+1));
    		Iri massFlow = p_ship.iri(ship_name+chimney+"MassFlow"+String.valueOf(i+1));
    		Iri massFlowValue = p_ship.iri(ship_name+chimney+"MassFlowValue"+String.valueOf(i+1));
    		TriplePattern[] mixture_tp = GetMixtureTP(material,mixture,species[i],massFlow,massFlowValue);
    		combined_tp = ArrayUtils.addAll(combined_tp, mixture_tp);
    	}
    	
    	for (int i = 0; i < getParticleNumber(); i++) {
    		TriplePattern[] particle_tp = getParticleTP(ship_name,particulateamount,i+1);
    		combined_tp = ArrayUtils.addAll(combined_tp, particle_tp);
    	}
    	return combined_tp;
    }
    
    /**
     * Provide graph for each particle
     * @param particulateamount
     */
    private static TriplePattern [] getParticleTP(String ship_name, Iri particulateAmount, Integer particle_index) {
    	String chimney = "_Chimney";
    	Iri particle_iri = p_ship.iri(ship_name+chimney+"Particle" + particle_index);
    	Iri massfraction_iri = p_ship.iri(ship_name+chimney+"ParticleMassFraction"+particle_index);
    	Iri vmassfraction_iri = p_ship.iri(ship_name+chimney+"ParticleMassFractionValue"+particle_index);
    	Iri density_iri = p_ship.iri(ship_name+chimney+"ParticleDensity"+particle_index);
    	Iri vdensity_iri = p_ship.iri(ship_name+chimney+"ParticleDensityValue"+particle_index);
    	Iri diameter_iri = p_ship.iri(ship_name+chimney+"ParticleDiameter"+particle_index);
    	Iri vdiameter_iri = p_ship.iri(ship_name+chimney+"ParticleDiameterValue"+particle_index);
    	
    	TriplePattern entity_tp = particulateAmount.has(p_behaviour.iri("hasRepresentativeParticle"),particle_iri);
    	
    	TriplePattern particle_tp = particle_iri.isA(p_behaviour.iri("SingleParticle"))
    			.andHas(p_system.iri("hasProperty"),massfraction_iri)
    			.andHas(p_phase_system.iri("has_density"),density_iri)
    			.andHas(p_geometry.iri("has_length"),diameter_iri);
    	
    	TriplePattern mass_tp = massfraction_iri.isA(p_phase_system.iri("MassFraction")).andHas(p_system.iri("hasValue"),vmassfraction_iri);
    	TriplePattern vmass_tp = vmassfraction_iri.isA(p_system.iri("ScalarValue")).andHas(p_system.iri("numericalValue"),0);
    	
    	TriplePattern density_tp = density_iri.isA(p_phase_system.iri("Density")).andHas(p_system.iri("hasValue"),vdensity_iri);
    	TriplePattern vdensity_tp = vdensity_iri.isA(p_system.iri("ScalarValue")).andHas(p_system.iri("numericalValue"),0);
    	
    	TriplePattern diameter_tp = diameter_iri.isA(p_geometry.iri("Diameter")).andHas(p_system.iri("hasValue"),vdiameter_iri);
    	TriplePattern vdiameter_tp = vdiameter_iri.isA(p_system.iri("ScalarValue")).andHas(p_system.iri("numericalValue"),0);
    	
    	TriplePattern [] combined_tp = {entity_tp,particle_tp,mass_tp,vmass_tp,density_tp,vdensity_tp,diameter_tp,vdiameter_tp};
    	return combined_tp;
    }
    
    /**
     * Number of particle to add
     * @return
     */
    private static int getParticleNumber() {
    	return 4;
    }
    
    /**
     * Returns triples for each waste stream, 1 waste stream for each species
     * @param Mixture
     * @param Species
     * @param MassFlow
     * @param MassFlowValue
     */
    
    private static TriplePattern[] GetMixtureTP(Iri material, Iri mixture, Iri species, Iri massFlow, Iri massFlowValue) {
    	Iri Mixture = iri("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#Mixture");
        
    	TriplePattern material_tp = material.has(p_material.iri("intrinsicCharacteristics"),mixture);
        TriplePattern mixture_tp = mixture.isA(Mixture).andHas(p_system.iri("containsDirectly"),species)
        		.andHas(p_system.iri("hasProperty"),massFlow);
        TriplePattern massFlow_tp = massFlow.isA(p_behaviour.iri("ConvectiveMassFlowrate"));
        TriplePattern[] value_tp = SparqlPatternGenerator.GetScalarTP(massFlow,massFlowValue,0.0,unit_g_per_s);
        
        TriplePattern[] combined_tp = {material_tp,mixture_tp,massFlow_tp};
        combined_tp = ArrayUtils.addAll(combined_tp, value_tp);
        return combined_tp;
    }
    
    public void UpdateShipChimney(String ship_iri,Chimney chim) {
    	
    }
    
    private static void performUpdate(ModifyQuery query) {
        RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQuery(query.getQueryString());
        System.out.println("kbClient.executeUpdate():"+kbClient.executeUpdate());
    }

    private static JSONArray performQuery(SelectQuery query) {
        RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setUser(SparqlAuthentication.getUser());
        kbClient.setPassword(SparqlAuthentication.getPassword());
        kbClient.setQueryEndpoint(endpoint);
        kbClient.setQuery(query.getQueryString());
        JSONArray result = null;
        result = kbClient.executeQuery(); 
        return result;
    }
}