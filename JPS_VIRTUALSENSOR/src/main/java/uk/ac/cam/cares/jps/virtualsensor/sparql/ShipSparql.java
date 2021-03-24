package uk.ac.cam.cares.jps.virtualsensor.sparql;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Expression;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Assignment;
import org.eclipse.rdf4j.sparqlbuilder.core.From;
import org.eclipse.rdf4j.sparqlbuilder.core.OrderCondition;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.objects.Chimney;
import uk.ac.cam.cares.jps.virtualsensor.objects.Particle;

public class ShipSparql {
	public static String shipKey = "ship";
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
    private static Prefix[] Prefixes = {p_ship,p_system,p_derived_SI_unit,p_space_time_extended,p_space_time,p_coordsys,p_time,p_SI_unit,p_plant,
    		p_technical,p_process,p_topology,p_chemprocess,p_behaviour,p_phase_system,p_geometry,p_material,p_substance,p_chemspecies,p_pseudo};
    
    // units
    private static Iri unit_degree = p_derived_SI_unit.iri("degree");
    private static Iri unit_knot = p_derived_SI_unit.iri("knot"); // not SI unit, change to m/s soon
    private static Iri unit_m = p_SI_unit.iri("m");
    private static Iri unit_JperkgK = p_derived_SI_unit.iri("J_per_kg.K"); 
    private static Iri unit_celcius = p_derived_SI_unit.iri("Celsius");
    private static Iri unit_bar = p_derived_SI_unit.iri("bar");
    private static Iri unit_kg_per_s = p_derived_SI_unit.iri("kg_per_s"); 
    private static Iri unit_kg_per_mol = p_derived_SI_unit.iri("kg_per_mol");
    private static Iri unit_kg_m3 = p_derived_SI_unit.iri("kg_per_cubic_m");

    // chemical species
    private static Iri NO2 = p_chemspecies.iri("Nitrogen_dioxide");
    private static Iri NOx = p_pseudo.iri("Nitrogen__oxides");
    private static Iri uHC = p_pseudo.iri("Unburned_Hydrocarbon");
    private static Iri CO = p_chemspecies.iri("Carbon__monoxide");
    private static Iri CO2 = p_chemspecies.iri("Carbon__dioxide");
    private static Iri O3 = p_chemspecies.iri("Ozone");
    private static Iri SO2 = p_chemspecies.iri("Sulfur__dioxide");
    private static Iri[] species = {NO2,NOx,uHC,CO,CO2,O3,SO2};
    
    @SuppressWarnings("serial")
	static Map<String, Iri> speciesIriMap = new HashMap<String , Iri>() {{
    	put("CO",CO);
    	put("CO2",CO2);
    	put("NO2",NO2);
    	put("HC",uHC);
    	put("NOx", NOx);
    	put("SO2",SO2);
    	put("O3",O3);
    }};
    
    // named graph
    private static Iri ship_graph = p_ship.iri("Ships");
    private static From FromGraph = SparqlBuilder.from(ship_graph);
    
    // type 
    static Iri Ship = p_ship.iri("Ship");
    
    // relations
    static Iri hasSubsystem = p_system.iri("hasSubsystem");
	static Iri realizes = p_technical.iri("realizes");
	static Iri hasOutput = p_topology.iri("hasOutput");
	static Iri refersToGeneralizedAmount = p_chemprocess.iri("refersToGeneralizedAmount");
	static Iri refersToMaterial = p_behaviour.iri("refersToMaterial");
	static Iri thermodynamicBehaviour = p_material.iri("thermodynamicBehaviour");
	static Iri hasProperty = p_system.iri("hasProperty");
	static Iri hasValue = p_system.iri("hasValue");
	static Iri numericalValue = p_system.iri("numericalValue");
	static Iri has_temperature = p_phase_system.iri("has_temperature");
	static Iri has_density = p_phase_system.iri("has_density");
	static Iri contains = p_system.iri("contains");
	static Iri hasRepresentativeParticle = p_behaviour.iri("hasRepresentativeParticle");
	static Iri has_length = p_geometry.iri("has_length");
	static Iri hasProjectedCoordinate_x = p_space_time_extended.iri("hasProjectedCoordinate_x");
    static Iri hasProjectedCoordinate_y = p_space_time_extended.iri("hasProjectedCoordinate_y");
    static Iri hasShipType = p_ship.iri("hasShipType");
    static Iri Value = p_system.iri("Value");
    static Iri hasDimension = p_system.iri("hasDimension");
    static Iri QuantitativeValue = p_system.iri("QuantitativeValue");
    static Iri MolecularWeight = p_substance.iri("MolecularWeight");
    static Iri ConvectiveMassFlowrate = p_behaviour.iri("ConvectiveMassFlowrate");
    static Iri ThermodynamicStateProperty = p_phase_system.iri("ThermodynamicStateProperty");
    static Iri intrinsicCharacteristics = p_material.iri("intrinsicCharacteristics");
    static Iri containsDirectly = p_system.iri("containsDirectly");
    static Iri hasSOG = p_ship.iri("hasSOG");
    static Iri hasGISCoordinateSystem = p_space_time_extended.iri("hasGISCoordinateSystem");
    static Iri hasInsideDiameter = p_plant.iri("hasInsideDiameter");
    static Iri hasHeight = p_plant.iri("hasHeight");
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
    	ModifyQuery modify = Queries.MODIFY();
    	
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

        TriplePattern ship_tp = ship_iri.isA(Ship).andHas(p_ship.iri("hasMMSI"),mmsi_iri)
                .andHas(hasShipType,type_iri).andHas(hasSOG,speed_iri)
                .andHas(p_ship.iri("hasCOG"), course_iri).andHas(hasDimension,length_iri)
                .andHas(hasDimension,beam_iri)
                .andHas(hasGISCoordinateSystem,coordinates_iri)
                .andHas(hasSubsystem,chimney_iri);
        
        TriplePattern mmsi_tp = mmsi_iri.isA(p_ship.iri("MMSI")).andHas(hasValue, vmmsi_iri);
        TriplePattern vmmsi_tp = vmmsi_iri.isA(p_system.iri("Property")).andHas(numericalValue, mmsi);

        TriplePattern type_tp = type_iri.isA(p_ship.iri("ShipType")).andHas(hasValue, vtype_iri);
        TriplePattern vtype_tp = vtype_iri.isA(p_system.iri("Property")).andHas(Value,type);

        TriplePattern length_tp = length_iri.isA(p_ship.iri("ShipLength")).andHas(hasValue, vlength_iri);
        TriplePattern vlength_tp = vlength_iri.isA(p_system.iri("PhysicalDimension")).andHas(QuantitativeValue, al)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_m);

        TriplePattern beam_tp = beam_iri.isA(p_ship.iri("ShipBeam")).andHas(hasValue, vbeam_iri);
        TriplePattern vbeam_tp = vbeam_iri.isA(p_system.iri("PhysicalDimension")).andHas(QuantitativeValue, aw)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_m);

        TriplePattern speed_tp = speed_iri.isA(p_ship.iri("SpeedOverGround")).andHas(hasValue, vspeed_iri);
        TriplePattern vspeed_tp = vspeed_iri.isA(p_system.iri("ScalarQuantity")).andHas(numericalValue, ss)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_knot).andHas(p_time.iri("hasTime"),time_iri);

        TriplePattern course_tp = course_iri.isA(p_ship.iri("CourseOverGround")).andHas(hasValue, vcourse_iri);
        TriplePattern vcourse_tp = vcourse_iri.isA(p_system.iri("ScalarQuantity")).andHas(numericalValue, cu)
                .andHas(p_system.iri("hasUnitOfMeasure"), unit_degree).andHas(p_time.iri("hasTime"),time_iri);

        TriplePattern projected_gp = coordinates_iri.isA(p_space_time_extended.iri("ProjectedCoordinateSystem"))
                .andHas(hasProjectedCoordinate_x,xcoord)
                .andHas(hasProjectedCoordinate_y,ycoord);

        TriplePattern xcoord_tp = xcoord.isA(p_space_time.iri("AngularCoordinate")).andHas(hasValue,vxcoord);
        TriplePattern ycoord_tp = ycoord.isA(p_space_time.iri("AngularCoordinate")).andHas(hasValue,vycoord);

        TriplePattern vxcoord_tp  = vxcoord.isA(p_coordsys.iri("CoordinateValue")).andHas(p_time.iri("hasTime"),time_iri)
                .andHas(numericalValue, lon).andHas(p_system.iri("hasUnitOfMeasure"), unit_degree);
        TriplePattern vycoord_tp = vycoord.isA(p_coordsys.iri("CoordinateValue")).andHas(p_time.iri("hasTime"),time_iri)
                .andHas(numericalValue, lat).andHas(p_system.iri("hasUnitOfMeasure"), unit_degree);

        TriplePattern time_tp = time_iri.isA(p_time.iri("Instant")).andHas(p_time.iri("inTimePosition"),timestamp);

        TriplePattern [] combined_tp = {ship_tp,mmsi_tp,vmmsi_tp,type_tp,vtype_tp,length_tp,vlength_tp,beam_tp,vbeam_tp,
                speed_tp,vspeed_tp,course_tp,vcourse_tp,time_tp,projected_gp,xcoord_tp,ycoord_tp,vxcoord_tp,vycoord_tp};

        InsertChimneyTP(modify,ship_name,chimney_iri);
        
        Prefix [] prefix_list = {p_ship,p_system,p_derived_SI_unit,p_space_time_extended,p_space_time,p_coordsys,p_time,p_SI_unit,p_plant,p_technical,
        		p_process,p_topology,p_chemprocess,p_behaviour,p_phase_system,p_geometry,p_material,p_substance,p_chemspecies,p_pseudo};
        
        modify.prefix(prefix_list).insert(combined_tp).where().with(ship_graph);

        SparqlGeneral.performUpdate(modify);
    }

    public static String[] GetShipIriWithinScope(Scope sc) {
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
        
        GraphPattern ship_gp = ship.has(hasGISCoordinateSystem,coord);
        
        // for coordinates
        Iri[] XcoordPredicates = {hasProjectedCoordinate_x,hasValue,numericalValue};
        GraphPattern xcoord_gp = SparqlGeneral.GetQueryGraphPattern(query, XcoordPredicates, null, coord, lon);
        
        Iri[] YcoordPredicates = {hasProjectedCoordinate_y,hasValue,numericalValue};
        GraphPattern ycoord_gp = SparqlGeneral.GetQueryGraphPattern(query, YcoordPredicates, null, coord, lat);
        
        // length query
        
        Iri[] LengthQueryPredicates = {hasDimension, hasValue, QuantitativeValue};
        Iri[] LengthQueryRdfTypes = {null, p_ship.iri("ShipLength"), null};
        GraphPattern length_gp = SparqlGeneral.GetQueryGraphPattern(query, LengthQueryPredicates, LengthQueryRdfTypes, ship, al);
        
        // beam query
        Iri[] BeamQueryPredicates = {hasDimension,hasValue,QuantitativeValue};
        Iri[] BeamQueryRdfTypes = {null, p_ship.iri("ShipBeam"), null};
        GraphPattern beam_gp = SparqlGeneral.GetQueryGraphPattern(query, BeamQueryPredicates, BeamQueryRdfTypes, ship, aw);
        
        // speed query
        Iri[] SpeedQueryPredicates = {hasSOG,hasValue,numericalValue};
        GraphPattern speed_gp = SparqlGeneral.GetQueryGraphPattern(query, SpeedQueryPredicates, null, ship, ss);
        
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
        .orderBy(ssDesc).orderBy(alDesc).orderBy(awDesc).limit(30);

        JSONArray queryresult = SparqlGeneral.performQuery(query);
        
        String[] shipIRI = new String[queryresult.length()];
        for (int i=0; i < queryresult.length(); i++) {
        	shipIRI[i] = queryresult.getJSONObject(i).getString(shipkey);
        }
        
        return shipIRI;
    }
    
    public static JSONObject queryShipProperties(String ship_iri_string) {
    	SelectQuery query = Queries.SELECT();
        Iri ship_iri = iri(ship_iri_string);
        
        Variable type = SparqlBuilder.var("type");
        Variable ss = SparqlBuilder.var("ss");
        
        // ship type query
        Iri[] TypeQueryPredicates = {hasShipType,hasValue,Value};
        GraphPattern type_gp = SparqlGeneral.GetQueryGraphPattern(query, TypeQueryPredicates, null, ship_iri, type);
        
        // speed query
        Iri[] SpeedQueryPredicates = {hasSOG,hasValue,numericalValue};
        GraphPattern speed_gp = SparqlGeneral.GetQueryGraphPattern(query, SpeedQueryPredicates, null, ship_iri, ss);
        
        query.from(FromGraph).prefix(p_ship,p_system).select(type,ss).where(type_gp,speed_gp);
        JSONArray queryresult = SparqlGeneral.performQuery(query);
        
        JSONObject response = queryresult.getJSONObject(0);
        return response;
    }
    
    public static double[] queryShipCoordinates(String ship_iri_string) {
    	Iri ship_iri = iri(ship_iri_string);
    	SelectQuery query = Queries.SELECT();
    	Variable coord = query.var();
    	
    	//coord node
    	Iri[] ship2coord_predicates = {hasGISCoordinateSystem};
    	GraphPattern ship2coord_gp = SparqlGeneral.GetQueryGraphPattern(query, ship2coord_predicates,null,ship_iri,coord);
    	
    	//x node
    	Variable x = SparqlBuilder.var("x");
    	Iri[] coord2xcoord_predicates = {hasProjectedCoordinate_x,hasValue,numericalValue};
    	GraphPattern coord2xcoord_gp = SparqlGeneral.GetQueryGraphPattern(query, coord2xcoord_predicates,null,coord, x);
    	
    	//y 
    	Variable y = SparqlBuilder.var("y");
    	Iri[] coord2ycoord_predicates = {hasProjectedCoordinate_y,hasValue,numericalValue};
    	GraphPattern coord2ycoord_gp = SparqlGeneral.GetQueryGraphPattern(query, coord2ycoord_predicates, null, coord, y);
    	
    	// combined query graph
    	GraphPattern combined_gp = GraphPatterns.and(ship2coord_gp,coord2xcoord_gp,coord2ycoord_gp);
    	
    	query.prefix(p_space_time_extended,p_system,p_ship).from(FromGraph).select(x,y).where(combined_gp);
    	
    	JSONArray queryresult = SparqlGeneral.performQuery(query);
    	double xvalue = queryresult.getJSONObject(0).getDouble("x");
    	double yvalue = queryresult.getJSONObject(0).getDouble("y");
    	double[] result = {xvalue,yvalue};
    	
    	return result;
    }
    
    private static void InsertChimneyTP(ModifyQuery modify, String ship_name, Iri chimney_iri) {
    	String chimney = "_Chimney";
    	Iri diameter = p_ship.iri(ship_name+chimney+"Diameter");
    	Iri v_diameter = p_ship.iri(ship_name+chimney+"DiameterValue");
    	Iri height = p_ship.iri(ship_name+chimney+"Height");
    	Iri v_height = p_ship.iri(ship_name+chimney+"HeightValue");
    	Iri release = p_ship.iri(ship_name+chimney+"ReleaseEmission");
    	Iri wasteproduct = p_ship.iri(ship_name+chimney+"WasteProduct");
    	Iri generalizedamount = p_ship.iri(ship_name+chimney+"GeneralizedAmount");
    	Iri particulateamount = p_ship.iri(ship_name+chimney+"Particulate");
    	Iri particlerate = p_ship.iri(ship_name+chimney+"ParticulateRate");
    	Iri particleratevalue = p_ship.iri(ship_name+chimney+"ParticulateRateValue");
    	Iri materialamount = p_ship.iri(ship_name+chimney+"MaterialAmount");
    	Iri massflow = p_ship.iri(ship_name+chimney+"MassFlow");
    	Iri massflowvalue = p_ship.iri(ship_name+chimney+"MassFlowValue");
    	
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
    	Iri density = p_ship.iri(ship_name+chimney+"Density");
    	Iri densityValue = p_ship.iri(ship_name+chimney+"DensityValue");
    	
    	TriplePattern pipe_tp = chimney_iri.isA(p_plant.iri("Pipe"))
    			.andHas(hasInsideDiameter,diameter)
    			.andHas(hasHeight,height)
    			.andHas(realizes,release);
    	
    	TriplePattern diameter_tp = diameter.isA(p_ship.iri("DimensionOfBow"));
    	SparqlGeneral.InsertScalarTP(modify,diameter,v_diameter,1.0,unit_m);
    	
    	TriplePattern height_tp = height.isA(p_ship.iri("DimensionOfBow"));
    	SparqlGeneral.InsertScalarTP(modify,height,v_height,20.0,unit_m);
    	
    	TriplePattern release_tp = release.isA(p_process.iri("ReleaseEmission")).andHas(hasOutput,wasteproduct);
    	TriplePattern waste_tp = wasteproduct.isA(p_process.iri("NonReusableWasteProduct")).andHas(refersToGeneralizedAmount,generalizedamount);
    	
    	TriplePattern amount_tp = generalizedamount.isA(p_behaviour.iri("GeneralizedAmount"))
    			.andHas(contains,particulateamount)
    			.andHas(hasSubsystem,materialamount)
    			.andHas(hasProperty,massflow);
    	
    	// particle total flowrate
    	TriplePattern particulateamount_tp = particulateamount.isA(p_behaviour.iri("ParticulateMaterialAmount")).andHas(hasProperty,particlerate);
    	TriplePattern particlerate_tp = particlerate.isA(ConvectiveMassFlowrate);
    	SparqlGeneral.InsertScalarTP(modify,particlerate,particleratevalue,0,unit_kg_per_s);
    	
    	TriplePattern massflow_tp = massflow.isA(ConvectiveMassFlowrate);
    	SparqlGeneral.InsertScalarTP(modify,massflow,massflowvalue,0,unit_kg_per_s);
    	
    	TriplePattern materialamount_tp = materialamount.isA(p_behaviour.iri("MaterialAmount"))
    			.andHas(refersToMaterial,material);
    	TriplePattern material_tp = material.isA(p_material.iri("Material")).andHas(thermodynamicBehaviour,singlephase);
    	TriplePattern singlephase_tp = singlephase.isA(p_phase_system.iri("SinglePhase"))
    			.andHas(hasProperty,cp)
    			.andHas(has_temperature,temperature)
    			.andHas(p_phase_system.iri("has_pressure"),pressure)
    			.andHas(hasProperty,molWeight)
    			.andHas(has_density,density);
    	
    	TriplePattern cp_tp = cp.isA(ThermodynamicStateProperty);
    	SparqlGeneral.InsertScalarTP(modify,cp,cp_value,0,unit_JperkgK);
    	
    	TriplePattern temperature_tp = temperature.isA(p_phase_system.iri("Temperature"));
    	SparqlGeneral.InsertScalarTP(modify,temperature,temperature_value,0,unit_celcius);
    	
    	TriplePattern pressure_tp = pressure.isA(p_phase_system.iri("Pressure"));
        SparqlGeneral.InsertScalarTP(modify,pressure,pressure_value,0,unit_bar);
    	
    	TriplePattern molWeight_tp = molWeight.isA(MolecularWeight);
    	SparqlGeneral.InsertScalarTP(modify,molWeight, molWeightValue, 0, unit_kg_per_mol);
    	
    	TriplePattern density_tp = density.isA(p_phase_system.iri("Density"));
    	SparqlGeneral.InsertScalarTP(modify, density, densityValue, 0, unit_kg_m3);
    	
    	TriplePattern [] combined_tp = {pipe_tp,diameter_tp,height_tp,release_tp,waste_tp,particulateamount_tp,particlerate_tp,
    			amount_tp,massflow_tp,materialamount_tp,material_tp,singlephase_tp,cp_tp,temperature_tp,pressure_tp,molWeight_tp,density_tp};
    	
    	// 1 mixture for each species
    	for (int i=0; i < 7; i++) {
    		Iri mixture = p_ship.iri(ship_name+chimney+"Mixture"+String.valueOf(i+1));
    		Iri massFlow = p_ship.iri(ship_name+chimney+"MassFlow"+String.valueOf(i+1));
    		Iri massFlowValue = p_ship.iri(ship_name+chimney+"MassFlowValue"+String.valueOf(i+1));
    		Iri mixtureSpecies = p_ship.iri(ship_name+chimney+"MixtureSpecies"+String.valueOf(i+1));
    		InsertMixtureTP(modify,material,mixture,mixtureSpecies,species[i],massFlow,massFlowValue);
    	}
    	
    	for (int i = 0; i < getParticleNumber(); i++) {
    		InsertParticleTP(modify,ship_name,particulateamount,i+1);
    	}
    	
    	modify.insert(combined_tp);
    }
    
    /**
     * Provide graph for each particle
     * @param particulateamount
     */
    private static void InsertParticleTP(ModifyQuery modify, String ship_name, Iri particulateAmount, Integer particle_index) {
    	String chimney = "_Chimney";
    	Iri particle_iri = p_ship.iri(ship_name+chimney+"Particle" + particle_index);
    	Iri massfraction_iri = p_ship.iri(ship_name+chimney+"ParticleMassFraction"+particle_index);
    	Iri vmassfraction_iri = p_ship.iri(ship_name+chimney+"ParticleMassFractionValue"+particle_index);
    	Iri density_iri = p_ship.iri(ship_name+chimney+"ParticleDensity"+particle_index);
    	Iri vdensity_iri = p_ship.iri(ship_name+chimney+"ParticleDensityValue"+particle_index);
    	Iri diameter_iri = p_ship.iri(ship_name+chimney+"ParticleDiameter"+particle_index);
    	Iri vdiameter_iri = p_ship.iri(ship_name+chimney+"ParticleDiameterValue"+particle_index);
    	
    	TriplePattern entity_tp = particulateAmount.has(hasRepresentativeParticle,particle_iri);
    	
    	TriplePattern particle_tp = particle_iri.isA(p_behaviour.iri("SingleParticle"))
    			.andHas(hasProperty,massfraction_iri)
    			.andHas(has_density,density_iri)
    			.andHas(has_length,diameter_iri);
    	
    	TriplePattern mass_tp = massfraction_iri.isA(p_phase_system.iri("MassFraction")).andHas(hasValue,vmassfraction_iri);
    	TriplePattern vmass_tp = vmassfraction_iri.isA(p_system.iri("ScalarValue")).andHas(numericalValue,0);
    	
    	TriplePattern density_tp = density_iri.isA(p_phase_system.iri("Density"));
    	SparqlGeneral.InsertScalarTP(modify, density_iri, vdensity_iri, 0, unit_kg_m3);
    	
    	TriplePattern diameter_tp = diameter_iri.isA(p_geometry.iri("Diameter"));
    	SparqlGeneral.InsertScalarTP(modify, diameter_iri, vdiameter_iri, 0, unit_m);
    	
    	modify.insert(entity_tp,particle_tp,mass_tp,vmass_tp,density_tp,diameter_tp);
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
    
    private static void InsertMixtureTP(ModifyQuery modify,Iri material, Iri mixture, Iri species, Iri species_type, Iri massFlow, Iri massFlowValue) {
    	Iri Mixture = iri("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#Mixture");
        
    	TriplePattern material_tp = material.has(intrinsicCharacteristics,mixture);
        TriplePattern mixture_tp = mixture.isA(Mixture).andHas(containsDirectly,species)
        		.andHas(hasProperty,massFlow);
        TriplePattern species_tp = species.isA(species_type);
        TriplePattern massFlow_tp = massFlow.isA(ConvectiveMassFlowrate);
        SparqlGeneral.InsertScalarTP(modify,massFlow,massFlowValue,0.0,unit_kg_per_s);
        
        modify.insert(material_tp,mixture_tp,massFlow_tp,species_tp);
    }
    
    public static void UpdateShipChimney(String ship_iri_string,Chimney chim) {
    	// molecular weight
    	Iri ship_iri = iri(ship_iri_string);
    	
    	Iri[] molWeightPredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,hasSubsystem,
    			refersToMaterial,thermodynamicBehaviour,hasProperty,hasValue,numericalValue};
    	Iri[] molWeightRdfTypes = new Iri[molWeightPredicates.length+1];
    	molWeightRdfTypes[8] = MolecularWeight;
    	
    	SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, molWeightPredicates, molWeightRdfTypes, ship_iri, chim.getMixtureMolWeight(), ship_graph));
    	
    	//cp
    	Iri[] cpPredicates = molWeightPredicates;
    	Iri[] cpRdfTypes = new Iri[cpPredicates.length+1];
    	cpRdfTypes[8] = ThermodynamicStateProperty;
    	SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, cpPredicates, cpRdfTypes, ship_iri, chim.getMixtureCp(), ship_graph));
    	
    	//temperature
    	Iri[] tempPredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,hasSubsystem,
    			refersToMaterial,thermodynamicBehaviour,has_temperature,hasValue,numericalValue};
    	SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, tempPredicates, null, ship_iri, chim.getMixtureTemperature(), ship_graph));
    	
    	// mass flux
    	Iri[] massFluxPredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,hasProperty,hasValue,numericalValue};
    	Iri[] massFluxRdfTypes = new Iri[massFluxPredicates.length+1];
    	massFluxRdfTypes[5] = ConvectiveMassFlowrate;
    	SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, massFluxPredicates, massFluxRdfTypes, ship_iri, chim.getMixtureMassFlux(), ship_graph));
    	
    	// density
    	Iri[] densityPredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,hasSubsystem,
    			refersToMaterial,thermodynamicBehaviour,has_density,hasValue,numericalValue};
    	SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, densityPredicates, null, ship_iri, chim.getMixtureDensity(), ship_graph));
    
    	// particles
    	// first query the IRIs of the representative particles, it is assumed that the number is always the same with the ones in chimney
    	Iri [] particlePredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,contains,hasRepresentativeParticle};
    	SelectQuery query = Queries.SELECT();
    	String particlekey = "particleIRI";
    	Variable particleIRI = SparqlBuilder.var(particlekey);
    	GraphPattern queryPattern = SparqlGeneral.GetQueryGraphPattern(query, particlePredicates, null, ship_iri, particleIRI);
    	query.prefix(Prefixes).select(particleIRI).where(queryPattern).from(FromGraph);
    	JSONArray particles = SparqlGeneral.performQuery(query);
    	
    	// then update each particle
    	if (particles.length() == chim.getNumpar()) {
    		for (int i = 0; i < particles.length(); i++) {
    			Iri particle_iri = iri(particles.getJSONObject(i).getString(particlekey));
    			
    			// update density
    			Iri[] partDenPredicates = {has_density,hasValue,numericalValue};
    			SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, partDenPredicates, null, particle_iri, chim.getParticle(i).getDensity(), ship_graph));
    			
    			// update diameter
    			Iri[] partDiameterPredicates = {has_length,hasValue,numericalValue};
    			SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, partDiameterPredicates, null, particle_iri, chim.getParticle(i).getDiameter(), ship_graph));
    		    
    			// update mass fraction
    			Iri[] partmassPredicates = {hasProperty,hasValue,numericalValue};
    			SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, partmassPredicates, null, particle_iri, chim.getParticle(i).getMassFraction(), ship_graph));
    		}
    	}
    	
    	// overall particulate flow rate
    	Iri[] particleRatePredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,contains,hasProperty,hasValue,numericalValue};
    	SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, particleRatePredicates, null, ship_iri, chim.getTotalParticleFlowrate(), ship_graph));
    
    	// gas phase
    	for (int i=0; i < chim.getNumpol(); i++) {
    		String speciesname = chim.getPollutant(i).getSpecies();
    		
    		if (speciesIriMap.get(speciesname) != null) {
    			Iri speciesIri = speciesIriMap.get(speciesname);
    			
    			// first query the mixture IRI containing the species
    			SelectQuery MixtureQuery = Queries.SELECT();
    			Variable mixture = SparqlBuilder.var("mixture");
    			
    			Iri[] mixturePredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,hasSubsystem,refersToMaterial,intrinsicCharacteristics};
    			GraphPattern ship2mixture_gp = SparqlGeneral.GetQueryGraphPattern(MixtureQuery,mixturePredicates,null,ship_iri,mixture);
    			
    			Iri[] speciesPredicates = {containsDirectly};
    			Iri[] speciesRdfTypes = new Iri[speciesPredicates.length+1];
    			speciesRdfTypes[1] = speciesIri;
    			GraphPattern mixture2species_gp = SparqlGeneral.GetQueryGraphPattern(MixtureQuery,speciesPredicates,speciesRdfTypes,mixture);
    			
    			GraphPattern mixtureQueryPattern = GraphPatterns.and(ship2mixture_gp,mixture2species_gp);
    			MixtureQuery.select(mixture).where(mixtureQueryPattern).from(FromGraph).prefix(Prefixes);
    			
    			JSONArray mixtureQueryResult = SparqlGeneral.performQuery(MixtureQuery);
 
    			Iri MixtureIri = iri(mixtureQueryResult.getJSONObject(0).getString("mixture"));
    			Iri[] flowratePredicates = {hasProperty,hasValue,numericalValue};
    			SparqlGeneral.performUpdate(SparqlGeneral.UpdateValue(Prefixes, flowratePredicates, null, MixtureIri, chim.getPollutant(i).getFlowrate(), ship_graph));
    		}
    	}
    }
    
    public static JSONArray QueryChimneyProperties(String ship_iri_string) {
    	Iri ship_iri = iri(ship_iri_string);
    	SelectQuery query = Queries.SELECT();
    	
    	// values we want to query
    	Variable diameter = SparqlBuilder.var("diameter");
    	Variable overallFlowrate = SparqlBuilder.var("overallFlowrate");
    	Variable density = SparqlBuilder.var("density");
    	Variable height = SparqlBuilder.var("height");
    	Variable temp = SparqlBuilder.var("temp");
    	Variable particleFlowrate = SparqlBuilder.var("particleFlowrate");
    	
    	// intermediate variables that intersect in the graph query
    	Variable chimney = query.var(); Variable generalizeAmount = query.var(); Variable singlephase = query.var();
    	
    	Iri[] chimneyPredicates = {hasSubsystem};
    	GraphPattern ship2chimney = SparqlGeneral.GetQueryGraphPattern(query, chimneyPredicates, null, ship_iri,chimney);
    	
    	Iri[] diameterPredicates = {hasInsideDiameter,hasValue,numericalValue};
    	GraphPattern chimney2diameter = SparqlGeneral.GetQueryGraphPattern(query, diameterPredicates, null, chimney, diameter);
    	
    	Iri[] generalPredicates = {realizes,hasOutput,refersToGeneralizedAmount};
    	GraphPattern chimney2general = SparqlGeneral.GetQueryGraphPattern(query, generalPredicates, null, chimney, generalizeAmount);
    	
    	Iri[] flowratePredicates = {hasProperty,hasValue,numericalValue};
    	GraphPattern general2flowrate = SparqlGeneral.GetQueryGraphPattern(query, flowratePredicates, null, generalizeAmount,overallFlowrate);
    	
    	Iri[] singlephasePredicates = {hasSubsystem,refersToMaterial,thermodynamicBehaviour};
    	GraphPattern general2singlephase = SparqlGeneral.GetQueryGraphPattern(query, singlephasePredicates, null, generalizeAmount,singlephase);
    	
    	Iri[] particlePredicates = {contains,hasProperty,hasValue,numericalValue};
    	GraphPattern general2particlerate = SparqlGeneral.GetQueryGraphPattern(query, particlePredicates, null, generalizeAmount,particleFlowrate);
    	
    	Iri[] densityPredicates = {has_density,hasValue,numericalValue};
    	GraphPattern singlephase2density = SparqlGeneral.GetQueryGraphPattern(query, densityPredicates, null, singlephase,density);
    	
    	Iri[] tempPredicates = {has_temperature,hasValue,numericalValue};
    	GraphPattern singlephase2temp = SparqlGeneral.GetQueryGraphPattern(query, tempPredicates, null, singlephase,temp);
    	
    	Iri[] heightPredicates = {hasHeight,hasValue,numericalValue};
    	GraphPattern chimney2height = SparqlGeneral.GetQueryGraphPattern(query, heightPredicates, null, chimney,height);
    	
    	GraphPattern queryPattern = GraphPatterns.and(ship2chimney,chimney2diameter,chimney2general,general2flowrate,general2singlephase,
    			singlephase2density,singlephase2temp,chimney2height,general2particlerate);
    	
    	query.from(FromGraph).select(diameter,overallFlowrate,density,height,temp,particleFlowrate).where(queryPattern).prefix(Prefixes);
    	JSONArray queryresult = SparqlGeneral.performQuery(query);
    	
    	return queryresult;
    }
    
    public static double QueryMixtureFlowrate(String ship_iri_string,String species) {
    	SelectQuery query = Queries.SELECT();
    	Iri ship_iri = iri(ship_iri_string);
    	Variable flowrate = SparqlBuilder.var("flowrate");
    	
    	// intermediate node
    	Variable mixture = query.var();
    	
    	Iri[] mixturePredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,hasSubsystem,refersToMaterial,intrinsicCharacteristics};
    	GraphPattern mixturePattern = SparqlGeneral.GetQueryGraphPattern(query, mixturePredicates, null, ship_iri, mixture);
    	
    	// match species
    	Iri[] speciesPredicates = {containsDirectly};
    	Iri[] rdfTypes = {null,speciesIriMap.get(species)};
    	GraphPattern speciesPattern = SparqlGeneral.GetQueryGraphPattern(query, speciesPredicates, rdfTypes, mixture);
    	
    	// flowrate
    	Iri[] flowratePredicates = {hasProperty,hasValue,numericalValue};
    	GraphPattern flowratePattern = SparqlGeneral.GetQueryGraphPattern(query, flowratePredicates, null, mixture,flowrate);
    	
    	GraphPattern queryPattern = GraphPatterns.and(mixturePattern,speciesPattern,flowratePattern);
    	
    	query.from(FromGraph).prefix(Prefixes).select(flowrate).where(queryPattern);
    	
    	double result = SparqlGeneral.performQuery(query).getJSONObject(0).getDouble("flowrate");
    	return result;
    }
    
    public static String[] QueryParticleIRI(String ship_iri_string) {
    	Iri ship_iri = iri(ship_iri_string);
    	
    	String particlekey = "particle";
    	SelectQuery query = Queries.SELECT();
    	Variable particle = SparqlBuilder.var(particlekey);
    	
    	Iri [] particlePredicates = {hasSubsystem,realizes,hasOutput,refersToGeneralizedAmount,contains,hasRepresentativeParticle};
    	GraphPattern queryPattern = SparqlGeneral.GetQueryGraphPattern(query, particlePredicates, null, ship_iri,particle);
    	
    	query.prefix(Prefixes).from(FromGraph).select(particle).where(queryPattern);
    	
    	JSONArray queryresult = SparqlGeneral.performQuery(query);
    	String[] particles = new String[queryresult.length()];
    	
    	for (int i = 0; i<queryresult.length(); i++) {
    		particles[i] = queryresult.getJSONObject(i).getString(particlekey);
    	}
    	return particles;
    }
    
    public static Particle QueryParticle(String particle_iri_string) {
    	Iri particle_iri = iri(particle_iri_string);
    	
    	//keys
    	String massfractionKey = "massfraction"; String diameterKey = "diameter"; String densityKey = "density";
    	
    	//variables to query
    	Variable massfraction = SparqlBuilder.var(massfractionKey);
    	Variable diameter = SparqlBuilder.var(diameterKey);
    	Variable density = SparqlBuilder.var(densityKey);
    	
    	SelectQuery query = Queries.SELECT();
    	
    	Iri[] massPredicates = {hasProperty,hasValue,numericalValue};
    	GraphPattern massPattern = SparqlGeneral.GetQueryGraphPattern(query,massPredicates,null,particle_iri,massfraction);
    	
    	Iri[] diameterPredicates = {has_length,hasValue,numericalValue};
    	GraphPattern diameterPattern = SparqlGeneral.GetQueryGraphPattern(query, diameterPredicates, null, particle_iri, diameter);
    	
    	Iri[] densityPredicates = {has_density,hasValue,numericalValue};
    	GraphPattern densityPattern = SparqlGeneral.GetQueryGraphPattern(query, densityPredicates, null, particle_iri, density);
    	
    	GraphPattern combinedGraph = GraphPatterns.and(massPattern,diameterPattern,densityPattern);
    	
    	// construct query
    	query.select(massfraction,diameter,density).from(FromGraph).prefix(Prefixes).where(combinedGraph);
    	
    	JSONObject queryresult = SparqlGeneral.performQuery(query).getJSONObject(0);
    	
    	Particle part = new Particle();
    	part.setDensity(queryresult.getDouble(densityKey));
    	part.setDiameter(queryresult.getDouble(diameterKey));
    	part.setMassFraction(queryresult.getDouble(massfractionKey));
    	
    	return part;
    }
    /** 
     * returns number of ships in the knowledge graph
     */
    public static int GetNumShips() {
    	SelectQuery query = Queries.SELECT();
    	String queryKey = "numship";
    	Variable numship = SparqlBuilder.var(queryKey);
    	Variable ship = query.var();
    	
    	Assignment assign = Expressions.count(ship).as(numship);
    	GraphPattern queryPattern = ship.isA(Ship);
    	
    	query.from(FromGraph).select(assign).where(queryPattern).prefix(p_ship);
    	
    	int result = SparqlGeneral.performQuery(query).getJSONObject(0).getInt(queryKey);
    	return result;
    }
}