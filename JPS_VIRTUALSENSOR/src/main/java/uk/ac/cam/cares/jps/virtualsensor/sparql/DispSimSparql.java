package uk.ac.cam.cares.jps.virtualsensor.sparql;

import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.virtualsensor.objects.DispSim;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.rdf4j.sparqlbuilder.core.From;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;

public class DispSimSparql {
	public static String SimKey = "simIRI";
	
    private static Prefix p_dispsim = SparqlBuilder.prefix("dispsim",iri("http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#"));
	private static Prefix p_citygml = SparqlBuilder.prefix("city",iri("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#"));
	private static Prefix p_space_time_extended = SparqlBuilder.prefix("space_time_extended",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"));
	private static Prefix p_system = SparqlBuilder.prefix("system",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"));
	private static Prefix p_coordsys = SparqlBuilder.prefix("coordsys",iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#"));
	private static Prefix p_space_time = SparqlBuilder.prefix("space_time",iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#"));
	private static Prefix p_msm = SparqlBuilder.prefix("msm",iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
	
	private static Prefix[] prefixes = {p_dispsim,p_citygml,p_space_time_extended,p_system,p_coordsys,p_space_time};
	
    // rdf type
    private static Iri DispersionSim = p_dispsim.iri("DispersionSim");
    private static Iri EnvelopeType = p_citygml.iri("EnvelopeType");
    private static Iri PointType = p_citygml.iri("PointType");
    private static Iri CoordinateValue = p_coordsys.iri("CoordinateValue");
    private static Iri StraightCoordinate = p_space_time.iri("StraightCoordinate");
    private static Iri ProjectedCoordinateSystem = p_space_time_extended.iri("ProjectedCoordinateSystem");
    
    //relations
    private static Iri hasNx = p_dispsim.iri("hasNx");
    private static Iri hasNy = p_dispsim.iri("hasNy");
    private static Iri hasEnvelope = p_citygml.iri("hasEnvelope");
    private static Iri srsname = p_citygml.iri("srsname");
    private static Iri lowerCornerPoint = p_citygml.iri("lowerCornerPoint");
    private static Iri upperCornerPoint = p_citygml.iri("upperCornerPoint");
    private static Iri hasGISCoordinateSystem = p_space_time_extended.iri("hasGISCoordinateSystem");
    private static Iri hasProjectedCoordinate_x = p_space_time_extended.iri("hasProjectedCoordinate_x");
    private static Iri hasProjectedCoordinate_y = p_space_time_extended.iri("hasProjectedCoordinate_y");
    private static Iri hasValue = p_system.iri("hasValue");
    private static Iri numericalValue = p_system.iri("numericalValue");
    private static Iri hasMainStation = p_dispsim.iri("hasMainStation"); // not yet in ontology
    private static Iri hasSubStation = p_dispsim.iri("hasSubStation"); // not yet in ontology
    private static Iri hasEmissionSource = p_dispsim.iri("hasEmissionSource"); // not yet in ontology
    private static Iri hasNumSubStations = p_dispsim.iri("hasNumSubStations");
    private static Iri hasServiceAgent = p_dispsim.iri("hasServiceAgent");
    private static Iri hasHttpUrl = p_msm.iri("hasHttpUrl");
    
    //unit
    private static Iri dimensionless = iri("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#dimensionless");
    
    //endpoint
    private static Iri sim_graph = p_dispsim.iri("Simulations");
    private static From FromGraph = SparqlBuilder.from(sim_graph);
    
    /** 
     * Initialise Episode/ADMS agent
     */
    public static void InitService(String service_iri_string, String httpURL) {
    	Iri service_iri = iri(service_iri_string);
    	TriplePattern service_tp = service_iri.has(hasHttpUrl,iri(httpURL));
    	ModifyQuery modify = Queries.MODIFY();
    	modify.prefix(p_msm,p_dispsim).with(sim_graph).insert(service_tp).where();
    	SparqlGeneral.performUpdate(modify);
    }
    
    public static String GetServiceURL(String sim_iri_string) {
    	Iri sim_iri = iri(sim_iri_string);
    	String queryKey = "url";
    	Variable url = SparqlBuilder.var(queryKey);
    	
    	SelectQuery query = Queries.SELECT();
    	Iri[] predicates = {hasServiceAgent,hasHttpUrl};
    	GraphPattern queryPattern = SparqlGeneral.GetQueryGraphPattern(query, predicates, null, sim_iri,url);
    	
    	query.prefix(p_msm,p_dispsim).from(FromGraph).where(queryPattern).select(url);
    	String result = SparqlGeneral.performQuery(query).getJSONObject(0).getString(queryKey);
    	return result;
    }
    
    /**
	 * Initialise a simulation on triple-store
	 */
	public static void InitSim(int sim_index, DispSim sim) {
		String sim_id = "sim" + sim_index;
    	Iri sim_iri = p_dispsim.iri(sim_id);
    	Iri nx = p_dispsim.iri(sim_id+"Nx");
    	Iri nxValue = p_dispsim.iri(sim_id+"NxValue");
    	Iri nyValue = p_dispsim.iri(sim_id+"NyValue");
    	Iri ny = p_dispsim.iri(sim_id+"Ny");
    	Iri numsub = p_dispsim.iri(sim_id+"NumSubStation");
    	Iri numsubvalue = p_dispsim.iri(sim_id+"NumSubStationValue");
    	
    	Iri envelope = p_dispsim.iri(sim_id+"Envelope");
    	Iri lowerCorner = p_dispsim.iri(sim_id+"LowerCorner");
    	Iri lowerCornerCoordinates = p_dispsim.iri(sim_id+"LowerCornerCoordinates");
    	Iri upperCorner = p_dispsim.iri(sim_id+"UpperCorner");
    	Iri upperCornerCoordinates = p_dispsim.iri(sim_id+"UpperCornerCoordinates");
    	
    	Iri lowerCornerX = p_dispsim.iri(sim_id+"LowerCornerX");
    	Iri lowerCornerXValue = p_dispsim.iri(sim_id+"LowerCornerXValue");
    	Iri lowerCornerY = p_dispsim.iri(sim_id+"LowerCornerY");
    	Iri lowerCornerYValue = p_dispsim.iri(sim_id+"LowerCornerYValue");
    	
    	Iri upperCornerX = p_dispsim.iri(sim_id+"UpperCornerX");
    	Iri upperCornerXValue = p_dispsim.iri(sim_id+"UpperCornerXValue");
    	Iri upperCornerY = p_dispsim.iri(sim_id+"UpperCornerY");
    	Iri upperCornerYValue = p_dispsim.iri(sim_id+"UpperCornerYValue");
    	
    	TriplePattern sim_tp = sim_iri.isA(DispersionSim).andHas(hasNx,nx).andHas(hasNy,ny).andHas(hasEnvelope,envelope).andHas(hasNumSubStations,numsub).andHas(hasServiceAgent,iri(sim.getServiceAgent()));
    	
    	TriplePattern envelope_tp = envelope.isA(EnvelopeType).andHas(lowerCornerPoint,lowerCorner).andHas(upperCornerPoint,upperCorner).andHas(srsname,sim.getScope().getCRSName());
    	
    	// lower corner
    	TriplePattern lowercorner_tp = lowerCorner.isA(PointType).andHas(hasGISCoordinateSystem,lowerCornerCoordinates);
    	TriplePattern lowercoord_tp = lowerCornerCoordinates.isA(ProjectedCoordinateSystem).andHas(hasProjectedCoordinate_x,lowerCornerX).andHas(hasProjectedCoordinate_y,lowerCornerY);
    	TriplePattern lowerxcoord_tp = lowerCornerX.isA(StraightCoordinate).andHas(hasValue,lowerCornerXValue);
    	TriplePattern lowerycoord_tp = lowerCornerY.isA(StraightCoordinate).andHas(hasValue,lowerCornerYValue);
    	TriplePattern vlowerxcoord_tp = lowerCornerXValue.isA(CoordinateValue).andHas(numericalValue, sim.getScope().getLowerx());
    	TriplePattern vlowerycoord_tp = lowerCornerYValue.isA(CoordinateValue).andHas(numericalValue, sim.getScope().getLowery());
    	
    	// repeat for upper corner
    	TriplePattern uppercorner_tp = upperCorner.isA(PointType).andHas(hasGISCoordinateSystem,upperCornerCoordinates);
    	TriplePattern uppercoord_tp = upperCornerCoordinates.isA(ProjectedCoordinateSystem).andHas(hasProjectedCoordinate_x,upperCornerX).andHas(hasProjectedCoordinate_y,upperCornerY);
    	TriplePattern upperxcoord_tp =  upperCornerX.isA(StraightCoordinate).andHas(hasValue,upperCornerXValue);
    	TriplePattern upperycoord_tp = upperCornerY.isA(StraightCoordinate).andHas(hasValue,upperCornerYValue);
    	TriplePattern vupperxcoord_tp =  upperCornerXValue.isA(CoordinateValue).andHas(numericalValue, sim.getScope().getUpperx());
    	TriplePattern vupperycoord_tp = upperCornerYValue.isA(CoordinateValue).andHas(numericalValue, sim.getScope().getUppery());
    	
    	// envelope done
    	// model grid information
    	TriplePattern[] nx_tp = SparqlGeneral.GetScalarTP(nx, nxValue, sim.getNx(), dimensionless);
    	TriplePattern[] ny_tp = SparqlGeneral.GetScalarTP(ny, nyValue, sim.getNy(), dimensionless);
    	TriplePattern[] numsub_tp = SparqlGeneral.GetScalarTP(numsub, numsubvalue, sim.getNumSubStations(), dimensionless);
    	
    	TriplePattern[] combined_tp = {sim_tp,envelope_tp,lowercorner_tp,lowercoord_tp,lowerxcoord_tp,lowerycoord_tp,vlowerxcoord_tp,vlowerycoord_tp,
    			uppercorner_tp,uppercoord_tp,upperxcoord_tp,upperycoord_tp,vupperxcoord_tp,vupperycoord_tp};
    	
    	combined_tp = ArrayUtils.addAll(combined_tp, nx_tp);
    	combined_tp = ArrayUtils.addAll(combined_tp, ny_tp);
    	combined_tp = ArrayUtils.addAll(combined_tp, numsub_tp);
    	
    	ModifyQuery modify = Queries.MODIFY();
    	modify.prefix(prefixes).with(sim_graph).where().insert(combined_tp);
    	SparqlGeneral.performUpdate(modify);
    }
	
	public static Scope GetScope(String sim_iri_string) {
		Iri sim_iri = iri(sim_iri_string);
		
		SelectQuery query = Queries.SELECT();
		
		//variables to query
		Variable lowerx = SparqlBuilder.var(Region.keyLowerx);
		Variable lowery = SparqlBuilder.var(Region.keyLowery);
		Variable upperx = SparqlBuilder.var(Region.keyUpperx);
		Variable uppery = SparqlBuilder.var(Region.keyUppery);
		Variable crs = SparqlBuilder.var(Region.keySrsname);
		
		// intermediate variables
		Variable envelope = query.var();
		Variable lowerCornerCoordinates = query.var();
		Variable upperCornerCoordinates = query.var();
		
		Iri[] sim2envelope_predicates = {hasEnvelope};
		GraphPattern sim2envelope = SparqlGeneral.GetQueryGraphPattern(query, sim2envelope_predicates, null, sim_iri, envelope);
		
		Iri[] env2srs_predicates = {srsname};
		GraphPattern env2srs = SparqlGeneral.GetQueryGraphPattern(query, env2srs_predicates, null, envelope, crs);
		
		Iri[] envelope2lowercoord_predicates = {lowerCornerPoint,hasGISCoordinateSystem};
		GraphPattern envelope2lowercoord = SparqlGeneral.GetQueryGraphPattern(query, envelope2lowercoord_predicates, null, envelope, lowerCornerCoordinates);
		
		Iri[] coord2x_predicates = {hasProjectedCoordinate_x,hasValue,numericalValue};
		GraphPattern lower2x = SparqlGeneral.GetQueryGraphPattern(query, coord2x_predicates, null, lowerCornerCoordinates, lowerx);
		
		Iri[] coord2y_predicates = {hasProjectedCoordinate_y,hasValue,numericalValue};
		GraphPattern lower2y = SparqlGeneral.GetQueryGraphPattern(query, coord2y_predicates, null, lowerCornerCoordinates, lowery);
		
		Iri[] envelope2uppercoord_predicates = {upperCornerPoint,hasGISCoordinateSystem};
		GraphPattern envelope2uppercoord = SparqlGeneral.GetQueryGraphPattern(query, envelope2uppercoord_predicates, null, envelope, upperCornerCoordinates);
		
		GraphPattern upper2x = SparqlGeneral.GetQueryGraphPattern(query, coord2x_predicates, null, upperCornerCoordinates, upperx);
		GraphPattern upper2y = SparqlGeneral.GetQueryGraphPattern(query, coord2y_predicates, null, upperCornerCoordinates, uppery);
		
		GraphPattern queryPattern = GraphPatterns.and(sim2envelope,envelope2lowercoord,lower2x,lower2y,envelope2uppercoord,upper2x,upper2y,env2srs);
		
		query.prefix(prefixes).from(FromGraph).select(lowerx,lowery,upperx,uppery,crs).where(queryPattern);
		JSONObject queryResult = SparqlGeneral.performQuery(query).getJSONObject(0);
		
		Scope sc = new Scope();
		sc.setLowerx(queryResult.getDouble(Region.keyLowerx));
		sc.setLowery(queryResult.getDouble(Region.keyLowery));
		sc.setUpperx(queryResult.getDouble(Region.keyUpperx));
		sc.setUppery(queryResult.getDouble(Region.keyUppery));
		sc.setCRSName(queryResult.getString(Region.keySrsname));
		
		return sc;
	}
	
	public static int GetNumSubStations(String sim_iri_string) {
		Iri sim_iri = iri(sim_iri_string);
		String queryKey = "numsub";
		Variable numsub = SparqlBuilder.var(queryKey);
		
		SelectQuery query = Queries.SELECT();
		
		Iri[] numsub_predicates = {hasNumSubStations,hasValue,numericalValue};
	    GraphPattern queryPattern = SparqlGeneral.GetQueryGraphPattern(query, numsub_predicates, null, sim_iri, numsub);
		
		query.select(numsub).prefix(p_system,p_dispsim).from(FromGraph).where(queryPattern);
		int result = SparqlGeneral.performQuery(query).getJSONObject(0).getInt(queryKey);
		return result;
	}
	
	/** 
	 * Insert main weather station used for this simulation
	 */
	public static void AddMainStation(String sim_iri_string, String station_iri_string) {
		Iri sim_iri = iri(sim_iri_string);

		// delete existing main station
		SubSelect sub = GraphPatterns.select();
		Variable oldmain = sub.var();
		TriplePattern delete_tp = sim_iri.has(hasMainStation,oldmain);
		sub.select(oldmain).where(delete_tp);
		ModifyQuery deleteQuery = Queries.MODIFY();
		deleteQuery.prefix(p_dispsim).with(sim_graph).where(sub).delete(delete_tp);
		SparqlGeneral.performUpdate(deleteQuery);
		
		//insert new station
		TriplePattern insert_tp = sim_iri.has(hasMainStation,iri(station_iri_string));
		ModifyQuery insertQuery = Queries.MODIFY();
		insertQuery.prefix(p_dispsim).with(sim_graph).insert(insert_tp);
		SparqlGeneral.performUpdate(insertQuery);
	}
	
	public static String GetMainStation(String sim_iri_string) {
		Iri sim_iri = iri(sim_iri_string);
		String stationKey = "station";
		Variable station = SparqlBuilder.var(stationKey);
		GraphPattern queryPattern = sim_iri.has(hasMainStation,station);
		SelectQuery query = Queries.SELECT();
		query.select(station).from(FromGraph).where(queryPattern).prefix(p_dispsim);
		String result = SparqlGeneral.performQuery(query).getJSONObject(0).getString(stationKey);
		return result;
	}
	
	/**
	 * 
	 */
	public static void AddSubStations(String sim_iri_string, String[] station_iri_string) {
		Iri sim_iri = iri(sim_iri_string);
		
		// find old stations to delete
		SubSelect sub = GraphPatterns.select();
		Variable stations = sub.var();
		TriplePattern deletePattern = sim_iri.has(hasSubStation,stations);
		sub.select(stations).where(deletePattern);
		
		ModifyQuery deleteQuery = Queries.MODIFY();
		deleteQuery.prefix(p_dispsim).where(sub).delete(deletePattern).with(sim_graph);
		SparqlGeneral.performUpdate(deleteQuery);
		
		// new stations to add
		TriplePattern[] insert_tp = new TriplePattern[station_iri_string.length];
		for (int i = 0; i < station_iri_string.length; i++) {
			Iri station_iri = iri(station_iri_string[i]);
			insert_tp[i] = sim_iri.has(hasSubStation,station_iri);
		}
		
		ModifyQuery insertQuery = Queries.MODIFY();
		insertQuery.prefix(p_dispsim).with(sim_graph).insert(insert_tp);
		SparqlGeneral.performUpdate(insertQuery);
	}
	
	public static String[] GetSubStations(String sim_iri_string) {
		Iri sim_iri = iri(sim_iri_string);
		String stationKey = "station";
		Variable station = SparqlBuilder.var(stationKey);
		GraphPattern queryPattern = sim_iri.has(hasSubStation,station);
		SelectQuery query = Queries.SELECT();
		query.select(station).from(FromGraph).where(queryPattern).prefix(p_dispsim);
		
		JSONArray queryResult = SparqlGeneral.performQuery(query);
		String[] stationIRI = new String[queryResult.length()];
		
		for (int i = 0; i < queryResult.length(); i++) {
			stationIRI[i] = queryResult.getJSONObject(i).getString(stationKey);
		}
		
		return stationIRI;
	}
	
	public static void AddEmissionSources(String sim_iri_string, String[] ship_iri_string) {
		Iri sim_iri = iri(sim_iri_string);
		
		// old ship IRIs to delete
		SubSelect sub = GraphPatterns.select();
		Variable oldships = sub.var();
		TriplePattern deletePattern = sim_iri.has(hasEmissionSource,oldships);
		sub.select(oldships).where(deletePattern);
		
		ModifyQuery deleteQuery = Queries.MODIFY();
		deleteQuery.prefix(p_dispsim).where(sub).delete(deletePattern).with(sim_graph);
		SparqlGeneral.performUpdate(deleteQuery);
		
		// add new ships
		TriplePattern[] insert_tp = new TriplePattern[ship_iri_string.length];
		for (int i = 0; i < ship_iri_string.length; i++) {
			Iri ship_iri = iri(ship_iri_string[i]);
			insert_tp[i] = sim_iri.has(hasEmissionSource,ship_iri);
		}
		
		ModifyQuery insertQuery = Queries.MODIFY();
		insertQuery.prefix(p_dispsim).with(sim_graph).insert(insert_tp);
		SparqlGeneral.performUpdate(insertQuery);
	}
	
	public static String[] GetEmissionSources(String sim_iri_string) {
		Iri sim_iri = iri(sim_iri_string);
		String shipKey = "ship";
		Variable ship = SparqlBuilder.var(shipKey);
		GraphPattern queryPattern = sim_iri.has(hasEmissionSource,ship);
		SelectQuery query = Queries.SELECT();
		query.select(ship).from(FromGraph).where(queryPattern).prefix(p_dispsim);
		
		JSONArray queryResult = SparqlGeneral.performQuery(query);
		String[] shipIRI = new String[queryResult.length()];
		
		for (int i = 0; i < queryResult.length(); i++) {
			shipIRI[i] = queryResult.getJSONObject(i).getString(shipKey);
		}
		
		return shipIRI;
	}
}
