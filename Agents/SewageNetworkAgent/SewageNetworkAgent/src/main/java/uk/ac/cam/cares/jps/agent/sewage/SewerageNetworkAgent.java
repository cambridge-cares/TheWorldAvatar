package uk.ac.cam.cares.jps.agent.sewage;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.graph.NodeFactory;
import javax.servlet.annotation.WebServlet;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import org.json.JSONObject;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

@Controller
@WebServlet(urlPatterns = {"/performsewageupdate"})


public class SewerageNetworkAgent extends JPSAgent {

	private static final long serialVersionUID = 1L;
	private static final Logger LOGGER = LogManager.getLogger(SewerageNetworkAgent.class);
	private static final String DATAINSTANTIATION = "Could not update data";
	private static final String KEY_ENDPOINT = "endpoint";
	private static String sparqlendpoint;

	// Common Base URLs
	public static String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	public static String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
	public static String XSD = "http://www.w3.org/2001/XMLSchema#";
	public static String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
	public static String OS = "https://www.theworldavatar.com/kg/ontosewage/";
	public static String KB = "https://www.theworldavatar.com/kb/ontosewage/";

	// IRIs for OntoSewage and others
	public static String BMO = "https://w3id.org/digitalconstruction/0.3/BuildingMaterials#";
	public static String s4watr = "https://saref.etsi.org/saref4watr/";
	public static String schema = "https://schema.org/";
	public static String dul = "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#";
	public static String ogc = "http://www.opengis.net/ont/geosparql#";
	public static String sio = "http://semanticscience.org/resource/";
	public static String juso = "http://rdfs.co/juso/";
	public static String hasPart = "http://www.theworldavatar.com/ontology/meta_model/mereology/mereology.owl#hasPart";

	// For units of measure
	public static String OM_QUANTITY = OM + "Quantity";
	public static String OM_MEASURE = OM + "Measure";
	public static String OM_UNIT = OM + "Unit";
	public static String OM_HAS_VALUE = OM + "hasValue";
	public static String OM_HAS_UNIT = OM + "hasUnit";
	public static String OM_SYMBOL = OM + "symbol";
	public static String OM_Has_NUMERICAL_VALUE = OM + "hasNumericalValue";
	public static String OM_LENGTH = OM + "Length";
	public static String OM_THICKNESS = OM + "Thickness";
	public static String OM_HEIGHT = OM + "Height";
	public static String OM_WIDTH = OM + "Width";
	public static String OM_DISTANCE = OM + "Distance";
	public static String OM_DEPTH = OM + "Depth";

	// Data types
	public static String RDF_TYPE = RDF + "type";
	public static String RDFS_COMMENT = RDFS + "comment";
	public static String RDFS_LABEL = RDFS + "label";
	public static String XSD_STRING = XSD + "string";
	public static String XSD_FLOAT = XSD + "float";
	public static String XSD_DATE = XSD + "date";
	public static String XSD_BOOLEAN = XSD + "boolean";
	public static String XSD_INTEGER = XSD + "integer";


	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {	
		
		sparqlendpoint = requestParams.getString(KEY_ENDPOINT);
		String HG_Path_0 = System.getenv("HGDATA0");
		String HG_Path_1 = System.getenv("HGDATA1");
		String HG_Path_2 = System.getenv("HGDATA2");
		String HG_Path_3 = System.getenv("HGDATA3");
		String HG_Path_4 = System.getenv("HGDATA4");
		String HG_Path_5 = System.getenv("HGDATA5");
		String HG_Path_6 = System.getenv("HGDATA6");	
		String KG_Path_0 = System.getenv("KGDATA0");
		String KG_Path_1 = System.getenv("KGDATA1");
		String KG_Path_2 = System.getenv("KGDATA2");
		String KG_Path_3 = System.getenv("KGDATA3");
		String KG_Path_4 = System.getenv("KGDATA4");
		String KG_Path_5 = System.getenv("KGDATA5");
		String KG_Path_6 = System.getenv("KGDATA6");
		String KG_Path_7 = System.getenv("KGDATA7");
		String KG_Path_8 = System.getenv("KGDATA8");
		String KG_Path_9 = System.getenv("KGDATA9");
		String KG_Path_10 = System.getenv("KGDATA10");		
		String KG_MainNet = System.getenv("KGMainNet");
		String KG_SubNet = System.getenv("KGSubNet");
		String BR_Path_0 = System.getenv("Branch0");
		String BR_Path_1 = System.getenv("Branch1");
		String BR_Path_2 = System.getenv("Branch2");
		String BR_Path_3 = System.getenv("Branch3");

		JSONObject jsonMessage = new JSONObject();
		try {
			HGInstantiation(HG_Path_0);
			HGInstantiation(HG_Path_1);
			HGInstantiation(HG_Path_2);
			HGInstantiation(HG_Path_3);
			HGInstantiation(HG_Path_4);
		    HGInstantiation(HG_Path_5);
			HGInstantiation(HG_Path_6);
			KGInstantiation(KG_Path_0);
			KGInstantiation(KG_Path_1);
			KGInstantiation(KG_Path_2);
			KGInstantiation(KG_Path_3);
			KGInstantiation(KG_Path_4);
			KGInstantiation(KG_Path_5);
			KGInstantiation(KG_Path_6);
			KGInstantiation(KG_Path_7);
			KGInstantiation(KG_Path_8);
			KGInstantiation(KG_Path_9);
			KGInstantiation(KG_Path_10);	
			BranchInstantiation(BR_Path_0);
			BranchInstantiation(BR_Path_1);
			BranchInstantiation(BR_Path_2);
			BranchInstantiation(BR_Path_3);
			KGMainSubNetInstantiation(KG_MainNet, KG_SubNet);
			jsonMessage.accumulate("Result", "Data has been instantiated.");
		} catch (JPSRuntimeException e) {
			LOGGER.error(DATAINSTANTIATION, e);
			throw new JPSRuntimeException(DATAINSTANTIATION, e);
		}

		return jsonMessage;
	}

	public static void HGInstantiation(String HG_Path) {
		int HG_column_length = 0;
		try {
			HG_column_length = ColNum(HG_Path, ",");
		} catch (java.io.IOException e) {
			e.printStackTrace();
		}

		for (int i = 1; i < HG_column_length; i++) { //HG_column_length; i++) {
			String[] HG_Instance = ReadCol(i, HG_Path, ","); 

			// Instantiation HG data	
			String HG_Instance_Name = HG_Instance[0];
			String HG001 = HG_Instance[1]; 
			String HG003 = HG_Instance[2];
			String HG004 = HG_Instance[3];
			String HG005 = HG_Instance[4];
			String HG006 = HG_Instance[5];
			String HG007 = HG_Instance[6];
			String HG008 = HG_Instance[7];
			String HG009 = HG_Instance[8];
			String HG010 = HG_Instance[9];
			String HG011 = HG_Instance[10];
			String HG101 = HG_Instance[11];
			String HG102 = HG_Instance[12];
			String HG103 = HG_Instance[13]; 
			String HG104 = HG_Instance[14];
			String HG107 = HG_Instance[15];
			String HG108 = HG_Instance[16]; 
			String HG301 = HG_Instance[17]; 
			String HG302 = HG_Instance[18];
			String HG303 = HG_Instance[19]; 
			String HG304 = HG_Instance[20]; 
			String HG305 = HG_Instance[21]; 
			String HG306 = HG_Instance[22]; 
			String HG307 = HG_Instance[23]; 
			String HG310 = HG_Instance[24]; 
			String HG311 = HG_Instance[25];
			String HG313 = HG_Instance[26];
			String HG401 = HG_Instance[27]; 
			String HG402 = HG_Instance[28];
			String HG403 = HG_Instance[29];
			String HG404 = HG_Instance[30];
			String HG406 = HG_Instance[31];
			String HG410 = HG_Instance[32];
			String HG500 = HG_Instance[33];
			String HG_GP002 = HG_Instance[34];
			String HG_GP001A = HG_Instance[35];
			String HG_GP001B = HG_Instance[36]; 
			String HG_GP003A = HG_Instance[37];
			String HG_GP003B = HG_Instance[38]; 
			String HG_GP004A = HG_Instance[39];
			String HG_GP004B = HG_Instance[40]; 
			String HG_GP007A = HG_Instance[41];
			String HG_GP007B = HG_Instance[42];
			String HG_GP008A = HG_Instance[43];
			String HG_GP008B = HG_Instance[44]; 
			String HG_GP009A = HG_Instance[45]; 
			String HG_GP009B = HG_Instance[46]; 
			String HG_GP010A = HG_Instance[47]; 
			String HG_GP010B = HG_Instance[48];


			UpdateBuilder SewerageComponentHG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(s4watr + "Pipe"))
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "componentID"), HG001)		
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "isAssociatedWith"), NodeFactory.createURI(KB + "SewagePlant" + HG108))	
					.addInsert(NodeFactory.createURI(KB + "SewagePlant" + HG108), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewagePlant"))		
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasUsage"), NodeFactory.createURI(KB + "SewerageUsage" + HG302))
					.addInsert(NodeFactory.createURI(KB + "SewerageUsage" + HG302), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageUsage"))		
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasSewerageRecords"), NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageRecords"))
					.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name), NodeFactory.createURI(OS + "hasOwnershipType"), NodeFactory.createURI(KB + "OwnershipType" + HG402))
					.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name), NodeFactory.createURI(OS + "hasFunctionalState"), NodeFactory.createURI(KB + "FunctionalState" + HG401))
					.addInsert(NodeFactory.createURI(KB + "OwnershipType" + HG402), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "OwnershipType"))
					.addInsert(NodeFactory.createURI(KB + "FunctionalState" + HG401), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "FunctionalState"));
			UpdateRequest SewerageComponentHG_ur = SewerageComponentHG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, SewerageComponentHG_ur.toString());


			UpdateBuilder ConstructionPropertiesHG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasConstructionProperties"), NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ConstructionProperties"))
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name), NodeFactory.createURI(OS + "constructionYear"), HG303)
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name), NodeFactory.createURI(BMO + "hasMaterial"), NodeFactory.createURI(KB + "Material" + HG304))
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name), NodeFactory.createURI(OS + "hasChannelType"), NodeFactory.createURI(KB + "ChannelType" + HG301))
					.addInsert(NodeFactory.createURI(KB + "Material" + HG304), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(BMO + "Material"))
					.addInsert(NodeFactory.createURI(KB + "ChannelType" + HG301), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ChannelType"));
			UpdateRequest ConstructionPropertiesHG_ur = ConstructionPropertiesHG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, ConstructionPropertiesHG_ur.toString());

            
			if (HG003.equals("None")) {	
			} else {
			UpdateBuilder BranchConnectionHG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "BranchConnection"))	
					.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name), NodeFactory.createURI(OS + "isInFlowDirection"), HG008)			
					.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name), NodeFactory.createURI(OS + "clockPositionOfBranchPipe"), HG009)			
					.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name), NodeFactory.createURI(OS + "relativeDistanceOnMainPipe"), NodeFactory.createURI(KB + "DistanceOnMainPipe" + "BranchConnection" + HG_Instance_Name))			
					.addInsert(NodeFactory.createURI(KB + "DistanceOnMainPipe" + "BranchConnection" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_DISTANCE));
			UpdateRequest BranchConnectionHG_ur = BranchConnectionHG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, BranchConnectionHG_ur.toString());
			omHasValue("DistanceOnMainPipe" + "BranchConnection" + HG_Instance_Name, "metre", HG007);
			}


			UpdateBuilder CrossSectionHG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasCrossSection"), NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name))		
					.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "CrossSection"))			
					.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name), NodeFactory.createURI(OS + "hasShape"), NodeFactory.createURI(KB + "Shape" + HG305))
					.addInsert(NodeFactory.createURI(KB + "Shape" + HG305), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Shape"))
					.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name), NodeFactory.createURI(OS + "hasHeight"), NodeFactory.createURI(KB + "Height" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Height" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT))
					.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name), NodeFactory.createURI(OS + "hasWidth"), NodeFactory.createURI(KB + "Width" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Width" +  HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_WIDTH));		
			UpdateRequest CrossSectionHG_ur = CrossSectionHG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, CrossSectionHG_ur.toString());
			omHasValue("Height" + HG_Instance_Name, "millimetre", HG307);		
			omHasValue("Width"  + HG_Instance_Name, "millimetre", HG306);	


			UpdateBuilder SewerageFluidHG_ub = 
					new UpdateBuilder()	
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "usedFor"), NodeFactory.createURI(KB + "SewerageFluid" + HG500))
					.addInsert(NodeFactory.createURI(KB + "SewerageFluid" + HG500), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageFluid"))
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasConnectionID"), HG011)
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasSourceID"), HG005)
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasPipeType"), NodeFactory.createURI(KB + "PipeType" + HG313))
					.addInsert(NodeFactory.createURI(KB + "PipeType" + HG313), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "PipeType"))
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasInclination"), NodeFactory.createURI(KB + "Inclination" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Inclination" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Inclination"))
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasLength"), NodeFactory.createURI(KB + "Length" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Length" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_LENGTH))
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasWallThickness"), NodeFactory.createURI(KB + "WallThickness" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "WallThickness" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_THICKNESS));
			UpdateRequest SewerageFluidHG_ur = SewerageFluidHG_ub.buildRequest(); 
			AccessAgentCaller.updateStore(sparqlendpoint, SewerageFluidHG_ur.toString());
			omHasValue("Inclination" + HG_Instance_Name, "Percentage", HG311);		
			omHasValue("Length" + HG_Instance_Name, "metre", HG310);	
			omHasValue("Thickness" + HG_Instance_Name, "metre", HG410);	
			AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeOther>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeOther>}");


			UpdateBuilder ConnectionPropertiesHG_ub = 
					new UpdateBuilder()	
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasConnectionProperties"), NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ConnectionProperties"))
					.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name), NodeFactory.createURI(OS + "hasUpstreamConnector"), HG003)
					.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name), NodeFactory.createURI(OS + "hasDownstreamConnector"), HG004)
					.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name), NodeFactory.createURI(OS + "hasEndpointObject"), HG006)
					.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name), NodeFactory.createURI(OS + "hasEndpointType"), NodeFactory.createURI(KB + "hasEndpointType" + HG010))
					.addInsert(NodeFactory.createURI(KB + "hasEndpointType" + HG010), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "EndpointType"));
			UpdateRequest ConnectionPropertiesHG_ur = ConnectionPropertiesHG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, ConnectionPropertiesHG_ur.toString());


			UpdateBuilder LocationInletHG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasInletLocation"), NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "location"))
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name), NodeFactory.createURI(OS + "isInWaterProtectionZone"), HG403)
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name), NodeFactory.createURI(OS + "isInFloodplane"), HG406)
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name), NodeFactory.createURI(OS + "isAssociatedWith"), NodeFactory.createURI(KB + "AssociatedInfrastructure" + HG404))
					.addInsert(NodeFactory.createURI(KB + "AssociatedInfrastructure" + HG404), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "AssociatedInfrastructure"))  
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasCoordinates"), NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "GeoCoordinates"))   	
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasPositionAccuracy"), HG_GP008A)	
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasCoordinateReference"), HG_GP002)		
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name), NodeFactory.createURI(ogc + "asWKT"), "POINT("+HG_GP003A+","+HG_GP004A+")^^ogc:wktLiteral")
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasPointDesignation"), HG_GP001A)
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevation"), NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name))	
					.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "elevation"))   
					.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevationReference"), HG_GP010A)
					.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevationAccuracy"), HG_GP009A)
					.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevationAboveSeaLevel"), NodeFactory.createURI(KB + "ElevationHeightI" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "ElevationHeightI" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT))
					.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name), NodeFactory.createURI(OS + "hasCatchmentAreaKey"), HG107)
					.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name), NodeFactory.createURI(sio + "SIO_000061"), NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "District"))
					.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name), NodeFactory.createURI(OS + "hasDistrictReference"), HG103)
					.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name), NodeFactory.createURI(OS + "hasDistrictName"), HG104)   
					.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name), NodeFactory.createURI(sio + "SIO_000061"), NodeFactory.createURI(KB + "Street" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "Street"))
					.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name), NodeFactory.createURI(OS + "hasStreetReference"), HG101)
					.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name), NodeFactory.createURI(OS + "hasStreetName"), HG102);
			UpdateRequest LocationInletHG_ur = LocationInletHG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, LocationInletHG_ur.toString()); 
			omHasValue("ElevationHeightI" + HG_Instance_Name, "metre", HG_GP007A);	


			UpdateBuilder LocationOutletHG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasOutletLocation"), NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "location"))
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name), NodeFactory.createURI(OS + "isInWaterProtectionZone"), HG403)
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name), NodeFactory.createURI(OS + "isInFloodplane"), HG406)
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name), NodeFactory.createURI(OS + "isAssociatedWith"), NodeFactory.createURI(KB + "AssociatedInfrastructure" + HG404))
					.addInsert(NodeFactory.createURI(KB + "AssociatedInfrastructure" + HG404), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "AssociatedInfrastructure"))  
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasCoordinates"), NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "GeoCoordinates"))   	
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasPositionAccuracy"), HG_GP008B)	
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasCoordinateReference"), HG_GP002)		
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name), NodeFactory.createURI(ogc + "asWKT"), "POINT("+HG_GP003B+","+HG_GP004B+")^^ogc:wktLiteral")
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasPointDesignation"), HG_GP001B)
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevation"), NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name))	
					.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "elevation"))   
					.addInsert(NodeFactory.createURI(KB + "ElevationOn" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevationReference"), HG_GP010B)
					.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevationAccuracy"), HG_GP009B)
					.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasElevationAboveSeaLevel"), NodeFactory.createURI(KB + "ElevationHeightO" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "ElevationHeightO" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT))	
					.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name), NodeFactory.createURI(OS + "hasCatchmentAreaKey"), HG107)
					.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name), NodeFactory.createURI(sio + "SIO_000061"), NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "District"))
					.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name), NodeFactory.createURI(OS + "hasDistrictReference"), HG103)
					.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name), NodeFactory.createURI(OS + "hasDistrictName"), HG104)   
					.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name), NodeFactory.createURI(sio + "SIO_000061"), NodeFactory.createURI(KB + "Street" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "Street"))
					.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name), NodeFactory.createURI(OS + "hasStreetReference"), HG101)
					.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name), NodeFactory.createURI(OS + "hasStreetName"), HG102);
			UpdateRequest LocationOutletHG_ur = LocationOutletHG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, LocationOutletHG_ur.toString());   
			omHasValue("ElevationHeightO" + HG_Instance_Name, "metre", HG_GP007B);


			if (HG006.equals("None")) {
				UpdateBuilder MainNetworkHG_ub = 
						new UpdateBuilder()
						.addInsert(NodeFactory.createURI(KB + "MainNetwork"), NodeFactory.createURI(hasPart), NodeFactory.createURI(KB + HG_Instance_Name));
				UpdateRequest MainNetworkHG_ur = MainNetworkHG_ub.buildRequest();
				AccessAgentCaller.updateStore(sparqlendpoint, MainNetworkHG_ur.toString());   
			} else {
				UpdateBuilder SubNetworkHG_ub = 
						new UpdateBuilder()
						.addInsert(NodeFactory.createURI(KB + "SubNetwork"), NodeFactory.createURI(hasPart), NodeFactory.createURI(KB + HG_Instance_Name));
				UpdateRequest SubNetworkHG_ur = SubNetworkHG_ub.buildRequest();
				AccessAgentCaller.updateStore(sparqlendpoint, SubNetworkHG_ur.toString()); 
			}
		}

		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y \"None\"} where {?x ?y \"None\"}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y \"POINT(None,None)^^ogc:wktLiteral\"} where {?x ?y \"POINT(None,None)^^ogc:wktLiteral\"}");
		}
	
		
	public static void KGInstantiation(String KG_Path) {
		int KG_column_length = 0;
		try {
			KG_column_length = ColNum(KG_Path, ",");
		} catch (java.io.IOException e) {
			e.printStackTrace();
		}

		for (int i = 1; i < KG_column_length; i++) { //KG_column_length; i++) {
			String[] KG_Instance = ReadCol(i, KG_Path, ","); 

			// Instantiation KG data
			String KG_Instance_Name = KG_Instance[0];	
			String KG001 = KG_Instance[1];
			String KG108 = KG_Instance[2];
			String KG211 = KG_Instance[3];
			String KG301 = KG_Instance[4];
			String KG302 = KG_Instance[5];
			String KG303 = KG_Instance[6];
			String KG304 = KG_Instance[7];
			String KG305 = KG_Instance[8];
			String KG306 = KG_Instance[9];
			String KG307 = KG_Instance[10];
			String KG308 = KG_Instance[11];
			String KG309 = KG_Instance[12];
			String KG310 = KG_Instance[13];
			String KG311 = KG_Instance[14];
			String KG312 = KG_Instance[15];
			String KG316 = KG_Instance[16];
			String KG318 = KG_Instance[17];
			String KG319 = KG_Instance[18];
			String KG401 = KG_Instance[19];
			String KG402 = KG_Instance[20];
			String KG403 = KG_Instance[21];
			String KG404 = KG_Instance[22];
			String KG406 = KG_Instance[23];
			String KG_GP001 = KG_Instance[24];; 
			String KG_GP002 = KG_Instance[25];
			String KG_GP010 = KG_Instance[26];
			String KG_GP003A = KG_Instance[27];
			String KG_GP003B = KG_Instance[28];
			String KG_GP004A = KG_Instance[29];
			String KG_GP004B = KG_Instance[30];
			String KG_GP007A = KG_Instance[31]; 
			String KG_GP007B = KG_Instance[32];
			String KG_GP008A = KG_Instance[33];
			String KG_GP008B = KG_Instance[34];
			String KG_GP009A = KG_Instance[35];
			String KG_GP009B = KG_Instance[36];


			UpdateBuilder SewerageComponentKG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(s4watr + "Manhole"))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "componentID"), KG001)	
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "isAssociatedWith"), NodeFactory.createURI(KB + "SewagePlant" + KG108))	
					.addInsert(NodeFactory.createURI(KB + "SewagePlant" + KG108), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewagePlant"))	
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasUsage"), NodeFactory.createURI(KB + "SewerageUsage" + KG302))
					.addInsert(NodeFactory.createURI(KB + "SewerageUsage" + KG302), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageUsage"))		
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasSewerageRecords"), NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageRecords"))	
					.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name), NodeFactory.createURI(OS + "hasOwnershipType"), NodeFactory.createURI(KB + "OwnershipType" + KG402))
					.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name), NodeFactory.createURI(OS + "hasFunctionalState"), NodeFactory.createURI(KB + "FunctionalState" + KG401))
					.addInsert(NodeFactory.createURI(KB + "OwnershipType" + KG402), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "OwnershipType"))
					.addInsert(NodeFactory.createURI(KB + "FunctionalState" + KG401), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "FunctionalState"));
			UpdateRequest SewerageComponentKG_ur = SewerageComponentKG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, SewerageComponentKG_ur.toString());


			UpdateBuilder ConstructionPropertiesKG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasConstructionProperties"), NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ConstructionProperties"))
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name), NodeFactory.createURI(OS + "constructionYear"), KG303)
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name), NodeFactory.createURI(BMO + "hasMaterial"), NodeFactory.createURI(KB + "Material" + KG304))
					.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name), NodeFactory.createURI(OS + "hasChannelType"), NodeFactory.createURI(KB + "ChannelType" + KG301))
					.addInsert(NodeFactory.createURI(KB + "Material" + KG304), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(BMO + "Material"))
					.addInsert(NodeFactory.createURI(KB + "ChannelType" + KG301), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ChannelType"));
			UpdateRequest ConstructionPropertiesKG_ur = ConstructionPropertiesKG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, ConstructionPropertiesKG_ur.toString());


			UpdateBuilder FlumeKG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasStructureType"), NodeFactory.createURI(KB + "StructureType" + KG306))
					.addInsert(NodeFactory.createURI(KB + "StructureType" + KG306), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "StructureType"))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasFlume"), NodeFactory.createURI(KB + "Flume" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Flume"))
					.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name), NodeFactory.createURI(OS + "hasShape"), NodeFactory.createURI(KB + "Shape" + KG316))
					.addInsert(NodeFactory.createURI(KB + "Shape" + KG316), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Shape"))
					.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name), NodeFactory.createURI(OS + "hasLength"), NodeFactory.createURI(KB + "Length" + "Flume" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Length" + "Flume" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_LENGTH))
					.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name), NodeFactory.createURI(OS + "hasWidth"), NodeFactory.createURI(KB + "Width" + "Flume" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Width" + "Flume" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_WIDTH));
			UpdateRequest FlumeKG_ur = FlumeKG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, FlumeKG_ur.toString());
			omHasValue("Length" + "Flume" + KG_Instance_Name, "millimetre", KG319);		
			omHasValue("Width" + "Flume" + KG_Instance_Name, "millimetre", KG318);	


			UpdateBuilder ShaftKG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasShaft"), NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Shaft"))	
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name), NodeFactory.createURI(OS + "hasShaftType"), NodeFactory.createURI(KB + "ShaftType" + KG305))
					.addInsert(NodeFactory.createURI(KB + "ShaftType" + KG305), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ShaftType"))	
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name), NodeFactory.createURI(OS + "hasShape"), NodeFactory.createURI(KB + "Shape" + KG307))
					.addInsert(NodeFactory.createURI(KB + "Shape" + KG307), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Shape"))
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name), NodeFactory.createURI(OS + "hasLength"), NodeFactory.createURI(KB + "Length" + "Shaft" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Length" + "Shaft" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_LENGTH))
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name), NodeFactory.createURI(OS + "hasWidth"), NodeFactory.createURI(KB + "Width" + "Shaft" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Width" + "Shaft" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_WIDTH))
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name), NodeFactory.createURI(OS + "hasDepth"), NodeFactory.createURI(KB + "Depth" + "Shaft" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Depth" + "Shaft" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_DEPTH));
			UpdateRequest ShaftKG_ur = ShaftKG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, ShaftKG_ur.toString());
			omHasValue("Length" + "Shaft" + KG_Instance_Name, "millimetre", KG308);		
			omHasValue("Width" + "Shaft" + KG_Instance_Name, "millimetre", KG309);	
			omHasValue("Depth" + "Shaft" + KG_Instance_Name, "metre", KG211);	


			UpdateBuilder CoverKG_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasCover"), NodeFactory.createURI(KB + "Cover" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Cover"))
					.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name), NodeFactory.createURI(BMO + "hasMaterial"), NodeFactory.createURI(KB + "Material" + KG311))
					.addInsert(NodeFactory.createURI(KB + "Material" + KG311), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(BMO + "Material"))
					.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name), NodeFactory.createURI(OS + "hasClass"), NodeFactory.createURI(KB + "Class" + KG312))
					.addInsert(NodeFactory.createURI(KB + "Class" + KG312), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "CoverClass"))
					.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name), NodeFactory.createURI(OS + "hasShape"), NodeFactory.createURI(KB + "Shape" + KG310))
					.addInsert(NodeFactory.createURI(KB + "Shape" + KG310), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Shape"));
			UpdateRequest CoverKG_ur = CoverKG_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, CoverKG_ur.toString());


			UpdateBuilder LocationKG1_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(dul + "hasLocation"), NodeFactory.createURI(KB + "Location" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "location"))
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name), NodeFactory.createURI(OS + "isInWaterProtectionZone"), KG403)
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name), NodeFactory.createURI(OS + "isInFloodplane"), KG406)
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name), NodeFactory.createURI(OS + "isAssociatedWith"), NodeFactory.createURI(KB + "AssociatedInfrastructure" + KG404))
					.addInsert(NodeFactory.createURI(KB + "AssociatedInfrastructure" + KG404), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "AssociatedInfrastructure")) 	
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name), NodeFactory.createURI(OS + "hasPointDesignation"), KG_GP001);			
			UpdateRequest LocationKG1_ur = LocationKG1_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, LocationKG1_ur.toString()); 	
			
			
			if (!KG_GP003B.equals("None") && !KG_GP004B.equals("None") && !KG_GP007B.equals("None")) {	
			UpdateBuilder LocationKG2_ub = 
				   new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name), NodeFactory.createURI(OS + "hasCoordinates"), NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "GeoCoordinates"))    
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name), NodeFactory.createURI(OS + "hasPositionAccuracy"), KG_GP008B)
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name), NodeFactory.createURI(OS + "hasCoordinateReference"), KG_GP002)
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name), NodeFactory.createURI(ogc + "asWKT"), "POINT("+KG_GP003B+","+KG_GP004B+")^^ogc:wktLiteral")  
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name), NodeFactory.createURI(OS + "hasElevation"), NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name))	
					.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "elevation"))   
					.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name), NodeFactory.createURI(OS + "hasElevationReference"), KG_GP010)
					.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name), NodeFactory.createURI(OS + "hasElevationAccuracy"), KG_GP009B)
					.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name), NodeFactory.createURI(OS + "hasElevationAboveSeaLevel"), NodeFactory.createURI(KB + "ElevationAboveSeaLevel" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "ElevationAboveSeaLevel" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT));
			UpdateRequest LocationKG2_ur = LocationKG2_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, LocationKG2_ur.toString()); 	
			omHasValue("ElevationAboveSeaLevel" + KG_Instance_Name, "metre", KG_GP007B);	
			} else {}			
		}

		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y \"None\"} where {?x ?y \"None\"}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/hasEndpointTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewagePlantNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y \"POINT(None,None)^^ogc:wktLiteral\"} where {?x ?y \"POINT(None,None)^^ogc:wktLiteral\"}");
	}


	public static void KGMainSubNetInstantiation (String KG_MainNet, String KG_SubNet) {
		String[] KG_Main = ReadCol(0,KG_MainNet, ","); 
		String[] KG_Sub = ReadCol(0,KG_SubNet, ","); 
		
		UpdateBuilder SewerageNetwork_ub = 
				new UpdateBuilder()
				.addInsert(NodeFactory.createURI(KB + "MainNetwork"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageNetwork"))
				.addInsert(NodeFactory.createURI(KB + "SubNetwork"), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageNetwork"));
		UpdateRequest SewerageNetwork_ur = SewerageNetwork_ub.buildRequest();
		AccessAgentCaller.updateStore(sparqlendpoint, SewerageNetwork_ur.toString());

		for (int i = 1; i < KG_Main.length; i++) { //KG_Main.length
			UpdateBuilder KG_main_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + "MainNetwork"), NodeFactory.createURI(hasPart), NodeFactory.createURI(KB + KG_Main[i]));
			UpdateRequest KG_main_ur = KG_main_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, KG_main_ur.toString());   
		}

		for (int i = 1; i < KG_Sub.length; i++) { //KG_Sub.length
			UpdateBuilder KG_sub_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + "SubNetwork"), NodeFactory.createURI(hasPart), NodeFactory.createURI(KB + KG_Sub[i]));
			UpdateRequest KG_sub_ur = KG_sub_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, KG_sub_ur.toString());   
		}
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y \"None\"} where {?x ?y \"None\"}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialStz>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialStz>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialStz> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialStz> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialMasonry>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialMasonry>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialMasonry> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialMasonry> ?y ?z}"); 
	    AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChloride>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChloride>}");
	    AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChloride> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChloride> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialDuctileCastIron>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialDuctileCastIron>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialDuctileCastIron> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialDuctileCastIron> ?y ?z}"); 	    
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialKgDrainagePipes>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialKgDrainagePipes>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialKgDrainagePipes> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialKgDrainagePipes> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolypropylene>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolypropylene>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolypropylene> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolypropylene> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialOtherMaterial>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialOtherMaterial>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialOtherMaterial> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialOtherMaterial> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialGlassFiberReinforcedPlastic>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialGlassFiberReinforcedPlastic>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialGlassFiberReinforcedPlastic> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialGlassFiberReinforcedPlastic> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethylene>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethylene>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethylene> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethylene> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethyleneHighDensity>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethyleneHighDensity>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethyleneHighDensity> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyethyleneHighDensity> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialConcreteSegments>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialConcreteSegments>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialConcreteSegments> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialConcreteSegments> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPrestressedConcrete>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPrestressedConcrete>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPrestressedConcrete> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPrestressedConcrete> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPavingNaturalStone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPavingNaturalStone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPavingNaturalStone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPavingNaturalStone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialKgUltraRib>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialKgUltraRib>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialKgUltraRib> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialKgUltraRib> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChlorideModified>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChlorideModified>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChlorideModified> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPolyvinylChlorideModified> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialStainlessSteel>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialStainlessSteel>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialStainlessSteel> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialStainlessSteel> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialSteel>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialSteel>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialSteel> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialSteel> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialInSituConcrete>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialInSituConcrete>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialInSituConcrete> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialInSituConcrete> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPipesWithLongitudinalThreadedConnection>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialPipesWithLongitudinalThreadedConnection>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialPipesWithLongitudinalThreadedConnection> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialPipesWithLongitudinalThreadedConnection> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialWoundPipePehd>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/MaterialWoundPipePehd>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/MaterialWoundPipePehd> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/MaterialWoundPipePehd> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypek>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypek>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypek> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypek> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypeVentLine>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ChannelTypeVentLine>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypeVentLine> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ChannelTypeVentLine> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateb>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateb>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateb> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateb> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateOther>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateOther>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateOther> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateOther> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRemoved>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRemoved>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRemoved> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRemoved> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRebuilt>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRebuilt>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRebuilt> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateRebuilt> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateFictionalModel>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateFictionalModel>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateFictionalModel> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateFictionalModel> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStatep>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStatep>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStatep> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStatep> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/FunctionalStateNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/FunctionalStateNone> ?y ?z}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypep>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypep>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypep> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypep> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeOther>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeOther>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeOther> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeOther> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeSewageCompany>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeSewageCompany>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeSewageCompany> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeSewageCompany> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNotKnown>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNotKnown>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNotKnown> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeNotKnown> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeStateOperation>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeStateOperation>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeStateOperation> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeStateOperation> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalRailroad>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalRailroad>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalRailroad> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalRailroad> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypes>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypes>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypes> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypes> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalPropertyOfficeUsBarracks>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalPropertyOfficeUsBarracks>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalPropertyOfficeUsBarracks> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeFederalPropertyOfficeUsBarracks> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeConstructionAid>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeConstructionAid>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeConstructionAid> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeConstructionAid> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeMunicipality>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeMunicipality>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeMunicipality> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/OwnershipTypeMunicipality> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructuren> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFieldPath>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFieldPath>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFieldPath> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFieldPath> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureCountryRoad>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureCountryRoad>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureCountryRoad> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureCountryRoad> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalkOrBikePath>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalkOrBikePath>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalkOrBikePath> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalkOrBikePath> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureStairs>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureStairs>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureStairs> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureStairs> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureDevelopedLandParcel>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureDevelopedLandParcel>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureDevelopedLandParcel> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureDevelopedLandParcel> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureGrassVerge>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureGrassVerge>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureGrassVerge> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureGrassVerge> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFederalRoad>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFederalRoad>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFederalRoad> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureFederalRoad> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurePrivateRoad>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurePrivateRoad>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurePrivateRoad> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurePrivateRoad> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureConstructionRoad>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureConstructionRoad>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureConstructionRoad> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureConstructionRoad> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureField>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureField>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureField> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureField> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureMilitaryArea>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureMilitaryArea>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureMilitaryArea> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureMilitaryArea> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureEmbankment>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureEmbankment>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureEmbankment> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureEmbankment> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurep>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurep>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurep> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructurep> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureAgriculturalRoadPaved>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureAgriculturalRoadPaved>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureAgriculturalRoadPaved> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureAgriculturalRoadPaved> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureParkingStrip>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureParkingStrip>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureParkingStrip> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureParkingStrip> ?y ?z}");	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureh>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureh>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureh> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureh> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalk>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalk>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalk> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureSidewalk> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureUnknown>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureUnknown>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureUnknown> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureUnknown> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureOther>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureOther>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureOther> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/AssociatedInfrastructureOther> ?y ?z}");	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsagem>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsagem>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsagem> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsagem> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsages>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsages>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsages> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsages> ?y ?z}");	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageFictitiousAttitude>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageFictitiousAttitude>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageFictitiousAttitude> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageFictitiousAttitude> ?y ?z}");	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageOpenDitch>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageOpenDitch>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageOpenDitch> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageOpenDitch> ?y ?z}");		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageControlOrElectricCable>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageControlOrElectricCable>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageControlOrElectricCable> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageControlOrElectricCable> ?y ?z}");	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialRegulation>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialRegulation>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialRegulation> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialRegulation> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSTR>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSTR>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSTR> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSTR> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialUse>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialUse>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialUse> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageSpecialUse> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageHistory>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageHistory>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageHistory> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageHistory> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsagestk>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsagestk>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsagestk> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsagestk> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageBiochannel>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageUsageBiochannel>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageBiochannel> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageUsageBiochannel> ?y ?z}"); 	    
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeNone>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeNone>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/PipeTypeNone> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/PipeTypeNone> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeOther>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeOther>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/PipeTypeOther> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/PipeTypeOther> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypea>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypea>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/PipeTypea> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/PipeTypea> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeVenting>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/PipeTypeVenting>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/PipeTypeVenting> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/PipeTypeVenting> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeSanitaryObject>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeSanitaryObject>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeSanitaryObject> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeSanitaryObject> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypes>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypes>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypes> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypes> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeCleaningOpening>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeCleaningOpening>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeCleaningOpening> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeCleaningOpening> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeOther>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShaftTypeOther>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeOther> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShaftTypeOther> ?y ?z}"); 	    
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeCircleProfile>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeCircleProfile>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeCircleProfile> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeCircleProfile> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeTrapezoidal>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeTrapezoidal>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeTrapezoidal> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeTrapezoidal> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeUShaped>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeUShaped>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeUShaped> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeUShaped> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeMouth>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeMouth>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeMouth> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeMouth> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeChannelCrossSection>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeChannelCrossSection>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeChannelCrossSection> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeChannelCrossSection> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeArcShapedHood>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeArcShapedHood>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeArcShapedHood> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeArcShapedHood> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/Shape00>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/Shape00>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/Shape00> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/Shape00> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeOther>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapeOther>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapeOther> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapeOther> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapePolygon>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ShapePolygon>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ShapePolygon> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ShapePolygon> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeConcealedManhole>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeConcealedManhole>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeConcealedManhole> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeConcealedManhole> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypePumpingStation>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypePumpingStation>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypePumpingStation> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypePumpingStation> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletStructure>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletStructure>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletStructure> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletStructure> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutlet>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutlet>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutlet> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutlet> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeInlet>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeInlet>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeInlet> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeInlet> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeSpecialStructure>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeSpecialStructure>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeSpecialStructure> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeSpecialStructure> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeStructure>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeStructure>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeStructure> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeStructure> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeRectangularProfileType1>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeRectangularProfileType1>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeRectangularProfileType1> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeRectangularProfileType1> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeDiffuser>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeDiffuser>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeDiffuser> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeDiffuser> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletReceivingWater>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletReceivingWater>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletReceivingWater> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeOutletReceivingWater> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeExpiration>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeExpiration>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeExpiration> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeExpiration> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypePlasterOpening>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypePlasterOpening>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypePlasterOpening> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypePlasterOpening> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainInlet>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainInlet>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainInlet> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainInlet> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypen03>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypen03>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypen03> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypen03> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainwaterRetentionBasin>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainwaterRetentionBasin>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainwaterRetentionBasin> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeRainwaterRetentionBasin> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeBlindShaft>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/StructureTypeBlindShaft>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeBlindShaft> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/StructureTypeBlindShaft> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidEmergencyDischarge>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidEmergencyDischarge>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidEmergencyDischarge> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidEmergencyDischarge> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWasteWaterFromSpecialFacilities>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWasteWaterFromSpecialFacilities>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWasteWaterFromSpecialFacilities> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWasteWaterFromSpecialFacilities> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidCommercialWastewater>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidCommercialWastewater>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidCommercialWastewater> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidCommercialWastewater> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluid2QsCollector>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluid2QsCollector>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluid2QsCollector> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluid2QsCollector> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWaterPollutingSubstances>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWaterPollutingSubstances>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWaterPollutingSubstances> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidWaterPollutingSubstances> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidh>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/SewerageFluidh>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidh> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/SewerageFluidh> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassFictional>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassFictional>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassFictional> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassFictional> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassG>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassG>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassClassG> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassClassG> ?y ?z}"); 	
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassH>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassH>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassClassH> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassClassH> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassA>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassA>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassClassA> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassClassA> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassF>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassClassF>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassClassF> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassClassF> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassConical>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/ClassConical>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/ClassConical> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/ClassConical> ?y ?z}"); 		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/Classunknown>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/Classunknown>}");
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {<https://www.theworldavatar.com/kb/ontosewage/Classunknown> ?y ?z} where {<https://www.theworldavatar.com/kb/ontosewage/Classunknown> ?y ?z}");	 
	}

	
	public static void BranchInstantiation (String HG_Branch) {
		int HG_Branch_length = 0;
		try {
			HG_Branch_length = ColNum(HG_Branch, ",");
		} catch (java.io.IOException e) {
			e.printStackTrace();
		}

		for (int i = 0; i < HG_Branch_length; i++) { //HG_Branch_length; i++) {
			String[] BR_Instance = ReadCol(i, HG_Branch, ","); 
			String BR_Instance_Name = BR_Instance[0];
			
			UpdateBuilder MainPipe_ub = 
					new UpdateBuilder()
					.addInsert(NodeFactory.createURI(KB + "BranchConnection" + BR_Instance_Name), NodeFactory.createURI(OS + "hasMainPipe"), NodeFactory.createURI(KB + BR_Instance_Name));		
			UpdateRequest MainPipe_ur = MainPipe_ub.buildRequest();
			AccessAgentCaller.updateStore(sparqlendpoint, MainPipe_ur.toString());   
			
			for (int j = 1; j < BR_Instance.length; j++) {
				UpdateBuilder BranchPipe_ub = 
						new UpdateBuilder()
						.addInsert(NodeFactory.createURI(KB + "BranchConnection" + BR_Instance_Name), NodeFactory.createURI(OS + "hasBranchPipe"), NodeFactory.createURI(KB + BR_Instance[j]));	
				UpdateRequest BranchPipe_ur = BranchPipe_ub.buildRequest();
				AccessAgentCaller.updateStore(sparqlendpoint, BranchPipe_ur.toString());   
			}
			
		}
		
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y \"None\"} where {?x ?y \"None\"}"); 
		AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y <https://www.theworldavatar.com/kb/ontosewage/None>} where {?x ?y <https://www.theworldavatar.com/kb/ontosewage/None>}"); 
	}

	public static void omHasValue(String Instance, String Unit, String NumericalValue) {
		UpdateBuilder omHasValue_ub =
				new UpdateBuilder()
				.addInsert(NodeFactory.createURI(KB + Instance), NodeFactory.createURI(OM_HAS_VALUE), NodeFactory.createURI(KB + "Measure" + Instance))
				.addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_MEASURE))
				.addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(OM_HAS_UNIT), NodeFactory.createURI(KB + Unit))
				.addInsert(NodeFactory.createURI(KB + Unit), NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_UNIT))
				.addInsert(NodeFactory.createURI(KB + Unit), NodeFactory.createURI(OM_SYMBOL), Unit)
				.addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(OM_Has_NUMERICAL_VALUE), NumericalValue);
		UpdateRequest omHasValue_ur = omHasValue_ub.buildRequest();
		AccessAgentCaller.updateStore(sparqlendpoint, omHasValue_ur.toString());
	}

	public static int ColNum(String filepath, String delimiter) throws java.io.IOException {
		FileReader fr_col;
		String[] currentLine;
		fr_col = new FileReader(filepath);
		BufferedReader br_col = new BufferedReader(fr_col);
		currentLine = br_col.readLine().split(",");
		int col_length = currentLine.length;
		return col_length;
	}

	public static String[] ReadCol(int col, String filepath, String delimiter) {
		String currentLine;
		String[] data;
		ArrayList<String> colData = new ArrayList<String>();

		try {
			FileReader fr = new FileReader(filepath);
			BufferedReader br = new BufferedReader(fr);
			while ((currentLine = br.readLine()) != null) {
				data = currentLine.split(delimiter);
				colData.add(data[col]);
			}
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
		return colData.toArray(new String[0]);
	}

}
