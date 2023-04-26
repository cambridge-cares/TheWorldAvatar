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
@WebServlet(urlPatterns = { "/performsewageupdate" })

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
			 KGMainSubNetInstantiation(KG_MainNet,KG_SubNet);
			
			AccessAgentCaller.updateStore(sparqlendpoint, "delete {?x ?y \"None\"} where {?x ?y \"None\"}");
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

		UpdateBuilder HG_instantiation_ub = new UpdateBuilder();

		for (int i = 1; i < HG_column_length; i++) { // HG_column_length; i++) {
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

			HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(RDF_TYPE),
					NodeFactory.createURI(s4watr + "Pipe"));

			if (!HG001.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
						NodeFactory.createURI(OS + "componentID"), HG001);
			}

			if (!HG108.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "isAssociatedWith"),
								NodeFactory.createURI(KB + HG_Instance_Name + "SewagePlant" + HG108))
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "SewagePlant" + HG108),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewagePlant"));
			}

			if (HG302.equals("PressureDrainage") || HG302.equals("MixedProcess") || HG302.equals("RainWater")
					|| HG302.equals("WasteWater") || HG302.equals("StreetDrainage") || HG302.equals("PreFlood")
					|| HG302.equals("WaterPipe")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "hasUsage"),
								NodeFactory.createURI(KB + HG_Instance_Name + "SewerageUsage" + HG302))
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "SewerageUsage" + HG302),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG302));
			}

			if (!HG401.equals("None") || !HG402.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasSewerageRecords"),
								NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageRecords"));

				if (HG402.equals("Public") || HG402.equals("Private") || HG402.equals("City")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasOwnershipType"),
									NodeFactory.createURI(KB + HG_Instance_Name + "OwnershipType" + HG402))
							.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "OwnershipType" + HG402),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG402));
				}

				if (HG401.equals("Constructed") || HG401.equals("Stock") || HG401.equals("OutOfService")
						|| HG401.equals("Planning") || HG401.equals("Insulated")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasFunctionalState"),
									NodeFactory.createURI(KB + HG_Instance_Name + "FunctionalState" + HG401))
							.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "FunctionalState" + HG401),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG401));
				}

			}

			if (!HG303.equals("None") || !HG304.equals("None") || !HG301.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasConstructionProperties"),
								NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ConstructionProperties"));

				if (!HG303.equals("None")) {
					HG_instantiation_ub.addInsert(
							NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name),
							NodeFactory.createURI(OS + "constructionYear"), HG303);
				}

				if (HG304.equals("Concrete") || HG304.equals("PolyvinylChlorideHard")
						|| HG304.equals("ReinforcedConcrete") || HG304.equals("Stoneware")
						|| HG304.equals("GrayCastIron")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name),
									NodeFactory.createURI(BMO + "hasMaterial"),
									NodeFactory.createURI(KB + HG_Instance_Name + "Material" + HG304))
							.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "Material" + HG304),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(BMO + HG304));
				}

				if (HG301.equals("Penstock") || HG301.equals("OpenGravityPipeline")
						|| HG301.equals("ClosedGravityPipeline")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasChannelType"),
									NodeFactory.createURI(KB + HG_Instance_Name + "ChannelType" + HG301))
							.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "ChannelType" + HG301),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG301));
				}
			}

			if (!HG003.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name),
						NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "BranchConnection"));

				if (!HG008.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name),
							NodeFactory.createURI(OS + "isInFlowDirection"), HG008);
				}

				if (!HG009.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name),
							NodeFactory.createURI(OS + "clockPositionOfBranchPipe"), HG009);
				}

				if (!HG007.equals("None")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "BranchConnection" + HG_Instance_Name),
									NodeFactory.createURI(OS + "relativeDistanceOnMainPipe"),
									NodeFactory.createURI(
											KB + "DistanceOnMainPipe" + "BranchConnection" + HG_Instance_Name))
							.addInsert(
									NodeFactory.createURI(
											KB + "DistanceOnMainPipe" + "BranchConnection" + HG_Instance_Name),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_DISTANCE));
					HG_instantiation_ub = omHasValue("DistanceOnMainPipe" + "BranchConnection" + HG_Instance_Name,
							"metre", HG007, HG_instantiation_ub);
				}
			}

			if (!HG305.equals("None") || !HG306.equals("None") || !HG307.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasCrossSection"),
								NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "CrossSection"));

				if (HG305.equals("EggShaped") || HG305.equals("OpenTrench") || HG305.equals("SemiCircle")
						|| HG305.equals("Circle") || HG305.equals("Rectangle") || HG305.equals("Square")
						|| HG305.equals("Round") || HG305.equals("Angular")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasShape"),
									NodeFactory.createURI(KB + HG_Instance_Name + "Shape" + HG305))
							.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "Shape" + HG305),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG305));
				}

				if (!HG307.equals("None")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasHeight"),
									NodeFactory.createURI(KB + "Height" + HG_Instance_Name))
							.addInsert(NodeFactory.createURI(KB + "Height" + HG_Instance_Name),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT));
					HG_instantiation_ub = omHasValue("Height" + HG_Instance_Name, "millimetre", HG307,
							HG_instantiation_ub);
				}

				if (!HG306.equals("None")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "CrossSection" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasWidth"),
									NodeFactory.createURI(KB + "Width" + HG_Instance_Name))
							.addInsert(NodeFactory.createURI(KB + "Width" + HG_Instance_Name),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_WIDTH));
					HG_instantiation_ub = omHasValue("Width" + HG_Instance_Name, "millimetre", HG306,
							HG_instantiation_ub);
				}

			}

			if (HG500.equals("RainWater") || HG500.equals("DomesticWasteWater")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name), NodeFactory.createURI(OS + "usedFor"),
								NodeFactory.createURI(KB + HG_Instance_Name + "SewerageFluid" + HG500))
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "SewerageFluid" + HG500),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG500));
			}

			if (!HG011.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
						NodeFactory.createURI(OS + "hasConnectionID"), HG011);
			}

			if (!HG005.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
						NodeFactory.createURI(OS + "hasSourceID"), HG005);
			}

			if (HG313.equals("ConnectionLine") || HG313.equals("Pipe") || HG313.equals("ConnectingDuct")
					|| HG313.equals("ReliefLine")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasPipeType"),
								NodeFactory.createURI(KB + HG_Instance_Name + "PipeType" + HG313))
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "PipeType" + HG313),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG313));
			}

			if (!HG311.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasInclination"),
								NodeFactory.createURI(KB + "Inclination" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "Inclination" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "Inclination"));
				HG_instantiation_ub = omHasValue("Inclination" + HG_Instance_Name, "Percentage", HG311,
						HG_instantiation_ub);
			}

			if (!HG310.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasLength"),
								NodeFactory.createURI(KB + "Length" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "Length" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_LENGTH));
				HG_instantiation_ub = omHasValue("Length" + HG_Instance_Name, "metre", HG310, HG_instantiation_ub);
			}

			if (!HG410.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasWallThickness"),
								NodeFactory.createURI(KB + "WallThickness" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "WallThickness" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_THICKNESS));
				HG_instantiation_ub = omHasValue("Thickness" + HG_Instance_Name, "metre", HG410, HG_instantiation_ub);
			}

			if (!HG003.equals("None") || !HG004.equals("None") || !HG006.equals("None") || !HG010.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasConnectionProperties"),
								NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ConnectionProperties"));

				if (!HG003.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasUpstreamConnector"), HG003);
				}

				if (!HG004.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasDownstreamConnector"), HG004);
				}

				if (!HG006.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasEndpointObject"), HG006);
				}

				if (HG010.equals("PropertyConnection") || HG010.equals("StreetInlet")
						|| HG010.equals("ManholeEndpoint")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "ConnectionProperties" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasEndpointType"),
									NodeFactory.createURI(KB + HG_Instance_Name + "hasEndpointType" + HG010))
							.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "hasEndpointType" + HG010),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG010));
				}
			}

			HG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasInletLocation"),
							NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "location"));

			if (!HG403.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name),
						NodeFactory.createURI(OS + "isInWaterProtectionZone"), HG403);
			}

			if (!HG406.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name),
						NodeFactory.createURI(OS + "isInFloodplane"), HG406);
			}

			if (HG404.equals("Terrain") || HG404.equals("SideWalk") || HG404.equals("MainRoad")
					|| HG404.equals("SideRoad") || HG404.equals("PrivateProperty")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name),
								NodeFactory.createURI(OS + "isAssociatedWith"),
								NodeFactory.createURI(KB + HG_Instance_Name + "AssociatedInfrastructure" + HG404))
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "AssociatedInfrastructure" + HG404),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG404));
			}

			if (!HG_GP008A.equals("None") || !HG_GP002.equals("None") || !HG_GP003A.equals("None")
					|| !HG_GP004A.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasCoordinates"),
								NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "GeoCoordinates"));

				if (!HG_GP008A.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasPositionAccuracy"), HG_GP008A);
				}

				if (!HG_GP002.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasCoordinateReference"), HG_GP002);
				}

				if (!HG_GP003A.equals("None") && !HG_GP004A.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "GeoCoordinateI" + HG_Instance_Name),
							NodeFactory.createURI(ogc + "asWKT"),
							"POINT(" + HG_GP003A + "," + HG_GP004A + ")^^ogc:wktLiteral");
				}
			}

			if (!HG_GP001A.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name),
						NodeFactory.createURI(OS + "hasPointDesignation"), HG_GP001A);
			}

			if (!HG_GP010A.equals("None") || !HG_GP009A.equals("None") || !HG_GP007A.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "LocationI" + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasElevation"),
								NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "elevation"));

				if (!HG_GP010A.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasElevationReference"), HG_GP010A);
				}

				if (!HG_GP009A.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasElevationAccuracy"), HG_GP009A);
				}

				if (!HG_GP007A.equals("None")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasElevationAboveSeaLevel"),
									NodeFactory.createURI(KB + "ElevationHeightI" + HG_Instance_Name))
							.addInsert(NodeFactory.createURI(KB + "ElevationHeightI" + HG_Instance_Name),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT));
					HG_instantiation_ub = omHasValue("ElevationHeightI" + HG_Instance_Name, "metre", HG_GP007A,
							HG_instantiation_ub);
				}
			}

			if (!HG107.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name),
						NodeFactory.createURI(OS + "hasCatchmentAreaKey"), HG107);
			}

			if (!HG103.equals("None") || !HG104.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name),
								NodeFactory.createURI(sio + "SIO_000061"),
								NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "District"));

				if (!HG103.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasDistrictReference"), HG103);
				}

				if (!HG104.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasDistrictName"), HG104);
				}
			}

			if (!HG101.equals("None") || !HG102.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "ElevationI" + HG_Instance_Name),
								NodeFactory.createURI(sio + "SIO_000061"),
								NodeFactory.createURI(KB + "Street" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "Street"));

				if (!HG101.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasStreetReference"), HG101);
				}

				if (!HG102.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasStreetName"), HG102);
				}
			}

			HG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasOutletLocation"),
							NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "location"));

			if (!HG403.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name),
						NodeFactory.createURI(OS + "isInWaterProtectionZone"), HG403);
			}

			if (!HG406.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name),
						NodeFactory.createURI(OS + "isInFloodplane"), HG406);
			}

			if (HG404.equals("Terrain") || HG404.equals("SideWalk") || HG404.equals("MainRoad")
					|| HG404.equals("SideRoad") || HG404.equals("PrivateProperty")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name),
								NodeFactory.createURI(OS + "isAssociatedWith"),
								NodeFactory.createURI(KB + HG_Instance_Name + "AssociatedInfrastructure" + HG404))
						.addInsert(NodeFactory.createURI(KB + HG_Instance_Name + "AssociatedInfrastructure" + HG404),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + HG404));
			}

			if (!HG_GP008B.equals("None") || !HG_GP002.equals("None") || !HG_GP003B.equals("None")
					|| !HG_GP004B.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasCoordinates"),
								NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "GeoCoordinates"));

				if (!HG_GP008B.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasPositionAccuracy"), HG_GP008B);
				}

				if (!HG_GP002.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasCoordinateReference"), HG_GP002);
				}

				if (!HG_GP003B.equals("None") && !HG_GP004B.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "GeoCoordinateO" + HG_Instance_Name),
							NodeFactory.createURI(ogc + "asWKT"),
							"POINT(" + HG_GP003B + "," + HG_GP004B + ")^^ogc:wktLiteral");
				}
			}

			if (!HG_GP001B.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name),
						NodeFactory.createURI(OS + "hasPointDesignation"), HG_GP001B);
			}

			if (!HG_GP010B.equals("None") || !HG_GP009B.equals("None") || !HG_GP007B.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "LocationO" + HG_Instance_Name),
								NodeFactory.createURI(OS + "hasElevation"),
								NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "elevation"));

				if (!HG_GP010B.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ElevationOn" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasElevationReference"), HG_GP010B);
				}

				if (!HG_GP009B.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasElevationAccuracy"), HG_GP009B);
				}

				if (!HG_GP007B.equals("None")) {
					HG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name),
									NodeFactory.createURI(OS + "hasElevationAboveSeaLevel"),
									NodeFactory.createURI(KB + "ElevationHeightO" + HG_Instance_Name))
							.addInsert(NodeFactory.createURI(KB + "ElevationHeightO" + HG_Instance_Name),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT));
					HG_instantiation_ub = omHasValue("ElevationHeightO" + HG_Instance_Name, "metre", HG_GP007B,
							HG_instantiation_ub);
				}
			}

			if (!HG107.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name),
						NodeFactory.createURI(OS + "hasCatchmentAreaKey"), HG107);
			}

			if (!HG103.equals("None") || !HG104.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name),
								NodeFactory.createURI(sio + "SIO_000061"),
								NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "District"));

				if (!HG103.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasDistrictReference"), HG103);
				}

				if (!HG104.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Distrcit" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasDistrictName"), HG104);
				}
			}

			if (!HG101.equals("None") || !HG102.equals("None")) {
				HG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + "ElevationO" + HG_Instance_Name),
								NodeFactory.createURI(sio + "SIO_000061"),
								NodeFactory.createURI(KB + "Street" + HG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(juso + "Street"));

				if (!HG101.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasStreetReference"), HG101);
				}

				if (!HG102.equals("None")) {
					HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "Street" + HG_Instance_Name),
							NodeFactory.createURI(OS + "hasStreetName"), HG102);
				}
			}

			if (HG006.equals("None")) {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "MainNetwork"), NodeFactory.createURI(hasPart),
						NodeFactory.createURI(KB + HG_Instance_Name));
			} else {
				HG_instantiation_ub.addInsert(NodeFactory.createURI(KB + "SubNetwork"), NodeFactory.createURI(hasPart),
						NodeFactory.createURI(KB + HG_Instance_Name));
			}
		}

		UpdateRequest HG_instantiation_ur = HG_instantiation_ub.buildRequest();
		AccessAgentCaller.updateStore(sparqlendpoint, HG_instantiation_ur.toString());
	}

	public static void KGInstantiation(String KG_Path) {
		int KG_column_length = 0;
		try {
			KG_column_length = ColNum(KG_Path, ",");
		} catch (java.io.IOException e) {
			e.printStackTrace();
		}
		
		UpdateBuilder KG_instantiation_ub = new UpdateBuilder();

		for (int i = 1; i < KG_column_length; i++) { // KG_column_length; i++) {
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
			String KG_GP001 = KG_Instance[24];
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

			KG_instantiation_ub.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE),
							NodeFactory.createURI(s4watr + "Manhole"));
			
			if (!KG001.equals("None")) {
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "componentID"),
						KG001);
			}
					
			if (!KG108.equals("None")) {
				KG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + KG_Instance_Name),
								NodeFactory.createURI(OS + "isAssociatedWith"),
								NodeFactory.createURI(KB + KG_Instance_Name + "SewagePlant" + KG108))
						.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "SewagePlant" + KG108),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewagePlant"));
			}
			
			if (KG302.equals("PressureDrainage") || KG302.equals("MixedProcess") || KG302.equals("RainWater")
					|| KG302.equals("WasteWater") || KG302.equals("StreetDrainage") || KG302.equals("PreFlood")
					|| KG302.equals("WaterPipe")) {
				KG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasUsage"),
								NodeFactory.createURI(KB + KG_Instance_Name + "SewerageUsage" + KG302))
						.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "SewerageUsage" + KG302),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + KG302));
			}
			
			if (!KG401.equals("None") || !KG402.equals("None")) {
				KG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + KG_Instance_Name),
								NodeFactory.createURI(OS + "hasSewerageRecords"),
								NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "SewerageRecords"));

				if (KG402.equals("Public") || KG402.equals("Private") || KG402.equals("City")) {
					KG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name),
									NodeFactory.createURI(OS + "hasOwnershipType"),
									NodeFactory.createURI(KB + KG_Instance_Name + "OwnershipType" + KG402))
							.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "OwnershipType" + KG402),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + KG402));
				}

				if (KG401.equals("Constructed") || KG401.equals("Stock") || KG401.equals("OutOfService")
						|| KG401.equals("Planning") || KG401.equals("Insulated")) {
					KG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "SewerageRecords" + KG_Instance_Name),
									NodeFactory.createURI(OS + "hasFunctionalState"),
									NodeFactory.createURI(KB + KG_Instance_Name + "FunctionalState" + KG401))
							.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "FunctionalState" + KG401),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + KG401));
				}

			}
					
			if (!KG303.equals("None") || !KG304.equals("None") || !KG301.equals("None")) {
				KG_instantiation_ub
						.addInsert(NodeFactory.createURI(KB + KG_Instance_Name),
								NodeFactory.createURI(OS + "hasConstructionProperties"),
								NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name))
						.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + "ConstructionProperties"));

				if (!KG303.equals("None")) {
					KG_instantiation_ub.addInsert(
							NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name),
							NodeFactory.createURI(OS + "constructionYear"), KG303);
				}

				if (KG304.equals("Concrete") || KG304.equals("PolyvinylChlorideHard")
						|| KG304.equals("ReinforcedConcrete") || KG304.equals("Stoneware")
						|| KG304.equals("GrayCastIron")) {
					KG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name),
									NodeFactory.createURI(BMO + "hasMaterial"),
									NodeFactory.createURI(KB + KG_Instance_Name + "Material" + KG304))
							.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "Material" + KG304),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(BMO + KG304));
				}

				if (KG301.equals("Penstock") || KG301.equals("OpenGravityPipeline")
						|| KG301.equals("ClosedGravityPipeline")) {
					KG_instantiation_ub
							.addInsert(NodeFactory.createURI(KB + "ConstructionProperties" + KG_Instance_Name),
									NodeFactory.createURI(OS + "hasChannelType"),
									NodeFactory.createURI(KB + KG_Instance_Name + "ChannelType" + KG301))
							.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "ChannelType" + KG301),
									NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + KG301));
				}
			}		

			if (!KG306.equals("None")) {
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + KG_Instance_Name),
						NodeFactory.createURI(OS + "hasStructureType"),
						NodeFactory.createURI(KB + KG_Instance_Name + "StructureType" + KG306))
				.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "StructureType" + KG306), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OS + KG306));
			}

			if (!KG316.equals("None") || !KG319.equals("None") || !KG318.equals("None")) {
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasFlume"),
						NodeFactory.createURI(KB + "Flume" + KG_Instance_Name))
				.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OS + "Flume"));
				
				if (KG316.equals("EggShaped") || KG316.equals("OpenTrench") || KG316.equals("SemiCircle")
						|| KG316.equals("Circle") || KG316.equals("Rectangle") ||KG316.equals("Square")
						|| KG316.equals("Round") || KG316.equals("Angular")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasShape"), NodeFactory.createURI(KB + KG_Instance_Name + "Shape" + KG316))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "Shape" + KG316), NodeFactory.createURI(RDF_TYPE),
							NodeFactory.createURI(OS + KG316));
				}
				
				if (!KG319.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasLength"),
							NodeFactory.createURI(KB + "Length" + "Flume" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Length" + "Flume" + KG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_LENGTH));
					KG_instantiation_ub = omHasValue("Length" + "Flume" + KG_Instance_Name, "millimetre", KG319, KG_instantiation_ub);
				}
				
                if (!KG318.equals("None")) {
                	KG_instantiation_ub
                	.addInsert(NodeFactory.createURI(KB + "Flume" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasWidth"),
							NodeFactory.createURI(KB + "Width" + "Flume" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Width" + "Flume" + KG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_WIDTH));
                	KG_instantiation_ub = omHasValue("Width" + "Flume" + KG_Instance_Name, "millimetre", KG318, KG_instantiation_ub);
				}
			}
					
			if (!KG305.equals("None") || !KG307.equals("None") || !KG308.equals("None") || !KG309.equals("None") || !KG211.equals("None") ) {
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasShaft"),
						NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name))
				.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OS + "Shaft"));
				
				if (KG307.equals("EggShaped") || KG307.equals("OpenTrench") || KG307.equals("SemiCircle")
						|| KG307.equals("Circle") || KG307.equals("Rectangle") ||KG307.equals("Square")
						|| KG307.equals("Round") || KG307.equals("Angular")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasShape"), NodeFactory.createURI(KB + KG_Instance_Name + "Shape" + KG307))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "Shape" + KG307), NodeFactory.createURI(RDF_TYPE),
							NodeFactory.createURI(OS + KG307));
				}
				
				if (KG305.equals("Outlet") || KG305.equals("Structure") || KG305.equals("StreetInletShaft")
						|| KG305.equals("FictionalShaft") || KG305.equals("BuildingConnection") 
						|| KG305.equals("InspectionOpening") || KG305.equals("ShaftNode")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasShaftType"), NodeFactory.createURI(KB + KG_Instance_Name + "ShaftType" + KG305))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "ShaftType" + KG305), NodeFactory.createURI(RDF_TYPE),
							NodeFactory.createURI(OS + KG305));
				}
				
				if (!KG308.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasLength"),
							NodeFactory.createURI(KB + "Length" + "Shaft" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Length" + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_LENGTH));
					KG_instantiation_ub = omHasValue("Length" + "Shaft" + KG_Instance_Name, "millimetre", KG308, KG_instantiation_ub);
				}
				
				if (!KG309.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasWidth"),
							NodeFactory.createURI(KB + "Width" + "Shaft" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Width" + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_WIDTH));
					KG_instantiation_ub = omHasValue("Width" + "Shaft" + KG_Instance_Name, "millimetre", KG309, KG_instantiation_ub);
				}
				
				if (!KG211.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasDepth"),
							NodeFactory.createURI(KB + "Depth" + "Shaft" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Depth" + "Shaft" + KG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_DEPTH));
					KG_instantiation_ub = omHasValue("Depth" + "Shaft" + KG_Instance_Name, "metre", KG211, KG_instantiation_ub);
				}
			}
					
			if (!KG311.equals("None") || !KG312.equals("None") || !KG310.equals("None")) {
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(OS + "hasCover"),
						NodeFactory.createURI(KB + "Cover" + KG_Instance_Name))
				.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OS + "Cover"));
				
				if (KG311.equals("Concrete") || KG311.equals("PolyvinylChlorideHard")
						|| KG311.equals("ReinforcedConcrete") || KG311.equals("Stoneware")
						|| KG311.equals("GrayCastIron")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name),
							NodeFactory.createURI(BMO + "hasMaterial"), NodeFactory.createURI(KB + KG_Instance_Name + "Material" + KG311))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "Material" + KG311), NodeFactory.createURI(RDF_TYPE),
							NodeFactory.createURI(BMO + KG311));
				}
				
				if (KG312.equals("ClassB") || KG312.equals("ClassD")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasClass"), NodeFactory.createURI(KB + KG_Instance_Name + "Class" + KG312))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "Class" + KG312), NodeFactory.createURI(RDF_TYPE),
							NodeFactory.createURI(OS + KG312));
				}
				
				if (KG310.equals("EggShaped") || KG310.equals("OpenTrench") || KG310.equals("SemiCircle")
						|| KG310.equals("Circle") || KG310.equals("Rectangle") || KG310.equals("Square")
						|| KG310.equals("Round") || KG310.equals("Angular")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Cover" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasShape"), NodeFactory.createURI(KB + KG_Instance_Name + "Shape" + KG310))
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "Shape" + KG310), NodeFactory.createURI(RDF_TYPE),
							NodeFactory.createURI(OS + KG310));
				}	
			}
			
	        KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + KG_Instance_Name), NodeFactory.createURI(dul + "hasLocation"),
							NodeFactory.createURI(KB + "Location" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "location"));
	        
	        if (!KG403.equals("None")) {
	        	KG_instantiation_ub
	        	.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name),
						NodeFactory.createURI(OS + "isInWaterProtectionZone"), KG403);
	        }
					
	        if (!KG406.equals("None")) {	
	        	KG_instantiation_ub
	        	.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name),
						NodeFactory.createURI(OS + "isInFloodplane"), KG406);
	        }
	        
			if (KG404.equals("Terrain") || KG404.equals("SideWalk") || KG404.equals("MainRoad")
					|| KG404.equals("SideRoad") || KG404.equals("PrivateProperty")) {
				KG_instantiation_ub
				        .addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name),
						        NodeFactory.createURI(OS + "isAssociatedWith"),
								NodeFactory.createURI(KB + KG_Instance_Name + "AssociatedInfrastructure" + KG404))
						.addInsert(NodeFactory.createURI(KB + KG_Instance_Name + "AssociatedInfrastructure" + KG404),
								NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OS + KG404));
			}
					
			if (!KG_GP001.equals("None")) {	
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name),
						NodeFactory.createURI(OS + "hasPointDesignation"), KG_GP001);
			}

			if (!KG_GP003B.equals("None") && !KG_GP004B.equals("None") && !KG_GP008B.equals("None") || !KG_GP002.equals("None")) {
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name),
						NodeFactory.createURI(OS + "hasCoordinates"),
						NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name))
				.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name),
						NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "GeoCoordinates"));
				
				if (!KG_GP008B.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasPositionAccuracy"), KG_GP008B);
				}
					
				if (!KG_GP002.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasCoordinateReference"), KG_GP002);
				}
				
				if (!KG_GP003B.equals("None") && !KG_GP004B.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "GeoCoordinate" + KG_Instance_Name),
							NodeFactory.createURI(ogc + "asWKT"),
							"POINT(" + KG_GP003B + "," + KG_GP004B + ")^^ogc:wktLiteral");
				}
			}
				
			if (!KG_GP010.equals("None") || !KG_GP009B.equals("None") || !KG_GP007B.equals("None")) {
				KG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + "Location" + KG_Instance_Name),
						NodeFactory.createURI(OS + "hasElevation"),
						NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name))
				.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name),
						NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(schema + "elevation"));
				
				if (!KG_GP010.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasElevationReference"), KG_GP010);
				}
				
				if (!KG_GP009B.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasElevationAccuracy"), KG_GP009B);
				}
				
				if (!KG_GP007B.equals("None")) {
					KG_instantiation_ub
					.addInsert(NodeFactory.createURI(KB + "Elevation" + KG_Instance_Name),
							NodeFactory.createURI(OS + "hasElevationAboveSeaLevel"),
							NodeFactory.createURI(KB + "ElevationAboveSeaLevel" + KG_Instance_Name))
					.addInsert(NodeFactory.createURI(KB + "ElevationAboveSeaLevel" + KG_Instance_Name),
							NodeFactory.createURI(RDF_TYPE), NodeFactory.createURI(OM_HEIGHT));
					KG_instantiation_ub =  omHasValue("ElevationAboveSeaLevel" + KG_Instance_Name, "metre", KG_GP007B, KG_instantiation_ub);
				}	
			}
		}

		UpdateRequest KG_instantiation_ur = KG_instantiation_ub.buildRequest();
		AccessAgentCaller.updateStore(sparqlendpoint, KG_instantiation_ur.toString());
	}

	public static void KGMainSubNetInstantiation(String KG_MainNet, String KG_SubNet) {
		String[] KG_Main = ReadCol(0, KG_MainNet, ",");
		String[] KG_Sub = ReadCol(0, KG_SubNet, ",");

		UpdateBuilder SewerageNetwork_ub = new UpdateBuilder()
				.addInsert(NodeFactory.createURI(KB + "MainNetwork"), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OS + "SewerageNetwork"))
				.addInsert(NodeFactory.createURI(KB + "SubNetwork"), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OS + "SewerageNetwork"));

		for (int i = 1; i < KG_Main.length; i++) { // KG_Main.length
			if (!KG_Main[i].equals("None")) {
				SewerageNetwork_ub
				.addInsert(NodeFactory.createURI(KB + "MainNetwork"),
						NodeFactory.createURI(hasPart), NodeFactory.createURI(KB + KG_Main[i]));	
			}
		}

		for (int i = 1; i < KG_Sub.length; i++) { // KG_Sub.length
			if (!KG_Sub[i].equals("None")) {
				SewerageNetwork_ub
				.addInsert(NodeFactory.createURI(KB + "SubNetwork"),
						NodeFactory.createURI(hasPart), NodeFactory.createURI(KB + KG_Sub[i]));
			}
		}
		
		UpdateRequest SewerageNetwork_ur = SewerageNetwork_ub.buildRequest();
		AccessAgentCaller.updateStore(sparqlendpoint, SewerageNetwork_ur.toString());
		}

	public static void BranchInstantiation(String HG_Branch) {
		int HG_Branch_length = 0;
		try {
			HG_Branch_length = ColNum(HG_Branch, ",");
		} catch (java.io.IOException e) {
			e.printStackTrace();
		}
		
		UpdateBuilder Branch_ub = new UpdateBuilder();

		for (int i = 0; i < HG_Branch_length; i++) { // HG_Branch_length; i++) {
			String[] BR_Instance = ReadCol(i, HG_Branch, ",");
			String BR_Instance_Name = BR_Instance[0];

			Branch_ub
			.addInsert(NodeFactory.createURI(KB + "BranchConnection" + BR_Instance_Name),
					   NodeFactory.createURI(OS + "hasMainPipe"), NodeFactory.createURI(KB + BR_Instance_Name));

			for (int j = 1; j < BR_Instance.length; j++) {
				if (!BR_Instance[j].equals("None")) {
					Branch_ub
					.addInsert(NodeFactory.createURI(KB + "BranchConnection" + BR_Instance_Name),
							   NodeFactory.createURI(OS + "hasBranchPipe"), NodeFactory.createURI(KB + BR_Instance[j]));
					}
			}
		}

		UpdateRequest Branch_ur = Branch_ub.buildRequest();
		AccessAgentCaller.updateStore(sparqlendpoint, Branch_ur.toString());	
	}

	public static UpdateBuilder omHasValue(String Instance, String Unit, String NumericalValue,
			UpdateBuilder HG_instantiation_ub) {
		HG_instantiation_ub
				.addInsert(NodeFactory.createURI(KB + Instance), NodeFactory.createURI(OM_HAS_VALUE),
						NodeFactory.createURI(KB + "Measure" + Instance))
				.addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OM_MEASURE))
				.addInsert(NodeFactory.createURI(KB + "Measure" + Instance), NodeFactory.createURI(OM_HAS_UNIT),
						NodeFactory.createURI(KB + Unit))
				.addInsert(NodeFactory.createURI(KB + Unit), NodeFactory.createURI(RDF_TYPE),
						NodeFactory.createURI(OM_UNIT))
				.addInsert(NodeFactory.createURI(KB + Unit), NodeFactory.createURI(OM_SYMBOL), Unit)
				.addInsert(NodeFactory.createURI(KB + "Measure" + Instance),
						NodeFactory.createURI(OM_Has_NUMERICAL_VALUE), NumericalValue);
		return HG_instantiation_ub;
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
