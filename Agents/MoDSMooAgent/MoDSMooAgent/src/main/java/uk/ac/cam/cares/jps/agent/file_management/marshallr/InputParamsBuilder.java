package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.jayway.jsonpath.JsonPath;

public class InputParamsBuilder {
	private static Logger logger = LoggerFactory.getLogger(InputParamsBuilder.class);
	public static ArrayList<ArrayList<String>> attribs = new ArrayList<ArrayList<String>>();
	public static ArrayList<ArrayList<String>> profileAttribs = new ArrayList<ArrayList<String>>();
	public static ArrayList<String> valueList = new ArrayList<String>();
	public static ArrayList<ArrayList<String>> valueListWithIndex = new ArrayList<ArrayList<String>>();
	public static ArrayList<ArrayList<String>> valueListWithLookup = new ArrayList<ArrayList<String>>();
	public static ArrayList<ArrayList<String>> speciesList = new ArrayList<ArrayList<String>>();
	
	public static void main(String[] args) {
		InputParamsBuilder inputBuilder = new InputParamsBuilder();
		
		String iniTemp = "830";
		String iniTempUnit = "K";
		String iniPres = "15";
		String iniPresUnit = "bar";
		String mechFile = "mechanism.xml";
		int numOfReactions = 215;
		String oxidiser = "Test-Fuel-Ox";
		String ignDelayModel = "3";
		String ignDelayDeltaT = "400";
		String ignDelaySpeciesIndex = "CO";
		String ignDelayShowAll = "1";
		ArrayList<String> species = new ArrayList<String>(Arrays.asList("DMM3", "0.010309278350515464", "N2", "0.9278350515463918", "O2", "0.061855670103092786"));
		
		
		String jsonString = new JSONObject()
				.put("kinetics", 
						new JSONObject()
						.put("reactor", new JSONObject()
								.put("iniTemp", new JSONObject()
										.put("value", iniTemp)
										.put("unit", iniTempUnit))
								.put("iniPres", new JSONObject()
										.put("value", iniPres)
										.put("unit", iniPresUnit)))
						.put("chemistry", new JSONObject()
								.put("mechFile", mechFile)
								.put("numOfReactions", numOfReactions))
						.put("oxidiser", oxidiser)
						.put("ignDelayPostProcessor", new JSONObject()
								.put("ignDelayModel", ignDelayModel)
								.put("ignDelayDeltaT", ignDelayDeltaT)
								.put("ignDelaySpeciesIndex", ignDelaySpeciesIndex)
								.put("ignDelayShowAll", ignDelayShowAll))
						.put("mixtures", new JSONObject()
								.put("composition", species))).toString();
		
		File inputParams = inputBuilder.formInputParamsXML(jsonString);
	}
	
	public File formInputParamsXML (String jsonString) {
		DocumentBuilderFactory icFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder icBuilder;
		
		String filePath = JsonPath.read(jsonString, "$.kinetics.filePath");
		File output = new File(filePath);
		try {
			icBuilder = icFactory.newDocumentBuilder();
			Document doc = icBuilder.newDocument();
			doc.setXmlStandalone(true);
			Element mainRootElement = doc.createElementNS("http://como.cheng.cam.ac.uk/srm", "srm_inputs");
			mainRootElement.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
			mainRootElement.setAttribute("version", "v11.1.0");
			doc.appendChild(mainRootElement);
			
			mainRootElement.appendChild(getPropertyGroupGeneral(doc, "general"));
			mainRootElement.appendChild(getPropertyGroupReactor(doc, "Reactor", jsonString));
			mainRootElement.appendChild(getPropertyGroupSolver(doc, "Solver"));
			mainRootElement.appendChild(getPropertyGroupChemistry(doc, "Chemistry", jsonString));
			mainRootElement.appendChild(getPropertyGroupNumerical(doc, "numerical", jsonString));
			mainRootElement.appendChild(getPropertyGroupPressureEquilibration(doc, "PressureEquilibration"));
			mainRootElement.appendChild(getPropertyGroupFuel(doc, "fuel"));
			mainRootElement.appendChild(getPropertyGroupOxidiser(doc, "Oxidiser", jsonString));
			mainRootElement.appendChild(getPropertyGroupEGR(doc, "EGR"));
			mainRootElement.appendChild(getPropertyGroupMixing(doc, "mixing"));
			mainRootElement.appendChild(getPropertyGroupHeatTransfer(doc, "heat_transfer"));
			mainRootElement.appendChild(getPropertyGroupCreviceAndBlowby(doc, "crevice_and_blowby"));
			mainRootElement.appendChild(getPropertyGroupInflowOutflow(doc, "InflowOutflow"));
			mainRootElement.appendChild(getPropertyGroupDirectInjection(doc, "direct_injection"));
			mainRootElement.appendChild(getPropertyGroupSoot(doc, "soot"));
			mainRootElement.appendChild(getPropertyGroupSweepSootModel(doc, "Sweep_soot_model"));
			mainRootElement.appendChild(getPropertyGroupEmpiricalSootModel(doc, "Empirical_soot_model"));
			mainRootElement.appendChild(getPropertyGroupEmissions(doc, "Emissions"));
			mainRootElement.appendChild(getPropertyGroupIgnDelayPostProcessor(doc, "IgnDelay_PostProcessor", jsonString));
			mainRootElement.appendChild(getPropertyGroupSparkIgnition(doc, "spark_ignition"));
			mainRootElement.appendChild(getPropertyGroupBreathing(doc, "Breathing"));
			mainRootElement.appendChild(getPropertyGroupInjectionFuel(doc, "InjectionFuel"));
			mainRootElement.appendChild(getPropertyGroupFriction(doc, "Friction"));
			mainRootElement.appendChild(getPropertyGroupGUIProperties(doc, "GUI_Properties"));
			mainRootElement.appendChild(getPropertyGroupEngineConventions(doc, "engine_conventions"));
			mainRootElement.appendChild(getPropertyGroupApparentHRR(doc, "ApparentHRR"));
			mainRootElement.appendChild(getPropertyGroupEmpiricalPMPNModels(doc, "Empirical_PM_PN_models"));
			mainRootElement.appendChild(getPropertyGroupSectionalP4NModel(doc, "Sectional_P4N_model"));
			mainRootElement.appendChild(getPropertyGroupNanoModel(doc, "NanoModel"));
			mainRootElement.appendChild(getPropertyGroupDimensionMaps(doc, "DimensionMaps"));
			mainRootElement.appendChild(getPropertyGroupMoMICSootModel(doc, "MoMIC_soot_model"));
			mainRootElement.appendChild(getPropertyGroupMoMICZnOModel(doc, "MoMIC_ZnO_model"));
			mainRootElement.appendChild(getPropertyGroupProductsFlame(doc, "products_flame"));
			mainRootElement.appendChild(getPropertyGroupMoMICSiModel1(doc, "MoMIC_Si_model_1"));
			mainRootElement.appendChild(getPropertyGroupMoMICSiModel2(doc, "MoMIC_Si_model_2"));
			mainRootElement.appendChild(getPropertyGroupMoMICSiModel3(doc, "MoMIC_Si_model_3"));
			mainRootElement.appendChild(getPropertyGroupMoMICTTIPModel1(doc, "MoMIC_TTIP_model_1"));
			mainRootElement.appendChild(getPropertyGroupMoMICTTIPModel2(doc, "MoMIC_TTIP_model_2"));
			mainRootElement.appendChild(getPropertyGroupNanoModelPostProcess(doc, "NanoModelPostProcess"));
			mainRootElement.appendChild(getPropertyGroupMoMICModel(doc, "MoMIC_model"));
			mainRootElement.appendChild(getFlame(doc));
			mainRootElement.appendChild(getMixtures(doc, "composition", jsonString));
			mainRootElement.appendChild(getMixturesSimple(doc, "composition_site"));
			mainRootElement.appendChild(getMixturesSimple(doc, "composition_bulk"));
			
            // output DOM XML to console 
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");
            DOMSource source = new DOMSource(doc);
//            StreamResult console = new StreamResult(System.out);
            
            StreamResult outputFile = new StreamResult(output);
            transformer.transform(source, outputFile);
            
            logger.info("XML DOM Created Successfully..");
		} catch (Exception e) {
            e.printStackTrace();
        }
		return output;
	}
	
	private static Node getPropertyGroupGeneral(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "CaseNumber", "1"));
		propertyGroup.appendChild(getProperty(doc, "ReactorNumber", "1"));
		propertyGroup.appendChild(getProperty(doc, "CycleNumber", "1"));
		propertyGroup.appendChild(getProperty(doc, "NumberOfCycles", "1"));
		propertyGroup.appendChild(getProperty(doc, "EngineML_filename", "InputEngineML.xml"));
		propertyGroup.appendChild(getProperty(doc, "CopyEngineML", "1"));
		propertyGroup.appendChild(getProperty(doc, "EngineML_CaseNumber", "1"));
		propertyGroup.appendChild(getProperty(doc, "Adjustment_filename", "..\\..\\..\\InputAdjustments.xml"));
		propertyGroup.appendChild(getProperty(doc, "ParticleFileOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "AllowCycleToCycleVariability", "0"));
		propertyGroup.appendChild(getProperty(doc, "CopyInputs", "0"));
		propertyGroup.appendChild(getProperty(doc, "LogFileOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "DevLogOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "FinalTimeStepOutputOnly", "0"));
		propertyGroup.appendChild(getProperty(doc, "ParticleFileOutputSpecies", "2"));
		valueList = new ArrayList<String>(Arrays.asList("O2", "DMM3"));
		propertyGroup.appendChild(getProperty(doc, "ParticleFileOutputSpeciesList", valueList));
		propertyGroup.appendChild(getProperty(doc, "FinalEnsembleFileOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "EGRFileOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "MassFractionOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "MoleFractionOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "MolarConcentrationOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "SurfaceConcentrationOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "SurfaceFractionOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "SurfacePhaseOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "MassBalanceOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "HeatBalanceOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "EnergyBalanceOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "MomentsOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "MechanismOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "FluxOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "UserDefinedRealFormat", "es14.5e3"));
		propertyGroup.appendChild(getProperty(doc, "ZoneFileName", "..\\..\\..\\InputZones.xml"));
		propertyGroup.appendChild(getProperty(doc, "TorqueOutput", "1"));
		propertyGroup.appendChild(getProperty(doc, "MassFlowsOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "MomentsStateOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "MomentsRateOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "MomentsPostProcessOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "HCSpeciesOutput", "0"));
		propertyGroup.appendChild(getProperty(doc, "ADMSOutput", "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupReactor(Document doc, String ref, String jsonString) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		String iniTemp = JsonPath.read(jsonString, "$.kinetics.reactor.iniTemp.value");
		String iniTempUnit = JsonPath.read(jsonString, "$.kinetics.reactor.iniTemp.unit");
		String iniPres = JsonPath.read(jsonString, "$.kinetics.reactor.iniPres.value");
		String iniPresUnit = JsonPath.read(jsonString, "$.kinetics.reactor.iniPres.unit");
		
		propertyGroup.appendChild(getProperty(doc, "ReactorModel", "0"));
		propertyGroup.appendChild(getProperty(doc, "ConstFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ReactorVolume", "dimension", "volume", "unit", "m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CrossSection", "allowprofile", "never", "dimension", "area", "readfrom", "value", "unit", "m^2")));
		profileAttribs = generateAttribs(new ArrayList<String>(Arrays.asList("delim", ",", "delimlen", "1", "dimension_lookup", "independent_variable", "headers", "1", 
				"indvarcol", "1", "mergedelims", "0", "periodicity", "0", "trim_leadb", "1", "trim_trailb", "1", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01", profileAttribs, ""));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MassFlowRate", "dimension", "mass_flow_rate", "unit", "kg/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IniTemp", "allowprofile", "optional", "dimension", "temperature", "readfrom", "value", "unit", iniTempUnit)));
		profileAttribs = generateAttribs(new ArrayList<String>(Arrays.asList("delim", ",", "delimlen", "1", "dimension_lookup", "independent_variable", "headers", "1", 
				"indvarcol", "1", "mergedelims", "0", "periodicity", "0", "trim_leadb", "1", "trim_trailb", "1", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, iniTemp, profileAttribs, ""));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IniPres", "dimension", "pressure", "unit", iniPresUnit)));
		propertyGroup.appendChild(getProperty(doc, attribs, iniPres));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "WallTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "420.0"));
		
		propertyGroup.appendChild(getProperty(doc, "IsothermalFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "EqConstFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ReactorIniFuelAirEquivRatio", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		propertyGroup.appendChild(getProperty(doc, "PostProcessorFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("allowprofile", "always", "dimension", "gas fraction", "readfrom", "profile", "type", "composition", "unit", "mass fraction")));
		profileAttribs = generateAttribs(new ArrayList<String>(Arrays.asList("delim", ",", "delimlen", "1", "dimension_lookup", "independent_variable", "headers", "1", 
				"indvarcol", "1", "mergedelims", "0", "periodicity", "0", "trim_leadb", "1", "trim_trailb", "1", "unit", "ms")));
		propertyGroup.appendChild(getMixtureProfile(doc, attribs, "", profileAttribs, ""));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupSolver(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "ODESolver", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RTol", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e-3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ATol", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e-11"));
		
		propertyGroup.appendChild(getProperty(doc, "IVPFlag", "1"));
		propertyGroup.appendChild(getProperty(doc, "SolverPeriodFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SolverPeriodStart", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SolverPeriodEnd", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "20"));
		
		propertyGroup.appendChild(getProperty(doc, "SensFlagOnOff", "0"));
		propertyGroup.appendChild(getProperty(doc, "SensFlagStepSizeEst", "1"));
		propertyGroup.appendChild(getProperty(doc, "SensFlagParameters", "1"));
		propertyGroup.appendChild(getProperty(doc, "SensNoofVariables", "5"));
		
		valueList = new ArrayList<String>(Arrays.asList("40", "1", "2", "32", "33"));
		propertyGroup.appendChild(getProperty(doc, "SensListofVariables", valueList));
		
		propertyGroup.appendChild(getProperty(doc, "SkipNonHC", "0"));
		propertyGroup.appendChild(getProperty(doc, "SkipHV", "0"));
		
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupChemistry(Document doc, String ref, String jsonString) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		String mechFile = JsonPath.read(jsonString, "$.kinetics.chemistry.mechFile");
		String chemModel = null;
		if (mechFile.endsWith(".xml")) {
			chemModel = "4";
		} else if (mechFile.endsWith(".owl")) {
			chemModel = "5";
		}
		int numOfReactions = JsonPath.read(jsonString, "$.kinetics.chemistry.numOfReactions");
		
		propertyGroup.appendChild(getProperty(doc, "ChemModel", chemModel));
		propertyGroup.appendChild(getProperty(doc, "StrictErrorChecking", "0"));
		propertyGroup.appendChild(getProperty(doc, "MechFile", mechFile));
		propertyGroup.appendChild(getProperty(doc, "ThermFile", ""));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ReactionRate_A_Modifiers", "dimension", "dimensionless", "unit", "-")));
		valueList = new ArrayList<String>();
		for (int i = 0; i < numOfReactions; i += 1) {
			valueList.add("1.0");
		}
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, generateValueWithIndex(valueList)));
		
		propertyGroup.appendChild(getProperty(doc, "SurfaceFile", ""));
		propertyGroup.appendChild(getProperty(doc, "TransportFile", ""));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupNumerical(Document doc, String ref, String jsonString) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		String simEnd = JsonPath.read(jsonString, "$.kinetics.numerical.simEnd");
		
		valueList = new ArrayList<String>(Arrays.asList("1", "2"));
		propertyGroup.appendChild(getProperty(doc, "RndSeeds", valueList));
		
		propertyGroup.appendChild(getProperty(doc, "UseIVCEVO", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SimStart", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SimEnd", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, simEnd));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SimStep", "dimension_lookup", "independent_variable_duration", "unit", "us")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1"));
		
		propertyGroup.appendChild(getProperty(doc, "No_Stochastic_Particles", "1"));
		propertyGroup.appendChild(getProperty(doc, "Initial_Particle_Ensemble", "0"));
		propertyGroup.appendChild(getProperty(doc, "Initial_Particle_Ensemble_filename", "..\\..\\..\\Input_Par_Ensemble.csv"));
		

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "OutputStep", "dimension_lookup", "independent_variable", "unit", "ms")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "2.0", "5.0", "10.0", "15.0", "20.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		propertyGroup.appendChild(getProperty(doc, "ZoneFlag", "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupPressureEquilibration(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "tauPEq", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.00001"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupFuel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "FuelFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "FuelMass", "dimension", "mass", "unit", "mg")));
		propertyGroup.appendChild(getProperty(doc, attribs, "120"));
		
		propertyGroup.appendChild(getMixturename(doc, "composition", "Fuel"));
		propertyGroup.appendChild(getProperty(doc, "IntakeCoolingFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "VapHeatIntake", "dimension", "energy_density_by_mass", "unit", "MJ/kg")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TLiqIntake", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupOxidiser(Document doc, String ref, String jsonString) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		String oxidiser = JsonPath.read(jsonString, "$.kinetics.oxidiser");
		propertyGroup.appendChild(getMixturename(doc, "composition", oxidiser));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupEGR(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "EGRFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EGR_fraction", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.1"));
		
		propertyGroup.appendChild(getProperty(doc, "EGRUpdate", "0"));
		propertyGroup.appendChild(getProperty(doc, "NVOFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "EGRPreProFlag", "2"));
		propertyGroup.appendChild(getProperty(doc, "EGRCompositionSource", "1"));
		propertyGroup.appendChild(getProperty(doc, "RBFFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "EGRNanoModelFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "EGRNanoModel_IO_Flag", "1"));
		propertyGroup.appendChild(getProperty(doc, "EGRDirectory", "./"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMixing(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "TurbMixModel", "0"));
		propertyGroup.appendChild(getProperty(doc, "TurbMixMode", "0"));
		propertyGroup.appendChild(getProperty(doc, "TurbProfile", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbCADs", "dimension", "crank_angle", "unit", "CAD aTDC")));
		propertyGroup.appendChild(getProperty(doc, attribs));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbtauUs", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "tauU", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e-2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CphiU", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Ck0", "dimension", "Turbulence kinetic energy", "unit", "m^2/s^2")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Tumble", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Swirl", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-SquishVolFr", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Csq", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Cden", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Cinj", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Cprod", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "TurbProfileFilename", "..\\..\\..\\turb-base.csv"));
		propertyGroup.appendChild(getProperty(doc, "MixPeriodFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MixPeriodStart", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MixPeriodEnd", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "20"));
		
		propertyGroup.appendChild(getProperty(doc, "DetailedMixModeCap", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-maxTauU", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.006"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TurbQuasi-Cdiss", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "20"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupHeatTransfer(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Heat_transfer_model", "0"));
		
//		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "tauU", "dimension", "time", "unit", "s")));
//		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Heat_transfer_Coefficent", "dimension", "HeatTransfer", "unit", "W/m^2/K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "50.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Heat_transfer_Surface_Area", "dimension", "area", "unit", "m^2")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Heat_transfer_multiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Piston_surface_area_ratio", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Head_surface_area_ratio", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "StoHTConst", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2000.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MassFactorHT", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.05"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "PistonWallTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "420.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "HeadWallTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "450.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "LinerWallTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "420.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Heat_transfer_correl", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "WoschniC1", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.28"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "WoschniC1_Breath", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "6.18"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "WoschniHRC2", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.02"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "HohenbergC1", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "130"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "HohenbergC2", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.4"));
		
		propertyGroup.appendChild(getProperty(doc, "WallTemperatureModel", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "HeadCoolantTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "365"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "LinerCoolantTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "365"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "PistonCoolantTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "365"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjectorCoolantTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "395"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RHeadCoolant", "dimension", "Thermal insulance", "unit", "K mm^2/W")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RLinerCoolant", "dimension", "Thermal insulance", "unit", "K mm^2/W")));
		propertyGroup.appendChild(getProperty(doc, attribs, "45"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RPistonCoolant", "dimension", "Thermal insulance", "unit", "K mm^2/W")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RInjectorCoolant", "dimension", "Thermal insulance", "unit", "K mm^2/W")));
		propertyGroup.appendChild(getProperty(doc, attribs, "30"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hHeadWall", "dimension", "length", "unit", "mm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hLinerWall", "dimension", "length", "unit", "mm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hPistonWall", "dimension", "length", "unit", "mm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "10"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hInjectorWall", "dimension", "length", "unit", "mm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "12"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hDHead", "dimension", "length", "unit", "um")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hDLiner", "dimension", "length", "unit", "um")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hDPiston", "dimension", "length", "unit", "um")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hDInjector", "dimension", "length", "unit", "um")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "AreaInjector", "dimension", "area", "unit", "cm^2")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kappaHeadWall", "dimension", "Thermal conductivity", "unit", "W/K/m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "54"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kappaLinerWall", "dimension", "Thermal conductivity", "unit", "W/K/m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "54"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kappaPistonWall", "dimension", "Thermal conductivity", "unit", "W/K/m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "155"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kappaInjectorWall", "dimension", "Thermal conductivity", "unit", "W/K/m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "54"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kappaDeposit", "dimension", "Thermal conductivity", "unit", "W/K/m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "chiInjectorWall", "dimension", "Diffusivity", "unit", "mm^2/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "15.3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "chiDeposit", "dimension", "Diffusivity", "unit", "mm^2/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "11.6"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupCreviceAndBlowby(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Crevice_ratio", "dimension", "percentage", "unit", "%")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CrTimeFactor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.03"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CrTimeFactor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.05"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "BlowbyGap", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-1.0e-4"));
		
		propertyGroup.appendChild(getProperty(doc, "CreviceModel", "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupInflowOutflow(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "InflowOutflowModel", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "mdotInlet", "dimension", "mass_flow_rate", "unit", "kg/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TempInlet", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1100.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.001"));
		
		propertyGroup.appendChild(getMixturename(doc, "composition", ""));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupDirectInjection(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "InjModel", "0"));
		propertyGroup.appendChild(getProperty(doc, "InjProfile", "0"));
		propertyGroup.appendChild(getProperty(doc, "UseSOIAdjforEOI", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SOI", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "5.0d0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EOI", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "15e0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjFuelMass", "dimension", "mass", "unit", "mg")));
		propertyGroup.appendChild(getProperty(doc, attribs, "11.6e0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjCADs", "dimension_lookup", "independent_variable", "unit", "ms")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "2.0", "5.0", "10.0", "15.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjRateMassCADs", "dimension_lookup", "mass_flow_rate", "unit", "mg/ms")));
		valueList = new ArrayList<String>(Arrays.asList("0.5", "0.1", "0.5", "0.5", "0.8"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "LambdaEvap", "dimension", "Diffusivity", "unit", "mm^2/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RhoLiq", "dimension", "density", "unit", "kg/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "690.64"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Liq_temp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "350.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjVeloc", "dimension", "speed", "unit", "m/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "30.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Full_Cone_Angle", "dimension", "plane_angle", "unit", "deg")));
		propertyGroup.appendChild(getProperty(doc, attribs, "40.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "VapHeat", "dimension", "energy_density_by_mass", "unit", "MJ/kg")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		propertyGroup.appendChild(getProperty(doc, "InjAlphaProfile", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjAlpha", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "5.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjAlphaCADs", "dimension_lookup", "independent_variable", "unit", "ms")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "2.0", "5.0", "10.0", "15.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjAlphas", "dimension_lookup", "dimensionless", "unit", "-")));
		valueList = new ArrayList<String>(Arrays.asList("50.0", "40.0", "35.0", "20.0", "10.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "tauInj", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.001"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CphiInj", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.0"));
		
		propertyGroup.appendChild(getProperty(doc, "SMD_model_flag", "0"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Nozzle_Diameter", "dimension", "length", "unit", "mm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.16"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjPress", "dimension", "pressure", "unit", "bar")));
		propertyGroup.appendChild(getProperty(doc, attribs, "650"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SurfaceTension", "dimension", "Surface_Tension", "unit", "mN/m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "25.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "FuelViscosity", "dimension", "Kinematic viscosity", "unit", "mm^2/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "3.36"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SMD0", "dimension", "length", "unit", "um")));
		propertyGroup.appendChild(getProperty(doc, attribs, "50.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SMD_param_A", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "6156"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SMD_param_nu", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.385"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SMD_param_sigma", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.737"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SMD_param_rhofuel", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.737"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SMD_param_rhoair", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.06"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SMD_param_deltaP", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.54"));
		
		propertyGroup.appendChild(getProperty(doc, "InjFuel", "1"));
		propertyGroup.appendChild(getProperty(doc, "InjProfileFilename", "..\\..\\..\\InjectionProfile.csv"));
		propertyGroup.appendChild(getProperty(doc, "InjAlphaProfileFilename", "..\\..\\..\\InjectionAlphaProfile.csv"));
		propertyGroup.appendChild(getProperty(doc, "InjAlphaWallProfileFilename", "..\\..\\..\\InjectionAlphaWallProfile.csv"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjWallAlpha", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "10000.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjWallFraction", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.05"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjWallCentre", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.8"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjWallEvapRate", "dimension", "mass_flow_rate", "unit", "g/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "20e-3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjWallAlphaCADs", "dimension_lookup", "independent_variable", "unit", "ms")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "2.0", "5.0", "10.0", "15.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjWallAlphas", "dimension_lookup", "dimensionless", "unit", "-")));
		valueList = new ArrayList<String>(Arrays.asList("20.0", "20.0", "10.0", "4.0", "2.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		propertyGroup.appendChild(getProperty(doc, "Nozzle_No", "6"));
		propertyGroup.appendChild(getProperty(doc, "nozzle_model_flag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "grad_CphiInj", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "grad_InjAlpha", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "grad_InjVeloc", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "grad_Full_Cone_Angle", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Nozzle_No_Base", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.5"));
		
		propertyGroup.appendChild(getProperty(doc, "InjSysType", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Full_Cone_Angle_Flag", "dimension", "plane_angle", "unit", "deg")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjEOIDelay", "dimension_lookup", "independent_variable_duration", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "3.0"));
		
		propertyGroup.appendChild(getProperty(doc, "InjPressProfile", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjPressEqnConst", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.066"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjPressCADs", "dimension_lookup", "independent_variable", "unit", "ms")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "2.0", "5.0", "10.0", "15.0", "20.0", "25.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InjPressures", "dimension_lookup", "pressure", "unit", "bar")));
		valueList = new ArrayList<String>(Arrays.asList("650", "650", "650", "650", "650", "650", "650"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		propertyGroup.appendChild(getProperty(doc, "CphiInterpFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "DepositModel", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DepRho", "dimension", "density", "unit", "kg/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "800"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "KHNOD", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "KHNO2D", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ki0D", "dimension", "Pre-exponential factor (2nd order)", "unit", "m^3/mol/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "5.9e6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EiD", "dimension", "energy_density_by_moles", "unit", "kJ/mol")));
		propertyGroup.appendChild(getProperty(doc, attribs, "73.6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ki0D2", "dimension", "Pre-exponential factor (2nd order)", "unit", "m^3/mol/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "12.e6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EiD2", "dimension", "dimensionless", "unit", "kJ/mol")));
		propertyGroup.appendChild(getProperty(doc, attribs, "86.6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ka0D", "dimension", "Pre-exponential factor (2nd order)", "unit", "m^3/mol/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.58e6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EaD", "dimension", "energy_density_by_moles", "unit", "kJ/mol")));
		propertyGroup.appendChild(getProperty(doc, attribs, "66.7"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ka0D2", "dimension", "Pre-exponential factor (2nd order)", "unit", "m^3/mol/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EaD2", "dimension", "energy_density_by_moles", "unit", "kJ/mol")));
		propertyGroup.appendChild(getProperty(doc, attribs, "71.35"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kaOH0D", "dimension", "Pre-exponential factor (2nd order)", "unit", "m^3/mol/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.51e6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EaOHD", "dimension", "energy_density_by_moles", "unit", "kJ/mol")));
		propertyGroup.appendChild(getProperty(doc, attribs, "22.8"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kaOH0D2", "dimension", "Pre-exponential factor (2nd order)", "unit", "m^3/mol/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.58e6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EaOHD2", "dimension", "energy_density_by_moles", "unit", "kJ/mol")));
		propertyGroup.appendChild(getProperty(doc, attribs, "25.9"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "kNO0D", "dimension", "Pre-exponential factor (2nd order)", "unit", "m^3/mol/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "4.8e6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ENOD", "dimension", "energy_density_by_moles", "unit", "kJ/mol")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Depssites", "dimension", "number surface density", "unit", "1/m^2")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.176e19"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Depkr", "dimension", "frequency", "unit", "1/h")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "hFilm", "dimension", "length", "unit", "um")));
		propertyGroup.appendChild(getProperty(doc, attribs, "100"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Tboiling0", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "373.15"));
		
		propertyGroup.appendChild(getProperty(doc, "FixQuenchTemp", "1"));
		propertyGroup.appendChild(getProperty(doc, "NonEquiwgtMethod", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "NonEquiwgtFactor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "15.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MixC1", "dimension_lookup", "independent_variable", "unit", "CAD aTDC")));
		propertyGroup.appendChild(getProperty(doc, attribs, "3.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MixC2", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MixC3", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0012"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupSoot(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "PopBalModel", "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupSweepSootModel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "SweepModel", "2"));
		propertyGroup.appendChild(getProperty(doc, "SweepMechanismFilename", "..\\..\\..\\InputSweepMechanism.xml"));
		
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MaxLocalEquivRatio", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "8.0"));
		
		propertyGroup.appendChild(getProperty(doc, "SweepTreeLevelCount", "9"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "LPDASplitRatio", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e9"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MaxNumSootPar", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e11"));
		
		propertyGroup.appendChild(getProperty(doc, "OutputDistrFlag", "1"));
		propertyGroup.appendChild(getProperty(doc, "NumDistrPts", "499"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "LBSizeDistr", "dimension", "length", "unit", "nm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "UBSizeDistr", "dimension", "length", "unit", "nm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "300"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SizeDistrSigma", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.07"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TEMStart", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "100.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TEMEnd", "dimension_lookup", "independent_variable", "unit", "ms")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-100.0"));
		
		propertyGroup.appendChild(getProperty(doc, "SkipPBSforRichParticles", "1"));
		propertyGroup.appendChild(getProperty(doc, "OutputPrimDistrFlag", "1"));
		propertyGroup.appendChild(getProperty(doc, "NumPrimDistrPts", "499"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "LBPrimSizeDistr", "dimension", "length", "unit", "nm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "UBPrimSizeDistr", "dimension", "length", "unit", "nm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "300"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SizePrimDistrSigma", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.07"));
		
		propertyGroup.appendChild(getProperty(doc, "ParticleModel", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DoublingThreshold", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.75"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DoublingLimit", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.46875"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupEmpiricalSootModel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "EmpSootModel", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Pre-exponential_Multiplier_SF", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Exponential_Multiplier_SF", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Pre-exponential_Multiplier_SO", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Exponential_Multiplier_SO", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "EmpSootPrecursor", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EmpSootDensity", "dimension", "density", "unit", "g/cm^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EmpSootMeanDiameter", "dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "4.5E-7"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupEmissions(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "NOx_Multiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CO_Multiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "uHCs_Multiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Soot_Multiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Fuel_Multiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "uHCs_Shift", "dimension", "mass_flow_rate", "unit", "g/h")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "NOx_Shift", "dimension", "mass_flow_rate", "unit", "g/h")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CO_Shift", "dimension", "mass_flow_rate", "unit", "g/h")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Soot_Shift", "dimension", "mass_flow_rate", "unit", "g/h")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupIgnDelayPostProcessor(Document doc, String ref, String jsonString) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		String ignDelayModel = JsonPath.read(jsonString, "$.kinetics.ignDelayPostProcessor.ignDelayModel");
		String ignDelayDeltaT = JsonPath.read(jsonString, "$.kinetics.ignDelayPostProcessor.ignDelayDeltaT");
		String ignDelaySpeciesIndex = JsonPath.read(jsonString, "$.kinetics.ignDelayPostProcessor.ignDelaySpeciesIndex");
		String ignDelayShowAll = JsonPath.read(jsonString, "$.kinetics.ignDelayPostProcessor.ignDelayShowAll");
		
		
		
		propertyGroup.appendChild(getProperty(doc, "IgnDelay_Model", ignDelayModel));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IgnDelay_DeltaT", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, ignDelayDeltaT));
		
		propertyGroup.appendChild(getProperty(doc, "IgnDelay_SpeciesIndex", ignDelaySpeciesIndex));
		
		propertyGroup.appendChild(getProperty(doc, "IgnDelay_ShowAll", ignDelayShowAll));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupSparkIgnition(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "SIMode", "0"));
		propertyGroup.appendChild(getProperty(doc, "KnockMisfFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Entr_Rate_Const", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "18.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "PeakPresSigma", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-3.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Spark_energy", "dimension", "energy", "unit", "J")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IniFlameRad", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0005"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SparkTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2000.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "HRRDrop", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.02"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EUtoEBTemp", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1500.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MinEBFrac", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "AnchorAngleWiebe", "dimension", "crank_angle", "unit", "CAD aTDC")));
		propertyGroup.appendChild(getProperty(doc, attribs, "5.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "AnchorMFBWiebe", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "StartCombMFBWiebe", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EndCombMFBWiebe", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.9"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MaxEntTime", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0025"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Bm", "dimension", "speed", "unit", "m/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.305"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Bphi", "dimension", "speed", "unit", "m/s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.549"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "phim", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.21"));
		
		propertyGroup.appendChild(getProperty(doc, "LamFlaSpeCor", "2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Alpha1", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.4"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Alpha2", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.271"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Alpha3", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "3.51"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Beta1", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.357"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Beta2", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.14"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Beta3", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.77"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Param_SL1", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-0.357"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Param_SL2", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.14"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Param_SL3", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.77"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Param_SL4", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.77"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EGR_Param_A", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.06"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EGR_Param_B", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.77"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SparkToRoofDis", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InletValveArea", "dimension", "area", "unit", "m^2")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.4e-3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "BowlRadius", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.55e-2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "BowlDepth", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.5e-3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RoofAngle", "dimension", "plane_angle", "unit", "deg")));
		propertyGroup.appendChild(getProperty(doc, attribs, "155.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "InletValveLift", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "7.5e-3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "tauB", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "tauE", "dimension", "time", "unit", "s")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.00075"));
		
		propertyGroup.appendChild(getProperty(doc, "EntrMixingModel", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EntMixConst", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "MixFact", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.1"));
		
		propertyGroup.appendChild(getProperty(doc, "InputMFBFileName", "..\\..\\..\\InputMFB.csv"));
		propertyGroup.appendChild(getProperty(doc, "SILookUpFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "BurnOrderFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "BurnOrderFileName", "..\\..\\..\\InputBurnOrder.csv"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CADSpark", "dimension", "crank_angle", "unit", "CAD aTDC")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-40.0"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "StdWiebe_SoC", "dimension", "crank_angle", "unit", "CAD aTDC")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-40.0"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DoubleWiebe_SoC", "dimension", "crank_angle", "unit", "CAD aTDC")));
		valueList = new ArrayList<String>(Arrays.asList("-40.0", "-40.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DoubleWiebeWeightFraction", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "StdWiebe_CombDur", "dimension", "crank_angle_duration", "unit", "CAD")));
		propertyGroup.appendChild(getProperty(doc, attribs, "50.0"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ModWiebe_CombDur", "dimension", "crank_angle_duration", "unit", "CAD")));
		propertyGroup.appendChild(getProperty(doc, attribs, "50.0"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DoubleWiebe_CombDur", "dimension", "crank_angle_duration", "unit", "CAD")));
		valueList = new ArrayList<String>(Arrays.asList("50.0", "50.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "StdWiebe_m", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ModWiebe_m", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DoubleWiebe_m", "dimension", "dimensionless", "unit", "-")));
		valueList = new ArrayList<String>(Arrays.asList("2.0", "2.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "StdWiebe_a", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "5.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DoubleWiebe_a", "dimension", "dimensionless", "unit", "-")));
		valueList = new ArrayList<String>(Arrays.asList("5.0", "5.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		propertyGroup.appendChild(getProperty(doc, "LamFlamePhiGlobalFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IncreaseDuration", "dimension", "crank_angle_duration", "unit", "CAD")));
		propertyGroup.appendChild(getProperty(doc, attribs, "10"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "DecreaseDuration", "dimension", "crank_angle_duration", "unit", "CAD")));
		propertyGroup.appendChild(getProperty(doc, attribs, "20"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Mu1", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.438"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Mu2", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.4111"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Mu3", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2006"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Param_SL0", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "35.615"));
		
		propertyGroup.appendChild(getProperty(doc, "FlamePropfFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SI_Char_Time", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.06"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Ini_Flame_Scale", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0001"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Flame_Scale_Const", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "550.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SI_Wall_Extinct2_Const", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "4.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SI_Wall_Extinct_Const", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "5.0"));
		
		propertyGroup.appendChild(getProperty(doc, "KnockSpecies", "OH"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "KnockSpeciesMassFrac", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1E-4"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupBreathing(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "BreathingModel", "0"));
		propertyGroup.appendChild(getProperty(doc, "NumIntValves", "2"));
		
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IntValveRefDiam", "dimension", "length", "unit", "mm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "30.0"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IntCADs", "dimension", "crank_angle", "unit", "CAD aTDC")));
		valueList = new ArrayList<String>(Arrays.asList("-355.0", "-318.0", "-261.0", "-204.0", "-167.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IntValveLiftsCAD", "dimension", "length", "unit", "mm")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "10.0", "12.0", "10.0", "0.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IntValveLiftsCD", "dimension", "length", "unit", "mm")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "5.0", "10.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IntValveCDs", "dimension", "dimensionless", "unit", "-")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "0.8", "1.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		propertyGroup.appendChild(getProperty(doc, "NumExhValves", "2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ExhValveRefDiam", "dimension", "length", "unit", "mm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "27.0"));

		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ExhCADs", "dimension", "crank_angle", "unit", "CAD aTDC")));
		valueList = new ArrayList<String>(Arrays.asList("141.0", "197.0", "253.0", "309.0", "365.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ExhValveLiftsCAD", "dimension", "length", "unit", "mm")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "10.0", "12.0", "10.0", "0.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ExhValveLiftsCD", "dimension", "length", "unit", "mm")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "5.0", "10.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ExhValveCDs", "dimension", "dimensionless", "unit", "-")));
		valueList = new ArrayList<String>(Arrays.asList("0.0", "0.8", "1.0"));
		propertyGroup.appendChild(getProperty(doc, attribs, valueList));
		
		propertyGroup.appendChild(getProperty(doc, "IntValveProfile", "0"));
		propertyGroup.appendChild(getProperty(doc, "IntValveProfileFilename", "..\\..\\..\\IntakeValveProfile.csv"));
		propertyGroup.appendChild(getProperty(doc, "ExhValveProfile", "0"));
		propertyGroup.appendChild(getProperty(doc, "ExhValveProfileFilename", "..\\..\\..\\ExhaustValveProfile.csv"));
		propertyGroup.appendChild(getProperty(doc, "MultiCylTorqueFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "CylinderCADOffsets", "dimension", "crank_angle", "unit", "CAD aTDC")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TorqueTimestep", "dimension", "crank_angle_duration", "unit", "CAD")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		propertyGroup.appendChild(getProperty(doc, "ExtrapMethod", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "IntMultiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ExhMultiplier", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1"));
		
		return propertyGroup;
	}
	
	
	public static Node getPropertyGroupInjectionFuel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getMixturename(doc, "composition", "Injection Fuel"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupFriction(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Acf", "dimension", "pressure", "unit", "bar")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Bcf", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.006"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Ccf", "dimension", "friction Ccf", "unit", "Pa min/m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "600"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Qcf", "dimension", "friction Qcf", "unit", "Pa min^2/m^2")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupGUIProperties(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Case_Alias", "PODE3_Figure-5_Ignition_Criterion"));
		propertyGroup.appendChild(getProperty(doc, "Case_Description", "Set IVPFLAG (line 180) to 1 in InputParams file."));
		propertyGroup.appendChild(getProperty(doc, "Mechanism_Name", "Wenming_PODE"));
		propertyGroup.appendChild(getProperty(doc, "Piston_Bowl_Name", "Unknown"));
		propertyGroup.appendChild(getProperty(doc, "UserImportedSootMechanism", "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupEngineConventions(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "TDC_convention_flag", "unit", "integer")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Stroke_convention_flag", "unit", "integer")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupApparentHRR(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "AppHRR_gammaFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "AppHRR_gammaConst", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.37"));
		
		propertyGroup.appendChild(getProperty(doc, "AppHRR_smoothFlag", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "AppHRR_dampFac", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.765928338"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupEmpiricalPMPNModels(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "PM_grad", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.4"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "EmpPM_SOF_SOx", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0"));
		
		propertyGroup.appendChild(getProperty(doc, "EmpPNModel", "0"));
		propertyGroup.appendChild(getProperty(doc, "soot_PN_correl", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ArndtsootA", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.965"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ArndtsootB", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "12.232"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Arndt_PM_A", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.985"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Arndt_PM_B", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "12.572"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Gallus_grad", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.1E12"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Gallus_const", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "4.06E11"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupSectionalP4NModel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_sections", "35"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Spacing_Factor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Max_Size", "dimension", "length", "unit", "nm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2500"));
		
		propertyGroup.appendChild(getProperty(doc, "Use_Max_Size", "1"));
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "1"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "0"));
		propertyGroup.appendChild(getProperty(doc, "Use_2D_Model", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Fractal_Dimension", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.8"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Volume_Filling_Factor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.43"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Fractal_Prefactor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.37"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Coagulation_Efficiency", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Surface_Growth_Factor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "OH_Efficiency", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.13"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Inception_Efficiency", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0001"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Fragmentation_Constant", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Condensation_Efficiency", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "PAH_species_sect", "PYRENE"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "PAH_diameter", "dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.787e-7"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CO", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0"));
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.2"));
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1", "1", "1", "1", "1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		propertyGroup.appendChild(getProperty(doc, "Use_Ash_Model", "0"));
		propertyGroup.appendChild(getProperty(doc, "Use_SO4_Model", "0"));
		propertyGroup.appendChild(getProperty(doc, "Use_SOF_Model", "0"));
		propertyGroup.appendChild(getProperty(doc, "Use_xSOF_Model", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "O2_Regen_K_Factor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1d+20"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "O2_Regen_Ea", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "125000.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "O2_Regen_B", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "O2_Regen_Alpha", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "NO2_Regen_K_Factor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1d+20"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "NO2_Regen_Ea", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "95000.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "NO2_Regen_B", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "NO2_Regen_Alpha", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "H2SO4_Accomodation_Factor", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.05"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Ash_Nucleation_Rate", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0001"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Ash_Condensation_Rate", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "SOF_species", "C8H18"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SOF_Boil_T", "dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, attribs, "400"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SOF_diameter", "dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.787e-7"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SOF_Inception_Eff", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.0001"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "SOF_Condensation_Eff", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", 
				"0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0", "0.0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_O2_Regen", "0"));
		propertyGroup.appendChild(getProperty(doc, "Allow_NO2_Regen", "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupNanoModel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "NanoModelFlag", "0"));
		propertyGroup.appendChild(getProperty(doc, "NanoModelFlag_Engines", "0"));
		propertyGroup.appendChild(getProperty(doc, "NanoModelFlag_Aftertreatment", "0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupDimensionMaps(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		valueListWithLookup = generateValueWithLookup(new ArrayList<String>(Arrays.asList("independent_variable", "time", "independent_variable_duration", "time", "mass_flow_rate", "mass_flow_rate")));
		propertyGroup.appendChild(getMap(doc, "default", valueListWithLookup));
		
		valueListWithLookup = generateValueWithLookup(new ArrayList<String>(Arrays.asList("independent_variable", "length", "independent_variable_duration", "length", "mass_flow_rate", "mass_flow_rate")));
		propertyGroup.appendChild(getMap(doc, "1", valueListWithLookup));
		
		valueListWithLookup = generateValueWithLookup(new ArrayList<String>(Arrays.asList("independent_variable", "crank_angle", "independent_variable_duration", "crank_angle_duration", "mass_flow_rate", "mass_per_plane_angle")));
		propertyGroup.appendChild(getMap(doc, "2", valueListWithLookup));
		
		valueListWithLookup = generateValueWithLookup(new ArrayList<String>(Arrays.asList("independent_variable", "length", "independent_variable_duration", "length", "mass_flow_rate", "mass_flow_rate")));
		propertyGroup.appendChild(getMap(doc, "4", valueListWithLookup));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICSootModel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_moments", "5"));
		propertyGroup.appendChild(getProperty(doc, "PAH_species", "PYRENE"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "PAH_diameter", "dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.97e-7"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "2"));
		propertyGroup.appendChild(getProperty(doc, "Closure_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0", "0", "0", "0", "0", "0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "1"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_CR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Type_FR")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICZnOModel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_moments", "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.64"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0", "0", "0", "0", "0", "0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "0"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "1"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_CR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupProductsFlame(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getMixturename(doc, "composition", ""));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICSiModel1(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_moments", "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "1"));
		propertyGroup.appendChild(getProperty(doc, "Use_dCutoff", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "erf_dx", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0", "0", "0", "0", "0", "0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1", "1", "1", "1", "1", "1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1", "1", "1", "1", "1", "1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_CR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICSiModel2(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_moments", "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "1"));
		propertyGroup.appendChild(getProperty(doc, "Use_dCutoff", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "erf_dx", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0", "0", "0", "0", "0", "0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1", "1", "1", "1", "1", "1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_CR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICSiModel3(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_moments", "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "1"));
		propertyGroup.appendChild(getProperty(doc, "Use_dCutoff", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "erf_dx", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.01"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.2"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0", "0", "0", "0", "0", "0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_CR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICTTIPModel1(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_moments", "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.64"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0", "0", "0", "0", "0", "0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_CR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "gamma_in", "default", "true", "desc", "Inception rate multiplier", "dimension", "dimensionless", "hidden", "true", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "gamma_sg", "default", "true", "desc", "Surface reaction rate multiplier", "dimension", "dimensionless", "hidden", "true", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICTTIPModel2(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Num_moments", "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_IN", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_SG", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FM", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CR", "1"));
		propertyGroup.appendChild(getProperty(doc, "Include_Heat_Release", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "epsilon_ij", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "2.64"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "alpha_T", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		propertyGroup.appendChild(getProperty(doc, "Scaling_Method", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Initial_State", "dimension", "number_concentration", "unit", "#/m^3")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("0", "0", "0", "0", "0", "0")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Method", "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_ATOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Cosolve_RTOL", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0d-3"));
		
		propertyGroup.appendChild(getProperty(doc, "Realise_Method", "0"));
		
		propertyGroup.appendChild(getProperty(doc, "Allow_OX", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_FR", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_IN", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_SG", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_FM", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Multiplier_CR", "dimension", "dimensionless", "unit", "-")));
		valueListWithIndex = generateValueWithIndex(new ArrayList<String>(Arrays.asList("1")));
		propertyGroup.appendChild(getPropertyWithValueIndex(doc, attribs, valueListWithIndex));
		
		propertyGroup.appendChild(getProperty(doc, "Cosolve_Opsplit", "1"));
		propertyGroup.appendChild(getProperty(doc, "Allow_CD", "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "rho", "default", "true", "desc", "solid phase density", "dimension", "density", "hidden", "true", "unit", "kg/m3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "4.25"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupNanoModelPostProcess(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		propertyGroup.appendChild(getProperty(doc, "Number_KDE_point", "200"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "LB_KDE_Diameter", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e-10"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "UB_KDE_Diameter", "dimension", "length", "unit", "m")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0e-6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "KDE_Sigma", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "0.15"));
		
		propertyGroup.appendChild(getProperty(doc, "Number_ME_point", "1000"));
		propertyGroup.appendChild(getProperty(doc, "max_loop", "5000"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "min_c_max", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "max_c_max", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "3.0"));
		
		propertyGroup.appendChild(getProperty(doc, "N_c_max", "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "ATol_ME", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-8"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "RTol_ME", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-4"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "min_lambda", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "-3000.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Const_step", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "r_frac", "dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, attribs, "3.0"));
		
		propertyGroup.appendChild(getProperty(doc, "GaussianFlag", "1"));
		
		return propertyGroup;
	}
	
	public static Node getPropertyGroupMoMICModel(Document doc, String ref) {
		Element propertyGroup = doc.createElement("property_group");
		propertyGroup.setAttribute("ref", ref);
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Num_moments", "default", "true", "desc", "Number for Moments", "uitype", "int")));
		propertyGroup.appendChild(getProperty(doc, attribs, "4"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "M0_cutoff", "default", "true", "desc", "Number Density Cutoff", "dimension", "number_concentration", "unit", "#/m^3")));
		propertyGroup.appendChild(getProperty(doc, attribs, "1e-15"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Material", "default", "true", "desc", "Discrete phase material")));
		propertyGroup.appendChild(getProperty(doc, attribs, "MATERIAL1"));
		
		return propertyGroup;
	}
	
	public static Node getFlame(Document doc) {
		Element flame = doc.createElement("flame");
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("ref", "Simulation", "index", "1")));
		flame.appendChild(getPropertyGroupForFlame(doc, attribs));
		
		return flame;
	}
	
	public static Node getMixtures(Document doc, String type, String jsonString) {
		Element mixtures = doc.createElement("mixtures");
		mixtures.setAttribute("type", type);
		
		String oxidiser = JsonPath.read(jsonString, "$.kinetics.oxidiser");
		ArrayList<String> species = JsonPath.read(jsonString, "$.kinetics.mixtures.composition");
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "gas fraction", "name", "Fuel", "unit", "mole fraction")));
		speciesList = generateSpecies(new ArrayList<String>(Arrays.asList("DMM3", "1.0")));
		mixtures.appendChild(getComposition(doc, attribs, speciesList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "gas fraction", "name", "Oxidiser_Phi=0.5", "unit", "mole fraction")));
		speciesList = generateSpecies(new ArrayList<String>(Arrays.asList("N2", "0.8888888888888888", "O2", "0.1111111111111111")));
		mixtures.appendChild(getComposition(doc, attribs, speciesList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "gas fraction", "name", "Oxidiser_Phi=1", "unit", "mole fraction")));
		speciesList = generateSpecies(new ArrayList<String>(Arrays.asList("O2", "0.0625", "N2", "0.9375")));
		mixtures.appendChild(getComposition(doc, attribs, speciesList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "gas fraction", "name", "Oxidiser_Phi=1.5", "unit", "mole fraction")));
		speciesList = generateSpecies(new ArrayList<String>(Arrays.asList("O2", "0.047619047619047616", "N2", "0.9523809523809523")));
		mixtures.appendChild(getComposition(doc, attribs, speciesList));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "gas fraction", "name", oxidiser, "unit", "mole fraction")));
		speciesList = generateSpecies(species);
		mixtures.appendChild(getComposition(doc, attribs, speciesList));
		
		return mixtures;
	}
	
	public static Node getMixturesSimple(Document doc, String type) {
		Element mixtures = doc.createElement("mixtures");
		mixtures.setAttribute("type", type);
		return mixtures;
	}
	
	
	
//	private static Node getPropertyGroup(Document doc, String ref, ArrayList<String> valueList) {
//		Element propertyGroup = doc.createElement("property_group");
//		propertyGroup.setAttribute("ref", ref);
//		propertyGroup.appendChild(getProperty(doc, ))
//	}
	
	private static Node getPropertyGroupForFlame(Document doc, ArrayList<ArrayList<String>> attribs) {
		Element propertyGroup = doc.createElement("property_group");
		for (ArrayList<String> attrib : attribs) {
			propertyGroup.setAttribute(attrib.get(0), attrib.get(1));
		}
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "prob", attribs, "LA"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "use_NANOMODEL", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "ignoreFluxInBC", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "FOAMfriendly", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "burn", attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "pressure", "unit", "atm")));
		propertyGroup.appendChild(getProperty(doc, "pres", attribs, "1.0e0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "mass flux", "unit", "g/cm^2/s")));
		propertyGroup.appendChild(getProperty(doc, "flrt", attribs, "6.843e-3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, "xfixt", attribs, "1.0e0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, "tfixt", attribs, "2000.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "speed", "unit", "cm/s")));
		propertyGroup.appendChild(getProperty(doc, "vfue", attribs, "5.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "speed", "unit", "cm/s")));
		propertyGroup.appendChild(getProperty(doc, "voxi", attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "velocity gradient", "unit", "1/s")));
		propertyGroup.appendChild(getProperty(doc, "afue", attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "velocity gradient", "unit", "1/s")));
		propertyGroup.appendChild(getProperty(doc, "aoxi", attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, "tfue", attribs, "298.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, "toxi", attribs, "1233.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "tfix", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "enrg", attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "noft", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "iniprof", attribs, "3"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, "xend", attribs, "2.15"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, "xcen", attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "length", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, "wmix", attribs, "1.5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "temperature", "unit", "K")));
		propertyGroup.appendChild(getProperty(doc, "max_temp", attribs, "2200.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "ustg", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "mult", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "tdif", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "rad1", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "rad2", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "equl", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "fxeq", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "gfac", attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "asen", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "adapt", attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "nmax", attribs, "500"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "padd", attribs, "100"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "npts", attribs, "6"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "usegrid", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "grid", attribs, "0.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "steady", attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "JacREL", attribs, "0.0e0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "JacABS", attribs, "0.0e0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "ssage", attribs, "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "ssrel", attribs, "1.0e-4"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "ssabs", attribs, "1.0e-11"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "tdage", attribs, "5"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "tdrel", attribs, "1.0e-4"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "tdabs", attribs, "1.0e-8"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "steps0", attribs, "200"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "steps1", attribs, "200"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "steps2", attribs, "10"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "strid0", attribs, "1.0e-7"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "tdec", attribs, "2.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "tinc", attribs, "3.1623"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "stride_max", attribs, "1.0e0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "stride_min", attribs, "1.0e-20"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "stride_min_fix", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "toler0", attribs, "1.0e-7"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "toler1", attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "toler2", attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "max_SEARCH", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "deltaDexp", attribs, "10"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "wdif", attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "sflr", attribs, "-1.0e-4"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "rstr", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "RestartFileName", attribs, ""));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "regrid", attribs, "0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "jjrg", attribs, "40"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "pcad", attribs, "0.7"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("dimension", "dimensionless", "unit", "-")));
		propertyGroup.appendChild(getProperty(doc, "rgtc", attribs, "1.0"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList()));
		propertyGroup.appendChild(getProperty(doc, "skip", attribs, "1"));
		
		attribs = generateAttribs(new ArrayList<String>(Arrays.asList("allowprofile", "always", "dimension", "temperature", "readfrom", "profile", "unit", "K")));
		profileAttribs = generateAttribs(new ArrayList<String>(Arrays.asList("delim", ",", "delimlen", "1", "dimension_lookup", "independent_variable", "headers", "1", 
				"indvarcol", "1", "mergedelims", "0", "periodicity", "0", "trim_leadb", "1", "trim_trailb", "1", "unit", "cm")));
		propertyGroup.appendChild(getProperty(doc, "ImpTemp", attribs, "0.0", profileAttribs, "ImposedFlameTempProfile.csv"));
		
		return propertyGroup;
	}
	
	private static ArrayList<ArrayList<String>> generateAttribs(ArrayList<String> attribs) {
		ArrayList<ArrayList<String>> attribList = new ArrayList<ArrayList<String>>();
		if (attribs.size()%2 == 0) {
			for (int i=0; i<attribs.size(); i+=2) {
				attribList.add(new ArrayList<String>(attribs.subList(i, i+2)));
			}
		} else {
			System.out.println("Please provide attribute and its name in pairs.");
		}
		return attribList;
	}
	
	private static ArrayList<ArrayList<String>> generateValueWithIndex(ArrayList<String> valueListWithIndex) {
		ArrayList<ArrayList<String>> valueList = new ArrayList<ArrayList<String>>();
		for (int i=0; i<valueListWithIndex.size(); i+=1) {
			ArrayList<String> valueWithIndex = new ArrayList<String>(Arrays.asList(Integer.toString(i+1), valueListWithIndex.get(i)));
			valueList.add(valueWithIndex);
		}
		return valueList;
	}
	
	private static ArrayList<ArrayList<String>> generateValueWithLookup(ArrayList<String> valueListWithLookup) {
		ArrayList<ArrayList<String>> valueList = new ArrayList<ArrayList<String>>();
		if (valueListWithLookup.size()%2 == 0) {
			for (int i=0; i<valueListWithLookup.size(); i+=2) {
				valueList.add(new ArrayList<String>(valueListWithLookup.subList(i, i+2)));
			}
		} else {
			System.out.println("Please provide value and its name in pairs.");
		}
		return valueList;
	}
	
	private static ArrayList<ArrayList<String>> generateSpecies(ArrayList<String> valueListForSpecies) {
		ArrayList<ArrayList<String>> speciesList = new ArrayList<ArrayList<String>>();
		if (valueListForSpecies.size()%2 == 0) {
			for (int i=0; i<valueListForSpecies.size(); i+=2) {
				speciesList.add(new ArrayList<String>(valueListForSpecies.subList(i, i+2)));
			}
		} else {
			System.out.println("Please provide composition and the species it represents to in pairs.");
		}
		return speciesList;
	}
	
	private static Node getProperty(Document doc, ArrayList<ArrayList<String>> attribs) {
		Element property = doc.createElement("property");
		for (ArrayList<String> attrib : attribs) {
			property.setAttribute(attrib.get(0), attrib.get(1));
		}
		return property;
	}
	
	private static Node getProperty(Document doc, String ref, String value) {
		Element property = doc.createElement("property");
		property.setAttribute("ref", ref);
		property.appendChild(getValue(doc, value));
		return property;
	}
	
	private static Node getProperty(Document doc, String ref, ArrayList<String> valueList) {
		Element property = doc.createElement("property");
		property.setAttribute("ref", ref);
		for (String value : valueList) {
			property.appendChild(getValue(doc, value));
		}
		return property;
	}
	
	private static Node getProperty(Document doc, ArrayList<ArrayList<String>> attribs, String value) {
		Element property = doc.createElement("property");
		for (ArrayList<String> attrib : attribs) {
			property.setAttribute(attrib.get(0), attrib.get(1));
		}
		property.appendChild(getValue(doc, value));
		return property;
	}
	
	private static Node getProperty(Document doc, String name, ArrayList<ArrayList<String>> attribs, String value) {
		Element property = doc.createElement(name);
		if (attribs!=null & !attribs.isEmpty()) {
			for (ArrayList<String> attrib : attribs) {
				property.setAttribute(attrib.get(0), attrib.get(1));
			}
		}
		property.appendChild(getValue(doc, value));
		return property;
	}
	
	
	private static Node getMap(Document doc, String reactorModel, ArrayList<ArrayList<String>> valueListWithLookup) {
		Element map = doc.createElement("map");
		map.setAttribute("ReactorModel", reactorModel);
		for (ArrayList<String> valueWithLookup : valueListWithLookup) {
			map.appendChild(getValueWithLookup(doc, valueWithLookup));
		}
		return map;
	}
	
	private static Node getComposition(Document doc, ArrayList<ArrayList<String>> attribs, ArrayList<ArrayList<String>> speciesList) {
		Element composition = doc.createElement("composition");
		for (ArrayList<String> attrib : attribs) {
			composition.setAttribute(attrib.get(0), attrib.get(1));
		}
		for (ArrayList<String> species : speciesList) {
			composition.appendChild(getValueForSpecies(doc, species));
		}
		return composition;
	}
	
	private static Node getProperty(Document doc, ArrayList<ArrayList<String>> attribs, ArrayList<String> valueList) {
		Element property = doc.createElement("property");
		for (ArrayList<String> attrib : attribs) {
			property.setAttribute(attrib.get(0), attrib.get(1));
		}
		for (String value : valueList) {
			property.appendChild(getValue(doc, value));
		}
		return property;
	}
	
	private static Node getPropertyWithValueIndex(Document doc, ArrayList<ArrayList<String>> attribs, ArrayList<ArrayList<String>> valueListWithIndex) {
		Element property = doc.createElement("property");
		for (ArrayList<String> attrib : attribs) {
			property.setAttribute(attrib.get(0), attrib.get(1));
		}
		for (ArrayList<String> valueWithIndex : valueListWithIndex) {
			property.appendChild(getValue(doc, valueWithIndex));
		}
		return property;
	}
	
	private static Node getProperty(Document doc, ArrayList<ArrayList<String>> attribs, String value, ArrayList<ArrayList<String>> profileAttribs, String profileValue) {
		Element property = doc.createElement("property");
		for (ArrayList<String> attrib : attribs) {
			property.setAttribute(attrib.get(0), attrib.get(1));
		}
		property.appendChild(getValue(doc, value));
		property.appendChild(getProfile(doc, profileAttribs, profileValue));
		return property;
	}
	
	private static Node getProperty(Document doc, String name, ArrayList<ArrayList<String>> attribs, String value, ArrayList<ArrayList<String>> profileAttribs, String profileValue) {
		Element property = doc.createElement(name);
		for (ArrayList<String> attrib : attribs) {
			property.setAttribute(attrib.get(0), attrib.get(1));
		}
		property.appendChild(getValue(doc, value));
		property.appendChild(getProfile(doc, profileAttribs, profileValue));
		return property;
	}
	
	private static Node getMixtureProfile(Document doc, ArrayList<ArrayList<String>> attribs, String value, ArrayList<ArrayList<String>> profileAttribs, String profileValue) {
		Element property = doc.createElement("mixtureprofile");
		for (ArrayList<String> attrib : attribs) {
			property.setAttribute(attrib.get(0), attrib.get(1));
		}
		property.appendChild(getValue(doc, value));
		property.appendChild(getProfile(doc, profileAttribs, profileValue));
		return property;
	}
	
	private static Node getMixturename(Document doc, String type, String value) {
		Element property = doc.createElement("mixturename");
		property.setAttribute("type", type);
		property.appendChild(getValue(doc, value));
		return property;
	}
	
	private static Node getValue(Document doc, String value) {
		Element node = doc.createElement("value");
		node.appendChild(doc.createTextNode(value));
		return node;
	}
	
	private static Node getValue(Document doc, ArrayList<String> valueWithIndex) {
		Element node = doc.createElement("value");
		node.setAttribute("index", valueWithIndex.get(0));
		node.appendChild(doc.createTextNode(valueWithIndex.get(1)));
		return node;
	}
	
	private static Node getValueWithLookup(Document doc, ArrayList<String> valueWithLookup) {
		Element node = doc.createElement("value");
		node.setAttribute("lookup", valueWithLookup.get(0));
		node.appendChild(doc.createTextNode(valueWithLookup.get(1)));
		return node;
	}
	
	private static Node getValueForSpecies(Document doc, ArrayList<String> species) {
		Element value = doc.createElement("value");
		value.setAttribute("species", species.get(0));
		value.appendChild(doc.createTextNode(species.get(1)));
		return value;
	}
	
	private static Node getProfile(Document doc, ArrayList<ArrayList<String>> attribs, String value) {
		Element profile = doc.createElement("profile");
		for (ArrayList<String> attrib : attribs) {
			profile.setAttribute(attrib.get(0), attrib.get(1));
		}
		profile.appendChild(getValue(doc, value));
		return profile;
	}
}
