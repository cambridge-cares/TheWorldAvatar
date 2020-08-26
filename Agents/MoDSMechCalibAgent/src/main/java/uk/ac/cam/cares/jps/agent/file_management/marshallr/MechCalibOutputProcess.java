package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontochem.model.converter.owl.OwlConverter;
import com.google.common.primitives.Doubles;

import uk.ac.cam.cares.jps.agent.file_management.MoDSInputsState;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.Property;
import uk.ac.cam.cares.jps.kg.OntoException;
import uk.ac.cam.cares.jps.kg.OntoKinKG;
import uk.ac.cam.cares.jps.kg.RepositoryManager;

public class MechCalibOutputProcess {
	private LinkedHashMap<String, String> origParams = new LinkedHashMap<String, String>();
	private LinkedHashMap<String, String> multipliers = new LinkedHashMap<String, String>();
	private LinkedHashMap<String, String> updatedParams = new LinkedHashMap<String, String>();
	
	public LinkedHashMap<String, String> getOrigParams() {
		return origParams;
	}

	public void setOrigParams(LinkedHashMap<String, String> origParams) {
		this.origParams = origParams;
	}

	public LinkedHashMap<String, String> getMultipliers() {
		return multipliers;
	}

	public void setMultipliers(LinkedHashMap<String, String> multipliers) {
		this.multipliers = multipliers;
	}

	public LinkedHashMap<String, String> getUpdatedParams() {
		return updatedParams;
	}

	public void setUpdatedParams(LinkedHashMap<String, String> updatedParams) {
		this.updatedParams = updatedParams;
	}
	
//	private String getCorrespondParam(String key) {
//		return getUpdatedParams().get(key);
//	}
	
	public static void main(String[] args) {
		MechCalibOutputProcess mechCalibPro = new MechCalibOutputProcess();
		
		String jobFolderPath = "C:\\Users\\jb2197\\Desktop\\PODE_Project\\Data\\Temp";
		String mechanismIRI = "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237";
		List<String> reactionIRIList = new ArrayList<String>(Arrays.asList(new String[] {"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570512_48",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570503_39",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570639_175",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570640_176",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570509_45",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570499_35",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570607_143",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570631_167",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570634_170",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570633_169",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570504_40",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570502_38",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570618_154",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570505_41",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570638_174",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570517_53",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570604_140",
				"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570624_160"}));
		String timeStamp = String.valueOf(System.nanoTime());
		
		try {
			mechCalibPro.processResults(jobFolderPath, mechanismIRI, reactionIRIList, timeStamp);
		} catch (IOException | MoDSMechCalibAgentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
	public void processResults(String jobFolderPath, String mechanismIRI, List<String> reactionIRIList, String timeStamp) throws IOException, MoDSMechCalibAgentException {
		String mechFolder = "";
		if (!jobFolderPath.endsWith("\\")) {
			jobFolderPath = jobFolderPath.concat("\\");
			mechFolder = jobFolderPath.concat("mechanism_").concat(timeStamp).concat("\\");
			new File(mechFolder).mkdir();
		}
		String calibSum = jobFolderPath+"CalibrationAlg\\CalibrationAlg_Summary.csv";
		String paramOutputResults = mechFolder+"UpdatedPreExpFactors.csv";
		String mechOwl = mechFolder+"mechanism__.owl";
		String mechXml = mechFolder+"mechanism.xml";
		String mechOwlUp = "mechanism_"+timeStamp+".owl";
		
		// download the mech file
		String mechWebPath = mechanismIRI.substring(0, mechanismIRI.indexOf("#"));
		RepositoryManager repo = new RepositoryManager();
		try {
			repo.downloadOntology(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), mechWebPath.substring(mechWebPath.lastIndexOf("/")+1), mechWebPath, 
					Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName(), mechOwl);
		} catch (OntoException e) {
			e.printStackTrace();
		}
		
		// obtain the xml version of mech file TODO to be replaced with owlConverter convert
		MechanismDownload mechanismDownload = new MechanismDownload();
		try {
			String mechanismWebPath = mechanismIRI.substring(0, mechanismIRI.indexOf("#"))
					.replace("/kb/", "/data/").replace(".owl", "/mechanism.xml");
			mechanismDownload.obtainMechanism(mechanismWebPath, mechXml);
		} catch (SAXException | ParserConfigurationException | TransformerFactoryConfigurationError
				| TransformerException e) {
			e.printStackTrace();
		}
		
		// TODO
//		OwlConverter owlConverter = new OwlConverter();
//		ArrayList<String> mechanismOwlFiles = new ArrayList<>();
//		mechanismOwlFiles.add(mechOwl);
//		try {
//			owlConverter.convert(mechanismOwlFiles, jobFolderPath);
//		} catch (OWLOntologyCreationException | com.cmclinnovations.ontochem.model.exception.OntoException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		
		// obtain rate parameters, then update the rate params in the mech.xml
		try {
			obtainNewRateParams(new File(calibSum), new File(paramOutputResults), new File(mechXml), mechanismIRI, reactionIRIList);
		} catch (XPathExpressionException | ParserConfigurationException | SAXException
				| TransformerFactoryConfigurationError | TransformerException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		// get the owl format of mech.xml with new name mechanism_timeStamp.owl
		CtmlConverter ctmlConverter = new CtmlConverter();
		ArrayList<String> mechCtmlFiles = new ArrayList<>();
		mechCtmlFiles.add(mechXml);
		try {
			ctmlConverter.convert(mechCtmlFiles, mechXml.substring(0, mechXml.lastIndexOf("\\")).concat("\\"));
		} catch (OWLOntologyCreationException | com.cmclinnovations.ontochem.model.exception.OntoException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		// upload the updated owl to server
		try {
			repo.loadOntology(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
					mechOwlUp, mechFolder, Property.RDF4J_ONTOKIN_KB_URL.getPropertyName(), 
					Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName());
		} catch (OntoException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	private void obtainNewRateParams(File calibSummary, File resultsFile, File mechXml, String mechanismIRI, List<String> reactionIRIList) 
			throws IOException, MoDSMechCalibAgentException, ParserConfigurationException, SAXException, XPathExpressionException, TransformerFactoryConfigurationError, TransformerException {
		// get the list of new multipliers
		if (calibSummary.isFile()) {
			BufferedReader br = new BufferedReader(new FileReader(calibSummary));
			// read the header to get the list of rxns
			String[] header = br.readLine().split(",");
			// get the column index of First/Last/Best and SumOfSquares
			int flbIdx = 1;
			int ofIdx = 3;
			for (int i = 0; i < header.length; i++) {
				if (header[i].toLowerCase().contains("first/last/best")) {
					flbIdx = i;
				} else if (header[i].toLowerCase().contains("sumofsquares")) {
					ofIdx = i;
				}
			}
			// get the best run of optimisation
			String line = "";
			String[] currentBest = new String[header.length];
			String bestline = "";
			boolean init = true;
			while ((line = br.readLine()) != null) {
				String[] dataLine = line.split(",");
				if (dataLine[flbIdx].contains("2")) {
					if (init) {
						bestline = line;
						init = false;
						currentBest = bestline.split(",");
					} else {
						if (Double.parseDouble(currentBest[ofIdx]) > Double.parseDouble(dataLine[ofIdx])) {
							bestline = line;
							currentBest = bestline.split(",");
						}
					}
				}
			}
			br.close();
			// read the multipliers
			for (int i = 0; i < header.length; i++) {
				if (header[i].contains(Property.MODEL_KINETICS.getPropertyName())) {
					multipliers.put(header[i].substring(header[i].lastIndexOf("_")+1), currentBest[i]);
				}
			}
		}
		
		// query the rate params
		OntoKinKG ontokinkg = new OntoKinKG();
		LinkedHashMap<String, String> orig = ontokinkg.queryRxnPreExpFactor(mechanismIRI, reactionIRIList);
		List<String[]> dataLines = new ArrayList<>();
		dataLines.add(new String[] {"ReactionNo", "OrigPreExpFactor", "Multiplier", "UpdatedPreExpFactor"});
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder;
		builder = factory.newDocumentBuilder();
		Document doc = builder.parse(mechXml);
		XPathFactory xPathfactory = XPathFactory.newInstance();
		XPath xpath = xPathfactory.newXPath();
		
		for (String rxn : orig.keySet()) {
			String[] token = orig.get(rxn).split("_");
			// get the original rate params
			origParams.put(rxn, token[0]);
			double updatedCoef = Double.parseDouble(origParams.get(rxn)) * Double.parseDouble(multipliers.get(rxn));
			// get the updated rate params
			updatedParams.put(rxn, Double.toString(updatedCoef));
			// add all info to dataLines for writing to file
			dataLines.add(new String[] {rxn, origParams.get(rxn), multipliers.get(rxn), updatedParams.get(rxn)});
			
			// update the xml version of mechanism file
			
			XPathExpression expr = xpath.compile("//ctml/reactionData/reaction[@id=\""+rxn+"\"]/rateCoeff/Arrhenius/A[@units=\""+token[1]+"\"]/text()");
			NodeList nl = (NodeList) expr.evaluate(doc, XPathConstants.NODESET);
			nl.item(0).setNodeValue(Double.toString(updatedCoef)); 
		}
		// write to file
		try (PrintWriter pw = new PrintWriter(resultsFile)) {
			dataLines.stream()
			.map(this::convertToCSV)
			.forEach(pw::println);
		}
		Transformer xformer = TransformerFactory.newInstance().newTransformer();
		xformer.transform(new DOMSource(doc), new StreamResult(mechXml));
	}
	
	/**
	 * Convert a string array to a string in the format of CSV file. 
	 * 
	 * @param data
	 * @return
	 */
	private String convertToCSV(String[] data) {
	    return Stream.of(data)
	      .map(this::escapeSpecialCharacters)
	      .collect(Collectors.joining(","));
	}
	
	/**
	 * Escape special characters when converting string array to string in the format of CSV file. 
	 * 
	 * @param data
	 * @return
	 */
	private String escapeSpecialCharacters(String data) {
	    String escapedData = data.replaceAll("\\R", " ");
	    if (data.contains(",") || data.contains("\"") || data.contains("'")) {
	        data = data.replace("\"", "\"\"");
	        escapedData = "\"" + data + "\"";
	    }
	    return escapedData;
	}
}
