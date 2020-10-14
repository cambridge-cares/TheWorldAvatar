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

import uk.ac.cam.cares.jps.agent.configuration.AutoMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.MoDSInputsState;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.Property;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.Utils;
import uk.ac.cam.cares.jps.kg.OntoException;
import uk.ac.cam.cares.jps.kg.OntoKinKG;
import uk.ac.cam.cares.jps.kg.RepositoryManager;

public class MechCalibOutputProcess {
	private AutoMechCalibAgentProperty autoMechCalibAgentProperty;
	
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
	
	public MechCalibOutputProcess(AutoMechCalibAgentProperty autoMechCalibAgentProperty) {
		this.autoMechCalibAgentProperty = autoMechCalibAgentProperty;
	}
	
	public void processResults(String jobFolderPath, String jsonString) throws IOException, AutoMechCalibAgentException {
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = JSonRequestParser.getOntoKinReactionsIRI(jsonString);
		String timeStamp = ""+Utils.getTimeStamp();
		String mechFolder = "";
		if (jobFolderPath.endsWith("\\")) {
			jobFolderPath = jobFolderPath.substring(0,jobFolderPath.length()-1);
		}
		mechFolder = jobFolderPath.concat(File.separator).concat("mechanism_").concat(timeStamp);
		new File(mechFolder).mkdirs();
		String calibSum = jobFolderPath.concat(File.separator).concat("CalibrationAlg\\CalibrationAlg_Summary.csv");
		String paramOutputResults = mechFolder.concat(File.separator).concat("UpdatedPreExpFactors.csv");
//		String mechXml = mechFolder+"mechanism.xml";
		String mechOwlUpload = "mechanism_"+timeStamp+".owl";
		
		// download the mechanism owl file, and convert it to xml format
		String mechHttpPath = mechanismIRI.substring(0, mechanismIRI.indexOf("#"));
		String mechOWL = mechFolder.concat(File.separator).concat(mechHttpPath.substring(mechHttpPath.lastIndexOf("/")+1));
		RepositoryManager repo = new RepositoryManager();
		try {
			repo.downloadOntology(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), mechHttpPath.substring(mechHttpPath.lastIndexOf("/")+1), mechHttpPath, 
					Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName(), mechOWL);
		} catch (OntoException e) {
			e.printStackTrace();
		}
		OwlConverter owlConverter = new OwlConverter();
		ArrayList<String> mechanismOwlFiles = new ArrayList<>();
		mechanismOwlFiles.add(mechOWL);
		try {
			owlConverter.convert(mechanismOwlFiles, mechFolder.replace("\\", "/"));
		} catch (OWLOntologyCreationException | com.cmclinnovations.ontochem.model.exception.OntoException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
		String mechXML = mechOWL.replace(".owl", ".xml");
		
		// obtain the xml version of mech file TODO to be replaced with owlConverter convert
//		MechanismDownload mechanismDownload = new MechanismDownload();
//		try {
//			String mechanismWebPath = mechanismIRI.substring(0, mechanismIRI.indexOf("#"))
//					.replace("/kb/", "/data/").replace(".owl", "/mechanism.xml");
//			mechanismDownload.obtainMechanism(mechanismWebPath, mechXml);
//		} catch (SAXException | ParserConfigurationException | TransformerFactoryConfigurationError
//				| TransformerException e) {
//			e.printStackTrace();
//		}
		
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
			obtainNewRateParams(new File(calibSum), new File(paramOutputResults), new File(mechXML), mechanismIRI, reactionIRIList);
		} catch (XPathExpressionException | ParserConfigurationException | SAXException
				| TransformerFactoryConfigurationError | TransformerException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		// get the owl format of mech.xml with new name mechanism_timeStamp.owl
		CtmlConverter ctmlConverter = new CtmlConverter();
		ArrayList<String> mechCtmlFiles = new ArrayList<>();
		mechCtmlFiles.add(mechXML);
		try {
			ctmlConverter.convert(mechCtmlFiles, mechFolder);
		} catch (OWLOntologyCreationException | com.cmclinnovations.ontochem.model.exception.OntoException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		// upload the updated owl to server
		try {
			repo.loadOntology(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
					mechOwlUpload, mechFolder.concat(File.separator), Property.RDF4J_ONTOKIN_KB_URL.getPropertyName(), 
					Property.RDF4J_ONTOKIN_REPOSITORY_ID.getPropertyName());
		} catch (OntoException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	private void obtainNewRateParams(File calibSummary, File resultsFile, File mechXml, String mechanismIRI, List<String> reactionIRIList) 
			throws IOException, AutoMechCalibAgentException, ParserConfigurationException, SAXException, XPathExpressionException, TransformerFactoryConfigurationError, TransformerException {
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
		OntoKinKG ontokinkg = new OntoKinKG(autoMechCalibAgentProperty);
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
