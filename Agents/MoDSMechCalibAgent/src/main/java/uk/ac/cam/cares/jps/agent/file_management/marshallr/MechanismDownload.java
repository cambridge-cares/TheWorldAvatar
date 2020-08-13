package uk.ac.cam.cares.jps.agent.file_management.marshallr;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

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

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class MechanismDownload {
	
	public static void main(String[] args) throws MalformedURLException, SAXException, IOException, ParserConfigurationException, TransformerFactoryConfigurationError, TransformerException {
		MechanismDownload mechanismDownload = new MechanismDownload();
		mechanismDownload.obtainMechanism("http://www.theworldavatar.com/data/ontokin/pode_mechanism_testing/mechanism.xml", "C:\\Users\\jb2197\\Documents\\c4e-jb2197-OntologyTools\\Codes\\thermochemistry\\MoDSMechCalibAgent\\src\\main\\resources\\JobFolder\\All\\chemical_mechanism.xml");
	}
	
	public void obtainMechanism(String url, String targetPath) throws MalformedURLException, SAXException, IOException, ParserConfigurationException, TransformerFactoryConfigurationError, TransformerException {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
		Document doc = db.parse(new URL(url).openStream());
		
		DOMSource source = new DOMSource(doc);
		StreamResult outputFile = new StreamResult(new File(targetPath));
		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		transformer.transform(source, outputFile);
		System.out.println("\nMechanism downloaded successfully.");
	}
}
