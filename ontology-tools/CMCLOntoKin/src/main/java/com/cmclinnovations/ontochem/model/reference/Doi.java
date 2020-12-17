package com.cmclinnovations.ontochem.model.reference;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import com.cmclinnovations.ontochem.model.IInitCtmlConverter;
import com.cmclinnovations.ontochem.model.InitCtmlConverter;
import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontochem.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.reference.data.structure.Creator;
import com.cmclinnovations.ontochem.model.reference.data.structure.Journal;
import com.cmclinnovations.ontochem.model.reference.data.structure.JournalSpecification;
import com.cmclinnovations.ontochem.model.reference.data.structure.Reference;

/**
 * Implements all the methods related to DOIs.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public class Doi extends CtmlConverter implements IDoi{
	
	private static final String IRI_PERSON = "<http://xmlns.com/foaf/0.1/Person>";
	private static final String IRI_FAMILIY_NAME = "<http://xmlns.com/foaf/0.1/familyName>";
	private static final String IRI_GIVEN_NAME = "<http://xmlns.com/foaf/0.1/givenName>";
	private static final String IRI_NAME = "<http://xmlns.com/foaf/0.1/name>";
	private static final String IRI_JOURNAL = "<http://purl.org/ontology/bibo/Journal>";
	private static final String IRI_ISSN = "<http://purl.org/ontology/bibo/issn>";
	private static final String IRI_TITLE = "<http://purl.org/dc/terms/title>";
	private static final String IRI_VOLUME = "<http://purl.org/ontology/bibo/volume>";
	private static final String IRI_PAGE_START = "<http://purl.org/ontology/bibo/pageStart>";
	private static final String IRI_PAGE_END = "<http://purl.org/ontology/bibo/pageEnd>";
	private static final String IRI_BASE_DX_DOI = "<http://dx.doi.org/";
	private static boolean iriDxDoiFlag = false;
	private static boolean iriJournalFlag = false;
	
	private static final String CMD_PREAMBLE = 
	        "curl -D - -L -H \"Accept: text/turtle\" \"http://dx.doi.org/";
	
	/**
	 * A set of dois.
	 */
	private List<String> doi;
	
	/**
	 * 
	 * @param doi
	 */
	public void add(List<String> doi){
		this.doi = doi;
	}
	
	/**
	 * Extracts content of a set of DOIs.
	 */
	public void extractContent() throws OntoException{
		if(doi!=null){
			// Initialise the instances of the classes that hold reference
			// metadata and data.
			IInitCtmlConverter iCtmlConverter = new InitCtmlConverter();
			iCtmlConverter.initReference();
			for(String adoi:doi){
				references.add(reference);
				extractContent(adoi);
				reference = new Reference();
				creators = new ArrayList<>();
				journalSpec = new JournalSpecification();
				journal = new Journal();
			}
		}
	}
	
	/**
	 * Extract content of a DOI.
	 * 
	 * @param doi
	 * @throws OntoException
	 */
	private void extractContent(String doi) throws OntoException {
		try {
			System.out.println("CMD:"+CMD_PREAMBLE+doi+"\"");
			// Run the Windows command
			Process process = Runtime.getRuntime().exec(CMD_PREAMBLE+doi+"\"");
			// Get input streams
			BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));
			// Read command standard output
			String s;
			String previous = "";
			// Processes standard output that does not include errors.
			while ((s = stdInput.readLine()) != null) {
				System.out.println(s);
				if(!s.isEmpty()){
					populateContent(s, previous);
				}
				previous = s;
			}
		} catch (IOException e) {
			e.printStackTrace();
			throw new OntoException("An IOException occurred while performing the query:" + CMD_PREAMBLE.concat(doi));
		}
	}
	
	/**
	 * Decides where to put the content based on the following two items:</br?
	 * 1. Current string.</br>
	 * 2. Previously parsed string.
	 * 
	 * @param current
	 * @param previous
	 * @throws OntoException
	 */
	private void populateContent(String current, String previous) throws OntoException {
		if(previous.contains(IRI_PERSON)){
			creator = new Creator();
			creators.add(creator);
		} else if(previous.contains(IRI_JOURNAL)){
			reference.setPublicationSpecification(journalSpec);
			journalSpec.setJournal(journal);
			reference.setContributor(creators);
			iriDxDoiFlag = false;
			iriJournalFlag = true;
		} else if(previous.trim().startsWith(IRI_BASE_DX_DOI)){
			iriDxDoiFlag = true;
			iriJournalFlag = false;			
		} else if(iriDxDoiFlag && previous.contains(IRI_TITLE)){
			reference.setTitle(normaliseString(current));
		} else if(iriDxDoiFlag && previous.contains(IRI_VOLUME)){
			journalSpec.setVolume(Integer.parseInt(normaliseString(current)));
		} else if(iriDxDoiFlag && previous.contains(IRI_PAGE_START)){
			journalSpec.setPageStart(Integer.parseInt(normaliseString(current)));
		} else if(iriDxDoiFlag && previous.contains(IRI_PAGE_END)){
			journalSpec.setPageEnd(Integer.parseInt(normaliseString(current)));
		} else if(previous.contains(IRI_FAMILIY_NAME)){
			creator.setFamilyName(normaliseString(current));
		} else if(previous.contains(IRI_GIVEN_NAME)){
			creator.setGivenName(normaliseString(current));
		} else if(previous.contains(IRI_NAME)){
			creator.setName(normaliseString(current));
		} else if(previous.contains(IRI_ISSN)){
			journal.setiSSN(normaliseString(current));
		} else if(iriJournalFlag && previous.contains(IRI_TITLE)){
			journal.setTitle(normaliseString(current));
		} 
	}
	
	/**
	 * Removes the unwanted substrings (e.g. ;, " and starting & closing space)</br> 
	 * from a string.
	 * 
	 * @param original
	 * @return
	 */
	private String normaliseString(String original){
		if(original==null){
			return null;
		}
		if(original.contains(";")){
			original = original.replace(";", "");
		}
		if(original.contains("\"")){
			original = original.replaceAll("\"", "");
		}
		original = original.trim();
		if(original.contains(" ") && original.lastIndexOf(".")+1==original.length()){
			original = original.substring(0, original.length()-1);
		}
		System.out.println("normalised string:"+original.trim());
		return original.trim();
	}
}
