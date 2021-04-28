package com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
/**
 * Adapted from Feroz's code. 
 *
 */
public class Doi extends PrimeConverter implements IDoi {
	
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
	
	private static final String CMD_PREAMBLE = "curl -D - -L -H \"Accept: text/turtle\" \"http://dx.doi.org/";
	
	private String doi;
	
	@Override
	public void extractContent() throws OntoChemExpException {
		if (doi != null) {
			extractContent(doi);
		}
	}

	@Override
	public void add(String doi) {
		this.doi = doi;
	}
	
	private void extractContent(String doi) throws OntoChemExpException {
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
//				System.out.println(s);
				if(!s.isEmpty()){
					populateContent(s, previous);
				}
				previous = s;
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void populateContent(String current, String previous) throws OntoChemExpException {
		if(previous.contains(IRI_PERSON)){
			contributor = new Contributor();
			contributorList.add(contributor);
		} else if(previous.contains(IRI_JOURNAL)){
			bibliographyLink.setPublicationSpecification(journalSpec);
			journalSpec.setJournal(journal);
			bibliographyLink.setContributor(contributorList);
			iriDxDoiFlag = false;
			iriJournalFlag = true;
		} else if(previous.trim().startsWith(IRI_BASE_DX_DOI)){
			iriDxDoiFlag = true;
			iriJournalFlag = false;			
		} else if(iriDxDoiFlag && previous.contains(IRI_TITLE)){
			bibliographyLink.setTitle(normaliseString(current));
		} else if(iriDxDoiFlag && previous.contains(IRI_VOLUME)){
			journalSpec.setVolume(Integer.parseInt(normaliseString(current)));
		} else if(iriDxDoiFlag && previous.contains(IRI_PAGE_START)){
			journalSpec.setPageStart(Integer.parseInt(normaliseString(current)));
		} else if(iriDxDoiFlag && previous.contains(IRI_PAGE_END)){
			journalSpec.setPageEnd(Integer.parseInt(normaliseString(current)));
		} else if(previous.contains(IRI_FAMILIY_NAME)){
			contributor.setFamilyName(normaliseString(current));
		} else if(previous.contains(IRI_GIVEN_NAME)){
			contributor.setGivenName(normaliseString(current));
		} else if(previous.contains(IRI_NAME)){
			contributor.setName(normaliseString(current));
		} else if(previous.contains(IRI_ISSN)){
			journal.setIssn(normaliseString(current));
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
//		System.out.println("normalised string:"+original.trim());
		return original.trim();
	}
}
