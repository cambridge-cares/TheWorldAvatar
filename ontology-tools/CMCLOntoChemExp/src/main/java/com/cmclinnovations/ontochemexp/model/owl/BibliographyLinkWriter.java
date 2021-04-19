
package com.cmclinnovations.ontochemexp.model.owl;

import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.BibliographyLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.Contributor;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.Doi;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.JournalSpecification;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.PublicationSpecification;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.BibliographyLinkParseStatus;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public class BibliographyLinkWriter extends PrimeConverter implements IBibliographyLinkWriter {
	public void writer(String qName) throws SAXException {
		readBibliographyLink(qName);
	}

	/**
	 * Forwards the call to the methods that first read and then write
	 * BibliographyLink data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readBibliographyLink(String qName) throws SAXException {
		if (bibliographyLinkParseStatus.isBibliographyLink()) {
			createBibliographyLink();
			try {
				addAllAttributes();
			} catch (OntoChemExpException e) {
				e.printStackTrace();
			}
			linkBibliographyLinkToExperiment();
		}
	}

	private void createBibliographyLink() {
		bibliographyLinkCount += 1;
		
		currentBibliographyLinkInstance = "BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount);

		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassBibliographyLink(), currentBibliographyLinkInstance);
		} catch (ABoxManagementException e) {
			logger.error("An individual of bibliographyLink could not be created.");
		}
	}

	private void addAllAttributes() throws OntoChemExpException {
		if (bibliographyLink.getPreferredKey() != null && !bibliographyLink.getPreferredKey().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
						ontoChemExpVocabulary.getDataPropertyhasPreferredKey(), bibliographyLink.getPreferredKey(),
						STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

//		if (bibliographyLink.getPrimeID() != null && !bibliographyLink.getPrimeID().trim().isEmpty()) {
//			try {
//				iABoxManagement.addProperty(
//						"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(), bibliographyLink.getPrimeID(), STRING);
//			} catch (ABoxManagementException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//		}
		
		if (bibliographyLink.getDoi() != null && !bibliographyLink.getDoi().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
						ontoChemExpVocabulary.getDataPropertyhasDOI(), bibliographyLink.getDoi(),
						STRING);
				
				if (iDoi == null) {
					iDoi = new Doi();
				}
				iDoi.add(bibliographyLink.getDoi());
				try {
					iDoi.extractContent();
				} catch (OntoChemExpException e) {
					logger.info("OntoChemExpException occurred while extracting the DOI content.");
					e.printStackTrace();
				}
				
				if (bibliographyLink.getContributor() != null) {
					List<Contributor> contributors = bibliographyLink.getContributor();
					for (Contributor contributor : contributors) {
						createContributor(contributor, ontoChemExpVocabulary.getOntoKinReferencePerson());
					}
				}
				
				if (bibliographyLink.getPublicationSpecification() != null) {
					createPublicationSpecification(bibliographyLink.getPublicationSpecification(), ontoChemExpVocabulary.getOntoKinReferenceJournalSpecification());
				}
				
				if (bibliographyLink.getTitle() != null) {
					iABoxManagement.addProperty(
							"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
							ontoChemExpVocabulary.getOntoKinReferenceTitle(), bibliographyLink.getTitle(),
							STRING);
				}
				
				journalSpecInstanceId++;
				journalInstanceId++;
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		
	}

	/**
	 * Links the equipment to the experiment.
	 * 
	 */
	private void linkBibliographyLinkToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasBibliographyLink(),
					"Experiment" + UNDERSCORE + experimentInstanceId,
					"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount));
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a copyrigh and an experiment conducted using it.");
		}
	}

	public void writeValue() {
		if (bibliographyLink.getValue() != null && !bibliographyLink.getValue().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
						ontoChemExpVocabulary.getDataPropertyhasValue(), bibliographyLink.getValue(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
//     		IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//     		iABoxManagement.addProperty("BibliographyLink"+UNDERSCORE+(bibliographyLinkID+bibliographyLinkCount), 
//     				dataPropertyIRI, bibliographyLink.getValue(), STRING);
		}
	}

	public void setUP() {
		bibliographyLinkList.add(bibliographyLink);
		bibliographyLink = new BibliographyLink();
		experiment.setBibliographyLink(bibliographyLinkList);
		bibliographyLinkList = new ArrayList<BibliographyLink>();
	}
	
	
	private void createContributor(Contributor contributor, String type) throws OntoChemExpException {
		String contributorInstance = ontoChemExpVocabulary.getOntoKinReferenceAgent() + UNDERSCORE + experimentPerformerInstanceID;
		if (type.equalsIgnoreCase(ontoChemExpVocabulary.getOntoKinReferencePerson())) {
			createPersonInfo(contributor, contributorInstance);
			// Adds the contributors to a bibliographyLink
			addContributorToBibliographyLink(contributor, contributorInstance, ontoChemExpVocabulary.getOntoKinReferencePerson());
		}
		// Adds the performers to an experiment
		addPerformerToExperiment(contributor, contributorInstance, type);
				
		experimentPerformerInstanceID++;
	}
	
	/**
	 * Adds the following information to the person being created:</br>
	 * 1. Name.</br>
	 * 2. Given name.</br>
	 * 3. Family name.</br>
	 * 
	 * @param contributor
	 * @param instance
	 * @throws OntoChemExpException
	 */
	private void createPersonInfo(Contributor contributor, String instance) throws OntoChemExpException {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getOntoKinReferencePerson(), instance);
			
			// Adds the family name of one of the persons performed the current experiment
			iABoxManagement.addProperty(instance, ontoChemExpVocabulary.getOntoKinReferenceFamilyName(), contributor.getFamilyName(), STRING);
			
			// Adds the given name of one of the persons performed the current experiment
			iABoxManagement.addProperty(instance, ontoChemExpVocabulary.getOntoKinReferenceGivenName(), contributor.getGivenName(), STRING);
			
			// Adds the name of one of the persons performed the current experiment
			iABoxManagement.addProperty(instance, ontoChemExpVocabulary.getOntoKinReferenceName(), contributor.getName(), STRING);
		} catch (ABoxManagementException e) {
			logger.error("Performer of the experiment could not be created.");
			e.printStackTrace();
		}
	}
	
	/**
	 * Adds a performer to the current experiment. 
	 * 
	 * @param performer
	 * @param performerInstance
	 * @param type
	 * @throws OntoChemExpException
	 */
	private void addPerformerToExperiment(Contributor performer, String performerInstance, String type) throws OntoChemExpException{
		try {
			if (!type.equalsIgnoreCase(ontoChemExpVocabulary.getOntoKinReferencePerson())) {
				iABoxManagement.createIndividual(ontoChemExpVocabulary.getOntoKinReferenceAgent(), performerInstance);
			}
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getOntoChemExpExperimenthasPerformer(), 
					currentExperimentInstance, performerInstance);
		} catch (ABoxManagementException e) {
			logger.error("Performer could not be linked to Experiment.");
			e.printStackTrace();
		}
	}

	/**
	 * Adds a contributor to the current bibliographyLink. 
	 * 
	 * @param contributor
	 * @param contributorInstance
	 * @param type
	 * @throws OntoChemExpException
	 */
	private void addContributorToBibliographyLink(Contributor contributor, String contributorInstance, String type) throws OntoChemExpException{
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getOntoKinReferencehasContributor(), 
					currentBibliographyLinkInstance, contributorInstance);
		} catch (ABoxManagementException e) {
			logger.error("Contributor could not be linked to BibliographyLlink.");
			e.printStackTrace();
		}
	}

	/**
	 * Creates a publication specification which includes the following:</br>
	 * 1. A link to an actual publication, e.g. Journal or Proceedings.</br>
	 * 2. Publication info including data and meta data.
	 * @param publicationSpecification
	 */
	private void createPublicationSpecification(PublicationSpecification publicationSpecification, String type) throws OntoChemExpException {
		String pubInstance = ontoChemExpVocabulary.getOntoKinReferenceJournalSpecification() + UNDERSCORE + journalSpecInstanceId;
		if (type.equalsIgnoreCase(ontoChemExpVocabulary.getOntoKinReferenceJournalSpecification())) {
			try {
				iABoxManagement.createIndividual(ontoChemExpVocabulary.getOntoKinReferenceJournalSpecification(), pubInstance);
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getOntoKinReferencehasPublicationSpecification(), 
						currentBibliographyLinkInstance, pubInstance);
			} catch (ABoxManagementException e) {
				logger.error("PublicationSpecification could not be created.");
				e.printStackTrace();
			}
			
		}
		
		addPublicationInfo(publicationSpecification, pubInstance, type);
	}

	/**
	 * Creates data and meta data of a publication. 
	 * 
	 * @param publicationSpecification
	 * @param type
	 * @throws OntoChemExpException
	 */
	private void addPublicationInfo(PublicationSpecification publicationSpecification, String pubInstance, String type) throws OntoChemExpException{
		JournalSpecification journalSpec = (JournalSpecification) publicationSpecification;
		if(journalSpec!=null && type!=null && type.equalsIgnoreCase(ontoChemExpVocabulary.getOntoKinReferenceJournalSpecification())){
			try {
				// Adds the volume of the journal in which the current reference has been published
				iABoxManagement.addProperty(pubInstance, ontoChemExpVocabulary.getOntoKinReferenceJournalVolume(), Integer.toString(journalSpec.getVolume()), INTEGER);
				// Adds the page start of the reference in the journal
				iABoxManagement.addProperty(pubInstance, ontoChemExpVocabulary.getOntoKinReferenceDocumentPageStart(), Integer.toString(journalSpec.getPageStart()), INTEGER);		
				// Adds the page end of the reference in the journal
				iABoxManagement.addProperty(pubInstance, ontoChemExpVocabulary.getOntoKinReferenceDocumentPageEnd(), Integer.toString(journalSpec.getPageEnd()), INTEGER);
			} catch (ABoxManagementException e) {
				logger.error("Publication information could not be added.");
				e.printStackTrace();
			}
			
			addJournalInfo(journalSpec, pubInstance, type);
		}
	}

	/**
	 * Adds journal title and issn to the OWL representation.
	 * 
	 * @param journalSpec
	 * @param type
	 * @throws OntoChemExpException
	 */
	private void addJournalInfo(JournalSpecification journalSpec, String journalSpecInstance, String type) throws OntoChemExpException{
		String jourInstance = ontoChemExpVocabulary.getOntoKinReferenceJournal() + UNDERSCORE + journalInstanceId;
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getOntoKinReferenceJournal(), jourInstance);
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getOntoKinReferenceSpecifies(), journalSpecInstance, jourInstance);
		} catch (ABoxManagementException e) {
			logger.error("Journal instance could not be created.");
		}
		
		try {
			// Adds the title of the journal  
			iABoxManagement.addProperty(jourInstance, ontoChemExpVocabulary.getOntoKinReferenceTitle(), journalSpec.getJournal().getTitle(), STRING);
			// Adds the issn of the journal  
			iABoxManagement.addProperty(jourInstance, ontoChemExpVocabulary.getOntoKinReferenceISSN(), journalSpec.getJournal().getIssn(), STRING);
		} catch (ABoxManagementException e) {
			logger.error("Journal information could not be added.");
		}
	}
}
