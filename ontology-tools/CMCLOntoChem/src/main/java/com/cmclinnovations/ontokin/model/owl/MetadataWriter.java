package com.cmclinnovations.ontokin.model.owl;

import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontokin.model.exception.OntoException;
import com.cmclinnovations.ontokin.model.utils.CtmlConverterUtils;

/**
 * Implements the method that forwards a call to those methods that
 * read the CTML metadata from an in-memory temporary storage 
 * to pass them to the corresponding CTML to OWL conversion methods.
 * 
 * 
 * @author msff2
 *
 */
public class MetadataWriter extends CtmlConverter implements IMetadataWriter{

	public void writer(char ch[], int start, int length) throws SAXException{
		readCtmlMetaDataElement(ch, start, length);
	}
	
	/**
	 * Forwards the call to the methods that read CTML meta data
	 * i.e. CTML version, commit, comment and validation info
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readCtmlMetaDataElement(char ch[], int start, int length) throws SAXException {
		if (ctmlMDParseStatus.isCtml()) {
			// Verifies if there is a need to create a mechanism. This has
			// implemented to avoid creating another instance of the same
			// mechanism in the next iteration of the parser through class.
			if(needsToCreateMechanism){
				// Calls this method to create an OWL instance of the chemical 
				// mechanism being parsed.
				createMechanism();
				createMechanismName();
				needsToCreateMechanism = false;
			}
			if (ctmlMDParseStatus.isCtmlCmclVersion()) {
				// Calls this method to read the CTML version info
				readCtmlVersion();
			}

			if (ctmlMDParseStatus.isCtmlCommit()) {
				// Calls this method to read the CTML commit info
				readCtmlCommit();
			}

			if (ctmlCommentParseStatus.isComment()) {
				// Calls this method to read the CTML comment
				readCtmlComment(ch, start, length);
			}
			if (validateParseStatus.isValidate()) {
				// Calls this method to read if the species
				// of the mechanism which is being imported
				// requires validation
				readCtmlValidate();
				ctmlMDParseStatus.setCtml(false);
			}
		}
	}

	/**
	 * Following the detection of the start of a CTML tag, it creates 
	 * an instance of mechanism in OWL.
	 * 
	 */
	private void createMechanism() {
		try {
			iOwlConstructWriter.createMechanism();
		} catch (OntoException e) {
			logger.error(
					"The version number of the CMCL parser used to encode a CTML mechanism file could not be created.");
		}
	}

	/**
	 * Following the creation of the mechanism it creates the name of it.
	 * 
	 */
	private void createMechanismName() {
		try {
			// Adds the name of the mechanism
			iOwlConstructWriter.addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), CtmlConverterUtils.mechanismName(mechanismName), appConfigOntokin
					.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)));
		} catch (OntoException e) {
			logger.error(
					"The version number of the CMCL parser used to encode a CTML mechanism file could not be created.");
		}
	}
	
	/**
	 * Reads the CTML version meta data and adds it 
	 * to the ontology bering created.
	 * 
	 */
	private void readCtmlVersion() {
		try {
			iOwlConstructWriter.addDataProperty(OWLRDFVocabulary.OWL_VERSION_INFO.getIRI(),
					ctmlMD.getCmclVersion(), appConfigOntokin.getCtml());
		} catch (OntoException e) {
			logger.error(
					"The version number of the CMCL parser used to encode a CTML mechanism file could not be created.");
		}
		ctmlMDParseStatus.setCtmlCmclVersion(false);
	}

	/**
	 * Reads the CTML commit meta data
	 * 
	 */
	private void readCtmlCommit() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getCtmlCommit(), ctmlMD.getCommit(), appConfigOntokin.getCtml());
		} catch (OntoException e) {
			logger.error("The commit information could not be created.");
		}
		ctmlMDParseStatus.setCtmlCommit(false);
	}

	/**
	 * Reads the CTML comment meta data
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readCtmlComment(char ch[], int start, int length) {
	}

	/**
	 * Forwards the call to the methods that check if the
	 * species and reactions appearing in the mechanism need to
	 * be validated
	 * 
	 */
	private void readCtmlValidate() {
		if (validateParseStatus.isSpeciesToBeValidated()) {
			readCtmlSpeciesValidate();
			validateParseStatus.setSpeciesToBeValidated(false);
		}
		if (validateParseStatus.isReactionsToBeValidated()) {
			readCtmlReactionsValidate();
			validateParseStatus.setReactionsToBeValidated(false);
		}
	}

	/**
	 * Reads the information from the CTML that tells if the
	 * species appearing in the mechanism requires a validation
	 */
	private void readCtmlSpeciesValidate() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getCtmlValidateSpecies(),
					validateMD.getSpecies(), appConfigOntokin.getOntokinMechanism().concat(UNDERSCORE)
							.concat(Long.toString(mechanismInstanceId)));
		} catch (OntoException e) {
			logger.error("Species validation metadata could not be created.");
		}
	}

	/**
	 * Reads the information from the CTML that tells if the
	 * reactions appearing in the mechanism requires a
	 * validation
	 */
	private void readCtmlReactionsValidate() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getCtmlValidateReactions(), validateMD.getReactions(),
					appConfigOntokin.getOntokinMechanism().concat(UNDERSCORE)
					.concat(Long.toString(mechanismInstanceId)));
		} catch (OntoException e) {
			logger.error("Reaction validation metadata could not be created.");
		}
	}

}
