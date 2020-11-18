package com.cmclinnovations.ontokin.model.owl;

import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.xml.sax.SAXException;
import com.cmclinnovations.ontokin.model.CtmlConverterState;
import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontokin.model.exception.OntoException;
import com.cmclinnovations.ontokin.model.utils.CtmlConverterUtils;

/**
 * Implements the method that forwards a call to those methods that
 * read reaction data and attributes from an in-memory temporary storage 
 * to pass them to the corresponding CTML to OWL conversion methods.
 * 
 * @author msff2
 *
 */
public class ReactionWriter extends CtmlConverter implements IReactionWriter{
	public void writer(char ch[], int start, int length) throws SAXException{
		writeCtmlReactionData(ch, start, length);
	}
	
	private void writeCtmlReactionData(char ch[], int start, int length) throws SAXException {
		if (reactionDataParseStatus.isReactionData()) {
			// Calls this method to write the reaction data
			// property values e.g. id
			writeReactionDataProperties();
			if (reactionParseStatus.isReaction()) {
				// Calls this method to write the reaction
				// properties of a mechanism e.g. name 
				writeReactionProperties(ch, start, length);
			}
		}
	}
	
	private void writeReactionDataProperties() {
		if (reactionDataParseStatus.isId()) {
			writeDataId();
			reactionDataParseStatus.setId(false);
		}
		if (reactionDataParseStatus.isCaseSensitive()) {
			writeCaseSensitiveInfo();
			reactionDataParseStatus.setCaseSensitive(false);
		}
	}
	
	private void writeReactionProperties(char ch[], int start, int length) {
		writeReactionAttributes();
		writeReactionElements(ch, start, length);
	}
	
	private void writeReactionAttributes(){
		// The reason of reading the reaction type property early is
		// to determine the exact reaction type to avoid duplicate
		// class codification to a reaction.
		writeTypeProperty();
		writeDuplicateInfoProperty();
		writeReversibleInfoProperty();
		writeLandauTellerProperty();
		writeNonConProperty();
		writePartialPressureProperty();
		writeSiteFracProperty();
		writeIdProperty();
	}
	
	private void writeReactionElements(char ch[], int start, int length){
		writeCommentProperty();
		writeEquationProperty(ch, start, length);
		writeOrderProperty(ch, start, length);
		writeRateCoefficient(ch, start, length);
		writeReactantsProperty(ch, start, length);
		writeProductsProperty(ch, start, length);
	}
	
	private void writeDuplicateInfoProperty(){
		if (reactionParseStatus.isDuplicate()) {
			writeDuplicateInfo();
			reactionParseStatus.setDuplicate(false);
		}
	}
	
	private void writeReversibleInfoProperty(){
		if (reactionParseStatus.isReversible()) {
			writeReversibleInfo();
			reactionParseStatus.setReversible(false);
		}
	}
	
	private void writeLandauTellerProperty(){
		if (reactionParseStatus.isLandauTeller()) {
			writeLandauTellerInfo();
			reactionParseStatus.setLandauTeller(false);
		}
	}
	
	private void writeTypeProperty(){
		if(!CtmlConverterState.createdReaction){
			reactionType = appConfigOntokin.getClassReaction();
			if (reactionParseStatus.isType()) {
				try{
				reactionType = CtmlConverterUtils.getReactionClass(reaction.getType());
				} catch(OntoException e){
					logger.error("Reaction type could not be identified.");
				}
				reactionParseStatus.setType(false);
			}
			writeReactionInstance();
			CtmlConverterState.createdReaction = true;
		}
	}
	
	private void writeNonConProperty(){
		if (reactionParseStatus.isNonCon()) {
			writeSiteConservingInfo();
			reactionParseStatus.setNonCon(false);
		}
	}
	
	private void writePartialPressureProperty(){
		if (reactionParseStatus.isPartialPressure()) {
			writePartialPressureInfo();
			reactionParseStatus.setPartialPressure(false);
		}
	}
	
	private void writeSiteFracProperty(){
		if (reactionParseStatus.isSiteFrac()) {
			writeSiteFractionInfo();
			reactionParseStatus.setSiteFrac(false);
		}
	}

	private void writeIdProperty(){
		if (reactionParseStatus.isId()) {
			writeReactionId();
			reactionParseStatus.setId(false);
		}
	}

	private void writeCommentProperty(){
		if (reactionParseStatus.isComment()) {
			// As this comment might be a multiline one, it is processed
			// at the CtmlConverter.endElement() method. Therefore,
			// reactionParseStatus.setComment() is set false, in that method
			// at the end of reading the value.
		}
	}

	private void writeEquationProperty(char ch[], int start, int length){
		if (reactionParseStatus.isEquation()) {
			// As equations cannot be read by calling normal element value 
			// value retrieval method, it is treated as a multiline value so
			// it is read at the CtmlConverter.endElement() method and
			// reactionParseStatus.setEquation() is set false, in that method
			// when the data retrieval and codification into OWL were over.
		}
	}

	private void writeOrderProperty(char ch[], int start, int length) {
		if (reactionOrderParseStatus.isOrder()) {
			if (!CtmlConverterState.createdReactionOrder) {
				// Creates an instance of reaction order once it finds the
				// appearance of the order element within a reaction.
				writeOrderInstance();
				CtmlConverterState.createdReactionOrder = true;
			}
			writeOrderProperties();
		}
	}
	
	public void writeRateCoefficient(char ch[], int start, int length) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			// Calls this method to write the Landau-Teller
			// rate coefficients of a reaction.
			writeLandauTellerProperties(ch, start, length);
			// Calls this method to write the third body efficiencies
			// of species, which take part in pressure dependent reaction.
			writeEfficiencyProperties(ch, start, length);
			// Calls this method to write the fall off model coefficients
			// of a reaction.
			writeFallOffProperties(ch, start, length);
		}
	}
	
	private void writeOrderProperties(){
		writeOrderDirection();
		writeSpeciesOrder();
	}
	
	public void writeReactionOrder(String order){
		writeOrderPropertyValue(order);
	}
	
	private void writeOrderDirection(){
		if (reactionOrderParseStatus.isDirection()) {
			writeOrderDirectionPropertyValue();
			reactionOrderParseStatus.setDirection(false);
		}	
	}
	
	private void writeSpeciesOrder(){
		if (reactionOrderParseStatus.isSpecies()) {
			writeSpeciesOrderPropertyValue();
			reactionOrderParseStatus.setSpecies(false);
		}	
	}
	
	public void decideArrheniusOrSticking(){
		if (arrhenius.getType() != null
				&& arrhenius.getType().equalsIgnoreCase(appConfigCtml
						.getReactionRateCoeffArrheniusStick())) {
			// Creates an instance of Sticking Coefficient Reaction 
			// once it finds the appearance of the arrhenius element
			// and also finds type="stick" within a reaction.
			writeStickingCoeffInstance();
			createdArrheniusArrhenius = false;
			createdArrheniusSticking = true;
		} else {
			// Creates an instance of Arrhenius Coefficient once it 
			// finds the appearance of the arrhenius element and does
			// not find type="stick" within a reaction.
			writeArrheniusInstance();
			createdArrheniusArrhenius = true;
			createdArrheniusSticking = false;
		}
	}
	
	private void writeLandauTellerProperties(char ch[], int start, int length){
		if (landauTellerParseStatus.isLandauTeller()) {
			if (!CtmlConverterState.createdLandauTeller) {
				// Creates an instance of Landau-Teller Coefficient once it finds the
				// appearance of the landauTeller element within a reaction.
				writeLandauTellerInstance();
				CtmlConverterState.createdLandauTeller = true;
			}
		}
	}
	
	private void writeEfficiencyProperties(char ch[], int start, int length){
		if (efficiencyParseStatus.isEfficiencies()) {
			writeAllEfficiencyProperties(ch, start, length);
			efficiencyParseStatus.setEfficiencies(false);
		}
	}
	
	private void writeFallOffProperties(char ch[], int start, int length){
		if (fallOffParseStatus.isFallOff()) {
			writeFallOffInstance();
			writeAllFallOffProperties();
			fallOffParseStatus.setFallOff(false);
		}
	}
	
	public void writeArrheniusProperties(String coefficientType, Long instanceId){
		writeNameProperty(coefficientType, instanceId);
		writeMotzWiseProperty(coefficientType, instanceId);
		writeSpeciesProperty(coefficientType, instanceId);
	}
	
	private void writeAllEfficiencyProperties(char ch[], int start, int length){
		writeSpeciesEfficiencies(ch, start, length);
		writeDefaultEfficiency();
	}
	
	private void writeAllFallOffProperties(){
		if(fallOffParseStatus.isType()){
			writeFallOffType();
			fallOffParseStatus.setType(false);
		}
		if(fallOffParseStatus.isNamedThirdBody()){
			writeNamedThirdBody();
			fallOffParseStatus.setNamedThirdBody(false);
		}
	}
	
	private void writeNameProperty(String coefficientType, Long instanceId) {
		if(arrheniusParseStatus.isName()){
			writeArrheniusName(coefficientType, instanceId);
			arrheniusParseStatus.setName(false);
		}
	}
	
	private void writeMotzWiseProperty(String coefficientType, Long instanceId) {
		if(arrheniusParseStatus.isMotzWise()){
			writeMotzWise(coefficientType, instanceId);
			arrheniusParseStatus.setMotzWise(false);
		}
	}
	
	private void writeSpeciesProperty(String coefficientType, Long instanceId) {
		if(arrheniusParseStatus.isSpecies()){
			writeSingleGasPhaseSpecies(coefficientType, instanceId);
			arrheniusParseStatus.setSpecies(false);
		}
	}
	
	public void writeCoverageDependecies(){
		if(coverageParseStatus.isCoverage()){
			// This setting is necessary to stop the reaction writer treating
			// Coverage.a and Coverage.e also as Arrhenius.A and Arrhenius.E. 
			arrheniusParseStatus.setA(false);
			arrheniusParseStatus.setE(false);			
			if (!CtmlConverterState.createdCoverageDependency) {
				// Creates an instance of Coverage Dependency Coefficient once 
				// it finds the appearance of the coverage element within 
				// a reaction.
				writeCoverageDependencyInstance();
				CtmlConverterState.createdCoverageDependency = true;
			}			
			// Writes the coverage coefficient parameters.
			writeCoverageCoefficients();
		}
	}

	private void writeCoverageCoefficients(){
		writeCoverageSpecies();
	}

	
	public void writeTminProperty(String tmin) {
		if(rateCoeffParseStatus.istMin()){
			// It is assumed that TMin appears as the first element in the list
			// of CHEB properties. Therefore, it uses TMin as an indicator of
			// of the presence of CHEB model and creates a CHEB instance when
			// TMin is found.
			writeChebInstance();
			writeTmin(tmin);
			writeTminUnitProperty();
			rateCoeffParseStatus.settMin(false);
		}
	}
	
	private void writeTminUnitProperty() {
		if(rateCoeffParseStatus.istMinUnits()){
			writeTminUnits();
			rateCoeffParseStatus.settMinUnits(false);
		}
	}
	
	public void writeTmaxProperty(String tmax) {
		if(rateCoeffParseStatus.istMax()){
			writeTmax(tmax);
			writeTmaxUnitProperty();
			rateCoeffParseStatus.settMax(false);
		}
	}
	
	private void writeTmaxUnitProperty() {
		if(rateCoeffParseStatus.istMaxUnits()){
			writeTmaxUnits();
			rateCoeffParseStatus.settMaxUnits(false);
		}
	}
	
	public void writePminProperty(String pmin) {
		if(rateCoeffParseStatus.ispMin()){
			writePmin(pmin);
			writePminUnitProperty();
			rateCoeffParseStatus.setpMin(false);
		}
	}
	
	private void writePminUnitProperty() {
		if(rateCoeffParseStatus.ispMinUnits()){
			writePminUnits();
			rateCoeffParseStatus.setpMinUnits(false);
		}
	}
	
	public void writePmaxProperty(String pmax) {
		if(rateCoeffParseStatus.ispMax()){
			writePmax(pmax);
			writePmaxUnitProperty();
			rateCoeffParseStatus.setpMax(false);
		}
	}
	
	private void writePmaxUnitProperty() {
		if(rateCoeffParseStatus.ispMaxUnits()){
			writePmaxUnits();
			rateCoeffParseStatus.setpMaxUnits(false);
		}
	}
	
	public void writeCoefficientA(String coeffA, String coefficientType, Long instanceId) {
		if(arrheniusParseStatus.isA()){
			writeA(coeffA, coefficientType, instanceId);
			if(arrheniusParseStatus.isAUnits()){
				writeAUnits(coefficientType, instanceId);
				arrheniusParseStatus.setAUnits(false);
			}
			arrheniusParseStatus.setA(false);
		}
	}
	
	public void writeCoefficientb(String coeffb, String coefficientType, Long instanceId) {
		if(arrheniusParseStatus.isB()){
			writeb(coeffb, coefficientType, instanceId);
			if(arrheniusParseStatus.isBUnits()){
				writebUnits(coefficientType, instanceId);
				arrheniusParseStatus.setBUnits(false);
			}
			arrheniusParseStatus.setB(false);
		}
	}
	
	public void writeCoefficientE(String coeffE, String coefficientType, Long instanceId) {
		if(arrheniusParseStatus.isE()){
			writeE(coeffE, coefficientType, instanceId);
			if(arrheniusParseStatus.isEUnits()){
				writeEUnits(coefficientType, instanceId);
				arrheniusParseStatus.setEUnits(false);
			}
			arrheniusParseStatus.setE(false);
		}
	}
	
	public void writeCoefficientP(String coeffP, String coefficientType, Long instanceId) {
		if(arrheniusParseStatus.isP()){
			writeP(coeffP, coefficientType, instanceId);
			if(arrheniusParseStatus.isPUnits()){
				writePUnits(coefficientType, instanceId);
				arrheniusParseStatus.setPUnits(false);
			}
			arrheniusParseStatus.setP(false);
		}
	}
	
	public void writeLandauTellerCoeffB(String coeffB) {
		if(landauTellerParseStatus.isB()){
			writeB(coeffB);
			if(landauTellerParseStatus.isBUnits()){
				writeBUnits();
				landauTellerParseStatus.setBUnits(false);
			}
			landauTellerParseStatus.setB(false);
		}
	}
	
	public void writeLandauTellerCoeffC(String coeffC) {
		if(landauTellerParseStatus.isC()){
			writeC(coeffC);
			if(landauTellerParseStatus.isCUnits()){
				writeCUnits();
				landauTellerParseStatus.setCUnits(false);
			}
			landauTellerParseStatus.setC(false);
		}
	}
	
	/**
	 * Originally written to codify the third body efficiency species. 
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeSpeciesEfficiencies(char ch[], int start, int length) {
		// As SAXParser fails to read the list of species for which the
		// thirdbody efficiencies are codified, it is handled with 
	}
		
	private void writeCoverageSpecies() {
		if(coverageParseStatus.isSpecies()){
			try{
				iOwlConstructWriter.addCoverageSpecies(basePath);
			}catch(OntoException e){
				logger.error("A coverage species for a reaction could not be "
						+ "created.");
			}
			arrheniusParseStatus.setSpecies(false);
		}
	}
	
	public void writeCoefficienta(String coeffa) {
		if(coverageParseStatus.isA()){
			writea(coeffa);
			if(coverageParseStatus.isaUnits()){
				writeaUnits();
				coverageParseStatus.setaUnits(false);
			}
			coverageParseStatus.setA(false);
		}
	}
	
	public void writeCoefficientm(String coeffm) {
		if(coverageParseStatus.isM()){
			writem(coeffm);
			if(coverageParseStatus.ismUnits()){
				writemUnits();
				coverageParseStatus.setmUnits(false);
			}
			coverageParseStatus.setM(false);
		}
	}
	
	public void writeCoefficiente(String coeffe) {
		if(coverageParseStatus.isE()){
			writee(coeffe);
			if(coverageParseStatus.iseUnits()){
				writeeUnits();
				coverageParseStatus.seteUnits(false);
			}
			coverageParseStatus.setE(false);
		}
	}
	
	private void writeReactantsProperty(char ch[], int start, int length){
		if (reactionParseStatus.isReactants()) {
			reactionParseStatus.setReactants(false);
		}
	}
	
	private void writeProductsProperty(char ch[], int start, int length){
		if (reactionParseStatus.isProducts()) {
			reactionParseStatus.setProducts(false);
		}
	}
	
	/**
	 * Writes the id of a group of reactions occurred in the context of 
	 * a phase in the mechanism being parsed.
	 */
	private void writeDataId() {
		try {
			iOwlConstructWriter.addReactionDataId(basePath, reactionData.getId());
		} catch (OntoException e) {
			logger.error("The id of the species belonging to pahse could not be created.");
		}
	}
	
	/**
	 * Writes if the reaction data is case sensitive or not.
	 */
	private void writeCaseSensitiveInfo() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, 
					appConfigOntokin.getReactionDataCaseSensitivity(),
					reactionData.getCaseSensitive(), 
					appConfigOntokin.getReactionMetadata(),
					reactionMetaDataInstanceId);
		} catch (OntoException e) {
			logger.error("The case sensitivity infomration of the "
					+ "reaction data could not be created.");
		}
	}

	/**
	 * Creates an instance of reaction in the mechanism OWL ontology being generated.
	 */
	private void writeReactionInstance() {
		try {
			iOwlConstructWriter.addReaction(basePath);
		} catch (OntoException e) {
			logger.error("An OWL instance for a reaction could not be created.");
		}
	}
	
	/**
	 * Writes in the OWL ontology if the given reaction is a duplicate one.
	 */
	private void writeDuplicateInfo() {
		try {
			iOwlConstructWriter.addDuplicateInfo(basePath, reaction.getDuplicate());
		} catch (OntoException e) {
			logger.error("The duplicate info of a reaction could not be created.");
		}
	}
	
	/**
	 * Writes the reversible info of a reaction in the mechanism OWL ontology
	 * being generated.
	 */
	private void writeReversibleInfo() {
		try {
			iOwlConstructWriter.addReversibleInfo(basePath, reaction.getReversible());
		} catch (OntoException e) {
			logger.error("The reversible info of a reaction could not be created.");
		}
	}
	
	/**
	 * Writes if the reaction is of type Landau-Teller.
	 */
	private void writeLandauTellerInfo() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getReactionLandauTeller(),
					reaction.getLandauTeller(),
					appConfigOntokin.getClassReaction().concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
							.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)));
		} catch (OntoException e) {
			logger.error("The Landau-Teller infomration of a reaction "
					+ "could not be created.");
		}
	}
	
	private void writeDefaultEfficiency() {
		try {
			if (efficiencies.getDefault() != null) {
				iOwlConstructWriter.addDefaultEfficiency(efficiencies.getDefault());
			}
		} catch (OntoException e) {
			logger.error("The default efficiency of a reaction could not " + "be created.");
		}
	}
	
	private void writeFallOffType() {
		try {
			if (fallOff.getType() != null) {
				iOwlConstructWriter.addFallOffType(basePath);
			}
		} catch (OntoException e) {
			logger.error("The fall of type of a reaction could not be created.");
		}
	}

	private void writeNamedThirdBody() {
		try {
			if (fallOff.getNamedThirdBody() != null) {
				iOwlConstructWriter.addNamedThirdBodySpecies(fallOff.getNamedThirdBody());
			}
		} catch (OntoException e) {
			logger.error("The named third body species of a Fall-off "
					+ "reaction could not be created.");
		}
	}	
	
	/**
	 * Writes if the reaction may not require to conserve site.
	 */
	private void writeSiteConservingInfo() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,appConfigOntokin.getReactionMayNotConserveSite(),
					 reaction.getNoncon(),
					appConfigOntokin.getClassReaction().concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
							.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)));
		} catch (OntoException e) {
			logger.error("The site conserving infomration of a reaction"
					+ " could not be created.");
		}
	}
	
	/**
	 * Adds the partial pressure information about the gas phase species 
	 * concentrations to a reaction.
	 */
	private void writePartialPressureInfo() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,appConfigOntokin.getConvertToPartialPressure(),
					 reaction.getPartialpressure(),
					appConfigOntokin.getClassReaction().concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
							.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)));
		} catch (OntoException e) {
			logger.error("The partial pressure infomration of a reactio could not be created.");
		}
	}
	
	/**
	 * Adds the partial pressure information about the gas phase species 
	 * concentrations to a reaction.
	 */
	private void writeSiteFractionInfo() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getConvertToSiteFraction(),
					reaction.getSitefrac(),
					appConfigOntokin.getClassReaction().concat(UNDERSCORE).concat(Long.toString(reactionInstanceId))
							.concat(UNDERSCORE).concat(Long.toString(reactionSerialNo)));
		} catch (OntoException e) {
			logger.error("The partial pressure infomration of a reactio could not be created.");
		}
	}
	
	/**
	 * Writes in the OWL ontology being generated the id of a reaction found 
	 * in the CTML file being parsed.
	 */
	private void writeReactionId() {
		try {
			iOwlConstructWriter.addReactionId(basePath, reaction.getId());
		} catch (OntoException e) {
			logger.error("The id of the species belonging to pahse could not be created.");
		}
	}
	
	/**
	 * Creates an instance of reaction order in the mechanism OWL ontology 
	 * being generated.
	 */
	private void writeOrderInstance() {
		try {
			iOwlConstructWriter.addReactionOrder(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for a reaction order could not be created.");
		}
	}
	
	/**
	 * Writes the order of a reaction for a species.
	 * 
	 * @param order the reaction order for a species
	 */
	private void writeOrderPropertyValue(String order) {
		if (order != null) {
			try {
				iOwlConstructWriter.addDataProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI(), order, appConfigOntokin
						.getReactionOrder().concat(UNDERSCORE).concat(Long.toString(reactionOrderInstanceId)));
			} catch (OntoException e) {
				logger.error("The order of a reaction could not be created.");
			}
		}
	}
	
	/**
	 * Adds the partial pressure information about the gas phase species 
	 * concentrations to a reaction.
	 */
	private void writeOrderDirectionPropertyValue() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getDataPropertyOrderDirection(), reactionOrder.getDirection(),
					appConfigOntokin.getReactionOrder(), reactionOrderInstanceId);
		} catch (OntoException e) {
			logger.error("The direction of a reaction could not be created.");
		}
	}
	
	/**
	 * Adds the partial pressure information about the gas phase species
	 * concentrations to a reaction.
	 */
	private void writeSpeciesOrderPropertyValue() {
		try {
			if(reactionOrder.getSpecies()!=null){
				iOwlConstructWriter.addReactionOrderSpecies(basePath);
			}
		} catch (OntoException e) {
			logger.error("A reaction order species could not be created.");
		}
	}
	
	/**
	 * Writes the name of Arrhenius rate coefficients if available.
	 * 
	 */
	private void writeArrheniusName(String coefficientType, Long instanceId) {
		if (arrhenius.getName() != null)
			try {
				iOwlConstructWriter.addArrheniusCoeffName(arrhenius.getName(), coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The name of the Arrhenius rate coefficients "
						+ "could not be created.");
			}
	}
	
	/**
	 * Writes if Motz-Wise correction is required to apply while 
	 * calculating the reaction rate of a sticking coefficient reaction.
	 */
	private void writeMotzWise(String coefficientType, Long instanceId) {
		if (arrhenius.getMotzWise() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasMotzWiseCorrection(), arrhenius.getMotzWise(),
						coefficientType.concat(UNDERSCORE).concat(Long.toString(instanceId)));
			} catch (OntoException e) {
				logger.error(
						"Whether or not the Arrhenius coefficient requires Motz-wise correction factor could not be created.");
			}
		}
	}
	
	/**
	 * Writes the single gas phase species that participates in a sticking
	 * coefficient reaction.
	 */
	private void writeSingleGasPhaseSpecies(String coefficientType, Long instanceId) {
		String speciesInstanceId = retreiveSpeciesInstanceId(arrhenius.getSpecies());
		if (speciesInstanceId != null) {
			try {
				iOwlConstructWriter.addObjectProperty(basePath, appConfigOntokin.getObjectPropertyHasSpecies(),
						coefficientType, appConfigOntokin.getClassSpecies(),
						coefficientType.concat(UNDERSCORE).concat(Long.toString(instanceId)),
						speciesInstanceId);
			} catch (OntoException e) {
				logger.error("The only gas phase species that acts as a reactant "
						+ "in a sticking coefficient reaction could " + "not be created.");
			}
		}
	}
	
	/**
	 * Retrieves the instance id of a species that is stored in a 
	 * species vs (unique) instance id hashmap. 
	 * 
	 * @param speciesName The name of a species.
	 * @return
	 */
	private String retreiveSpeciesInstanceId(String speciesName) {
		String speciesInstanceId = null;
		if (speciesName != null) {
			speciesInstanceId = speciesUniqueIDMap
					.get(speciesName.concat(UNDERSCORE).concat(appConfigCtml.getGasSpeciesDataId()));
		}
		return speciesInstanceId;
	}
	
	/**
	 * Writes the minimum temperature property for Chebyshev Polynomials' 
	 * coefficients.
	 * 
	 * @param tmin the minimum temperature below which the Chebyshev 
	 * parameterisation is invalid
	 */
	private void writeTmin(String tmin) {
		if (tmin != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyTempMin(), tmin,
						appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The minimum temperature for Chebyshev "
						+ "coefficients could not be created.");
			}
			writeTminUnits();
		}
	}
	
	/**
	 * Writes the minimum temperature units property for Chebyshev Polynomials' 
	 * coefficients.
	 * 
	 */
	private void writeTminUnits() {
		if (reactionTMin.getUnits() != null)
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyTempMinUnits(),
						reactionTMin.getUnits(), appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The units of the minimum temperature for "
						+ "Chebyshev coefficients could not be created.");
			}
	}

	/**
	 * Writes the maximum temperature property for Chebyshev Polynomials'
	 * coefficients.
	 * 
	 * @param the maximum temperature above which the Chebyshev parameterisation is invalid
	 */
	private void writeTmax(String tmax) {
		if (tmax != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyTempMax(), tmax,
						appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The maximum temperature for Chebyshev "
						+ "coefficients could not be created.");
			}
			writeTmaxUnits();
		}
	}

	/**
	 * Writes the maximum temperature units property for Chebyshev Polynomials'
	 * coefficients.
	 * 
	 */
	private void writeTmaxUnits() {
		if (reactionTMax.getUnits() != null)
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyTempMaxUnits(),
						reactionTMax.getUnits(), appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The units of the maximum temperature for "
						+ "Chebyshev coefficients could not be created.");
			}
	}

	/**
	 * Writes the maximum pressure property for Chebyshev Polynomials'
	 * coefficients.
	 *
	 * @param pmax the maximum pressure above which the Chebyshev parameterisation is invalid 
	 */
	private void writePmax(String pmax) {
		if (pmax != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyPressureMax(), pmax,
						appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The maximum pressure for Chebyshev coefficients "
						+ "could not be created.");
			}
			writePmaxUnits();
		}
	}

	/**
	 * Writes the maximum pressure units property for Chebyshev Polynomials'
	 * coefficients.
	 * 
	 */
	private void writePmaxUnits() {
		if (reactionPMax.getUnits() != null)
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyPressureMaxUnits(),
						reactionPMax.getUnits(), appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The units of the maximum pressure for "
						+ "Chebyshev coefficients could not be created.");
			}
	}
	
	/**
	 * Writes the minimum pressure property for Chebyshev Polynomials'
	 * coefficients.
	 * 
	 * @param pmin the minimum pressure below which the Chebyshev parameterisation is invalid
	 */
	private void writePmin(String pmin) {
		if (pmin != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyPressureMin(), pmin,
						appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The minimum pressure for Chebyshev coefficients "
						+ "could not be created.");
			}
			writePminUnits();
		}
	}

	/**
	 * Writes the minimum pressure units property for Chebyshev Polynomials'
	 * coefficients.
	 * 
	 */
	private void writePminUnits() {
		if (reactionPMin.getUnits() != null)
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyPressureMinUnits(),
						reactionPMin.getUnits(), appConfigOntokin.getClassCHEBCoefficient(), rateCoeffChebInstanceId);
			} catch (OntoException e) {
				logger.error("The units of the minimum pressure for "
						+ "Chebyshev coefficients could not be created.");
			}
	}
	
	/**
	 * Writes the pre-exponential factor for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param A Arrhenius coefficent A
	 * @param coefficientType
	 * @param instanceId
	 */
	private void writeA(String A, String coefficientType, Long instanceId) {
		if (A != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrCoeffA(), A,
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The pre-exponential factor for an Arrhenius "
						+ "rate constant computation could not be created.");
			}
		}
	}
	
	/**
	 * Writes the units of pre-exponential factor for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeAUnits(String coefficientType, Long instanceId) {
		if (arrheniusCoeffA.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrCoeffAUnits(), arrheniusCoeffA.getUnits(),
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The units of pre-exponential factor for an "
						+ "Arrhenius rate constant computation could not "
						+ "be created.");
			}
		}
	}
	
	/**
	 * Writes the temperature exponent for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param b Arrhenius coefficient b
	 * @param coefficientType
	 * @param instanceId
	 */
	private void writeb(String b, String coefficientType, Long instanceId) {
		if (b != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrCoeffb(), b,
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The temperature exponent for an Arrhenius "
						+ "rate constant computation could not be created.");
			}
		}
	}
	
	/**
	 * Writes the units of temerature exponent for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writebUnits(String coefficientType, Long instanceId) {
		if (arrheniusCoeffB.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrCoeffbUnits(), arrheniusCoeffB.getUnits(),
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The units of temperature exponent for an "
						+ "Arrhenius rate constant computation could not "
						+ "be created.");
			}
		}
	}
	
	/**
	 * Writes the activation energy for an Arrhenius rate constant computation.
	 * 
	 * @param E Arrhenius coefficient E
	 * @param coefficientType
	 * @param instanceId
	 */
	private void writeE(String E, String coefficientType, Long instanceId) {
		if (E != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrCoeffE(), E,
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The activation energy for an Arrhenius "
						+ "rate constant computation could not be created.");
			}
		}
	}
	
	/**
	 * Writes the units of activation energy for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeEUnits(String coefficientType, Long instanceId) {
		if (arrheniusCoeffE.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrCoeffEUnits(), arrheniusCoeffE.getUnits(),
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The units of activation energy for an "
						+ "Arrhenius rate constant computation could not "
						+ "be created.");
			}
		}
	}
	
	/**
	 * Writes the reference pressure for an Arrhenius rate 
	 * constant computation.
	 *  
	 * @param P the reference pressure
	 * @param coefficientType
	 * @param instanceId
	 */
	private void writeP(String P, String coefficientType, Long instanceId) {
		if (P != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrRefPressure(), P,
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The reference pressure for an Arrhenius "
						+ "rate constant computation could not be created.");
			}
		}
	}
	
	/**
	 * Writes the units of reference pressure for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writePUnits(String coefficientType, Long instanceId) {
		if (arrheniusCoeffP.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasArrRefPressureUnits(), arrheniusCoeffP.getUnits(),
						coefficientType, instanceId);
			} catch (OntoException e) {
				logger.error("The units of activation energy for an "
						+ "Arrhenius rate constant computation could not "
						+ "be created.");
			}
		}
	}

	/**
	 * Writes the pre-exponential coverage modifier for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param a coverage modifier a
	 */
	private void writea(String a) {
		if (a != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasCovDepA(), a,
						appConfigOntokin.getClassCoverageCoefficient(), rateCoeffCovDepInstanceId);
			} catch (OntoException e) {
				logger.error("The pre-exponential coverage modifier for "
						+ "an Arrhenius rate constant computation could "
						+ "not be created.");
			}
		}
	}
	
	/**
	 * Writes the units of pre-exponential coverage modifier for an 
	 * Arrhenius rate constant computation.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeaUnits() {
		if (coverageA.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasCovDepAUnits(), coverageA.getUnits(),
						appConfigOntokin.getClassCoverageCoefficient(), rateCoeffCovDepInstanceId);
			} catch (OntoException e) {
				logger.error("The pre-exponential coverage modifier for "
						+ "an Arrhenius rate constant computation could "
						+ "not be created.");
			}
		}
	}
	
	/**
	 * Writes the reaction order coverage modifier for an Arrhenius rate 
	 * constant computation.
	 * 
	 * @param m coverage modifier m
	 */
	private void writem(String m) {
		if (m != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasCovDepM(), m,
						appConfigOntokin.getClassCoverageCoefficient(), rateCoeffCovDepInstanceId);
			} catch (OntoException e) {
				logger.error("The reaction order coverage modifier for "
						+ "an Arrhenius rate constant computation could "
						+ "not be created.");
			}
		}
	}
	
	/**
	 * Writes the units of reaction order coverage modifier for an 
	 * Arrhenius rate constant computation.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writemUnits() {
		if (coverageA.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasCovDepMUnits(), coverageM.getUnits(),
						appConfigOntokin.getClassCoverageCoefficient(), rateCoeffCovDepInstanceId);
			} catch (OntoException e) {
				logger.error("The units of reaction order coverage modifier "
						+ "for an Arrhenius rate constant computation could "
						+ "not be created.");
			}
		}
	}
	
	/**
	 * Writes the exponential coverage modifier for an Arrhenius rate 
	 * constant computation.
	 *  
	 * @param e coverage modifier
	 */
	private void writee(String e) {
		if (e != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasCovDepE(), e,
						appConfigOntokin.getClassCoverageCoefficient(), rateCoeffCovDepInstanceId);
			} catch (OntoException oe) {
				logger.error("The reaction order coverage modifier for "
						+ "an Arrhenius rate constant computation could "
						+ "not be created.");
			}
		}
	}
	
	/**
	 * Writes the units of exponential coverage modifier for an 
	 * Arrhenius rate constant computation.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeeUnits() {
		if (coverageE.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasCovDepEUnits(), coverageE.getUnits(),
						appConfigOntokin.getClassCoverageCoefficient(), rateCoeffCovDepInstanceId);
			} catch (OntoException e) {
				logger.error("The units of rexponential coverage modifier "
						+ "for an Arrhenius rate constant computation could "
						+ "not be created.");
			}
		}
	}

	/**
	 * Writes the Landu-Teller coefficient of the term T^(-1/3).
	 * 
	 * @param B a Landau-Teller coefficient
	 */
	private void writeB(String B) {
		if (B != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasLanTellerCoeffB(), B,
						appConfigOntokin.getClassLandauTellerCoefficient(), rateCoeffLanTellerInstanceId);
			} catch (OntoException e) {
				logger.error("The Landau-Teller coefficient of the term "
						+ "T^(-1/3) for reaction rate constant computation "
						+ "could not be created.");
			}
		}
	}
	
	/**
	 *  Writes the units of Landu-Teller coefficient of the term T^(-1/3).
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeBUnits() {
		if (landauTellerB.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasLanTellerCoeffBUnits(), landauTellerB.getUnits(),
						appConfigOntokin.getClassLandauTellerCoefficient(), rateCoeffLanTellerInstanceId);
			} catch (OntoException e) {
				logger.error("The units of Landau-Teller coefficient of the "
						+ "term T^(-1/3) for the reaction rate constant "
						+ "computation could not be created.");
			}
		}
	}

	/**
	 * Writes the Landu-Teller coefficient of the term T^(-2/3).
	 * 
	 * @param C a Landau-Teller coefficient
	 */
	private void writeC(String C) {
		if (C != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasLanTellerCoeffC(), C,
						appConfigOntokin.getClassLandauTellerCoefficient(), rateCoeffLanTellerInstanceId);
			} catch (OntoException e) {
				logger.error("The Landau-Teller coefficient of the term "
						+ "T^(-2/3) for the reaction rate constant computation "
						+ "could not be created.");
			}
		}
	}
	
	/**
	 *  Writes the units of Landu-Teller coefficient of the term T^(-2/3).
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void writeCUnits() {
		if (landauTellerC.getUnits() != null) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH,
						appConfigOntokin.getDataPropertyHasLanTellerCoeffCUnits(), landauTellerC.getUnits(),
						appConfigOntokin.getClassLandauTellerCoefficient(), rateCoeffLanTellerInstanceId);
			} catch (OntoException e) {
				logger.error("The units of Landau-Teller coefficient of the "
						+ "term T^(-2/3) for reaction rate constant "
						+ "computation could not be created.");
			}
		}
	}

	
	/**
	 * Creates an instance of Arrhenius Rate Coefficients in the mechanism 
	 * OWL ontology being generated.
	 */
	private void writeArrheniusInstance() {
		try {
			iOwlConstructWriter.addArrheniusCoefficient(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for Arrhenius rate "
					+ "coefficients could not be created.");
		}
	}
	
	/**
	 * Creates an instance of Sticking Coefficient Reaction in the mechanism 
	 * OWL ontology being generated.
	 */
	private void writeStickingCoeffInstance() {
		try {
			iOwlConstructWriter.addStickingCoefficient(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for Sticking "
					+ "coefficient could not be created.");
		}
	}
	
	/**
	 * Creates an instance of Landau-Teller Rate Coefficients in the mechanism 
	 * OWL ontology being generated.
	 */
	private void writeLandauTellerInstance() {
		try {
			iOwlConstructWriter.addLandauTellerCoefficient(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for Landau-Teller rate "
					+ "coefficients could not be created.");
		}
	}
	
	/**
	 * Creates an instance of Fall-off Rate Coefficients in the mechanism 
	 * OWL ontology being generated.
	 */
	private void writeFallOffInstance() {
		try {
			iOwlConstructWriter.addFallOffInstance(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for Arrhenius rate "
					+ "coefficients could not be created.");
		}
	}

	/**
	 * Creates the data property and value of Fall-off Model Coefficients 
	 * in the mechanism OWL ontology being generated.
	 * 
	 * @param fallOffModeldata data containing 3 or 5 parameters of a Fall-off model
	 */
	public void writeFallOffModelData(String fallOffModeldata) {
		if (fallOffModeldata != null && !fallOffModeldata.trim().isEmpty()) {
			try {
				iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyHasCoeffValues(),
						fallOffModeldata, appConfigOntokin.getClassFallOffModelCoefficient(),
						rateCoeffFallOffInstanceId);
				writeNumberOfFallOffCoefficients(fallOffModeldata);
			} catch (OntoException e) {
				logger.error("Fall-off coefficients could not be created.");
			}
		}
	}

	/**
	 * Creates the number of coefficients in the current Fall-off model.
	 * 
	 * @param fallOffModeldata
	 */
	private void writeNumberOfFallOffCoefficients(String fallOffModeldata){
		int numberOfCoefficients = 0;
		if(fallOffModeldata.contains(SPACE)){
			numberOfCoefficients = fallOffModeldata.split(SPACE).length;
		}
		try{
		iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getOntokinHasNumberOfCoefficients(),
				Integer.toString(numberOfCoefficients), appConfigOntokin.getClassFallOffModelCoefficient(),
				rateCoeffFallOffInstanceId);
		}catch(OntoException e){
			logger.error("Number of Fall-off coefficients could not be created.");
		}
	}
	
	/**
	 * Creates an instance of Coverge Dependency Rate Coefficients in the mechanism 
	 * OWL ontology being generated.
	 */
	private void writeCoverageDependencyInstance() {
		try {
			iOwlConstructWriter.addCovDependencyCoefficient(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for Coverage Dependency "
					+ "rate coefficients could not be created.");
		}
	}
	
	/**
	 * Creates an instance of Chebyshev Rate Coefficients in the mechanism 
	 * OWL ontology being generated.
	 */
	public void writeChebInstance() {
		try {
			iOwlConstructWriter.addChebCoefficient(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for Chebyshev rate "
					+ "coefficients could not be created.");
		}
	}

}
