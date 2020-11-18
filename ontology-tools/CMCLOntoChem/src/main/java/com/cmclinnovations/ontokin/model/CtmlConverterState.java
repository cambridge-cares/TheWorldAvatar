package com.cmclinnovations.ontokin.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.springframework.context.ApplicationContext;

import com.cmclinnovations.ontokin.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontokin.model.configuration.CtmlVocabulary;
import com.cmclinnovations.ontokin.model.configuration.OntoKinKB;
import com.cmclinnovations.ontokin.model.configuration.OntoKinVocabulary;
import com.cmclinnovations.ontokin.model.converter.ctml.IElementConverter;
import com.cmclinnovations.ontokin.model.converter.ctml.IMetadataConverter;
import com.cmclinnovations.ontokin.model.converter.ctml.IPhaseConverter;
import com.cmclinnovations.ontokin.model.converter.ctml.IReactionConverter;
import com.cmclinnovations.ontokin.model.converter.ctml.ISpeciesConverter;
import com.cmclinnovations.ontokin.model.converter.owl.query.sparql.IElementQuery;
import com.cmclinnovations.ontokin.model.converter.owl.query.sparql.IMetadataQuery;
import com.cmclinnovations.ontokin.model.converter.owl.query.sparql.IPhaseQuery;
import com.cmclinnovations.ontokin.model.converter.owl.query.sparql.IReactionQuery;
import com.cmclinnovations.ontokin.model.converter.owl.query.sparql.ISpeciesQuery;
import com.cmclinnovations.ontokin.model.data.structure.ctml.Ctml;
import com.cmclinnovations.ontokin.model.data.structure.ctml.CtmlComment;
import com.cmclinnovations.ontokin.model.data.structure.ctml.Validate;
import com.cmclinnovations.ontokin.model.data.structure.ctml.element.Element;
import com.cmclinnovations.ontokin.model.data.structure.ctml.element.ElementData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.ElementArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Kinetics;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Phase;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.PhaseArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.ReactionArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.SpeciesArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.State;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Thermo;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Transport;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.thermo.SiteDensity;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Arrhenius;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientE;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientP;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientb;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Coverage;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterE;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterM;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Efficiencies;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.FallOff;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTeller;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTellerCoefficientB;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTellerCoefficientC;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.RateCoefficient;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Reaction;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ReactionData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ReactionOrder;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMin;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.RateCoeffFloatArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMin;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Density;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.DipoleMoment;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.FloatArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.LJDiameter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.LJWellDepth;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.NASA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Polarizability;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.RotRelax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.STRING;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Size;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Species;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesThermo;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesTransport;
import com.cmclinnovations.ontokin.model.owl.IElementWriter;
import com.cmclinnovations.ontokin.model.owl.IMetadataWriter;
import com.cmclinnovations.ontokin.model.owl.IOwlConstructWriter;
import com.cmclinnovations.ontokin.model.owl.IPhaseWriter;
import com.cmclinnovations.ontokin.model.owl.IRateCoefficientWriter;
import com.cmclinnovations.ontokin.model.owl.IReactionWriter;
import com.cmclinnovations.ontokin.model.owl.ISpeciesWriter;
import com.cmclinnovations.ontokin.model.parse.status.ctml.CtmlCommentParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.ElementDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.MetaDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.PhaseParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.ReactionDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.SpeciesDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.ValidateParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.elementdata.ElementParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.ElementArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.KineticsParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.PhaseArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.ReactionArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.SpeciesArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.StateParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.ThermoParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.TransportParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.thermo.SiteDensityParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.ReactionParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.RateCoefficientParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ReactionOrderParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.ArrheniusParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.CoverageParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.EfficiencyParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.FallOffParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.LandauTellerParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.RateCoeffArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.SpeciesParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.DensityParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.SizeParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.SpeciesThermoParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.SpeciesTransportParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.nasa.CoefficientArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.nasa.NASAPolynomialParseStatus;
import com.cmclinnovations.ontokin.model.parser.ctml.ICtmlCommentParser;
import com.cmclinnovations.ontokin.model.parser.ctml.IElementDataParser;
import com.cmclinnovations.ontokin.model.parser.ctml.IMetadataParser;
import com.cmclinnovations.ontokin.model.parser.ctml.IPhaseParser;
import com.cmclinnovations.ontokin.model.parser.ctml.IReactionDataParser;
import com.cmclinnovations.ontokin.model.parser.ctml.ISpeciesDataParser;
import com.cmclinnovations.ontokin.model.parser.ctml.IValidateParser;
import com.cmclinnovations.ontokin.model.parser.ctml.elementdata.IElementParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.IElementArrayParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.IKineticsParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.IPhaseArrayParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.IPhaseThermoParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.IReactionArrayParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.ISpeciesArrayParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.IStateParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.ITransportParser;
import com.cmclinnovations.ontokin.model.parser.ctml.phase.thermo.ISiteDensityParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.IReactionParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IArrheniusParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.ICoverageParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IEfficiencyParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IFallOffParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.ILandauTellerParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IRateCoeffArrayParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IRateCoeffPressureMaxParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IRateCoeffPressureMinParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IRateCoeffTempMaxParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IRateCoeffTempMinParser;
import com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction.IReactionOrderParser;
import com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.ISpeciesParser;
import com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species.IDensityParser;
import com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species.ISizeParser;
import com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species.ISpeciesThermoParser;
import com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species.ISpeciesTransportParser;
import com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species.nasa.ICoefficentArrayParser;
import com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species.nasa.INASAPolynomialParser;

import de.derivo.sparqldlapi.QueryEngine;

/**
 * This class maintains the current state of all the member variables
 * used in the CTML to OWL conversion.
 * 
 * @author msff2
 *
 */
public class CtmlConverterState {

	public static ApplicationContext applicationContext;
	public static CtmlVocabulary appConfigCtml;
	public static AppConfigOperationControl opCtrl;
	public static OntoKinVocabulary appConfigOntokin;
	public static OntoKinKB ontoKinKB;
	
	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if the CTML element and attributes have already been parsed.
	public static MetaDataParseStatus ctmlMDParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML parser methods.
	public static IMetadataParser iCtmlMDParser;

	// A member variable created to hold the instance of the class that
	// can store the CTML metadata. These metadata are stored to be
	// codified in the mechanism OWL ontology that is being created.
	public static Ctml ctmlMD;

	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if the CTML comment and and material attribute have already been parsed.
	public static CtmlCommentParseStatus ctmlCommentParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML comment parser methods.
	public static ICtmlCommentParser iCtmlCommentParser;

	// A member variable created to hold the instance of the class that
	// can store the CTML material and its comment. These metadata are stored to be
	// codified in the mechanism OWL ontology that is being created.
	public static CtmlComment ctmlComment;
	
	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if the CTML validate element and attributes have already been parsed.
	public static ValidateParseStatus validateParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML Validate parser methods.
	public static IValidateParser iValidateParser;

	// A member variable created to hold the instance of the class that
	// can store the Validate tag metadata. These metadata are stored to be
	// codified in the mechanism OWL ontology that is being created.
	public static Validate validateMD;

	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if a CTML Phase element and attributes have already been parsed.
	public static PhaseParseStatus phaseParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML Validate parser methods.
	public static IPhaseParser iPhaseParser;

	// A member variable created to hold the instance of the class that
	// can store the Phase metadata. These metadata are stored to be
	// codified in the mechanism OWL ontology that is being created.
	public static Phase phaseMD;

	// A member variable created to hold the instance of the class that
	// can store the element array metadata. These metadata are stored to be
	// codified in the mechanism OWL ontology that is being created.
	public static ElementArray elementArray;

	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if the CTML element array element and attributes have already
	// been parsed.
	public static ElementArrayParseStatus elementArrayParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML ElementArray parser methods.
	public static IElementArrayParser iElementArrayParser;

	// A member variable created to hold the instance of the class that
	// can store the species array metadata. These metadata are stored to be
	// codified in the mechanism OWL ontology that is being created.
	public static SpeciesArray speciesArray;

	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if the CTML species array element and attributes have already
	// been parsed.
	public static SpeciesArrayParseStatus speciesArrayParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML species array parser methods.
	public static ISpeciesArrayParser iSpeciesArrayParser;

	// A member variable created to hold the instance of the class that
	// can store the reaction array metadata. These metadata are stored to be
	// codified in the mechanism OWL ontology that is being created.
	public static ReactionArray reactionArray;

	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if the CTML reaction array element and attributes have already
	// been parsed.
	public static ReactionArrayParseStatus reactionArrayParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML reaction array parser methods.
	public static IReactionArrayParser iReactionArrayParser;

	// A member variable created to hold the instance of the class that
	// can store the phase state information if available. This information
	// is codified in the mechanism OWL ontology that is being created.
	public static State phaseState;

	// A member variable created to hold the instance of the class that
	// maintains a link to the getter and setter of the flag that indicates
	// if the CTML phase state element has already been parsed.
	public static StateParseStatus phaseStateParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML phase state parser methods.
	public static IStateParser iPhaseStateParser;

	// A member variable created to hold the instance of the class that
	// can store the thermodynamic property model. This information
	// is codified in the mechanism OWL ontology that is being created.
	public static Thermo thermoProperty;

	// A member variable created to hold the instance of the class that
	// maintains a link to the getter and setter of the flag that indicates
	// if the CTML thermodynamic property model has already been parsed.
	public static ThermoParseStatus thermoParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML thermodynamic property model parser methods.
	public static IPhaseThermoParser iThermoParser;

	// A member variable created to hold the instance of the class that
	// can store the site density. This information is codified in the
	// mechanism OWL ontology that is being created.
	public static SiteDensity siteDensity;

	// A member variable created to hold the instance of the class that
	// maintains a link to the getters and setters of the flags that indicate
	// if the CTML phase site density has already been parsed.
	public static SiteDensityParseStatus siteDensityParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML phase site density parser methods.
	public static ISiteDensityParser iSiteDensityParser;
	
	// A member variable created to hold the instance of the class that
	// can store the kinetics of a phase. This information is codified in the
	// mechanism OWL ontology that is being created.
	public static Kinetics kinetics;

	// A member variable created to hold the instance of the class that
	// maintains a link to the getters and setters of the flags that indicate
	// if the CTML phase kinetics has already been parsed.
	public static KineticsParseStatus kineticsParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML kinetics parser methods.
	public static IKineticsParser iKineticsParser;	
	
	// A member variable created to hold the instance of the class that
	// can store the transport property model. This information
	// is codified in the mechanism OWL ontology that is being created.
	public static Transport transportProperty;

	// A member variable created to hold the instance of the class that
	// maintains a link to the getter and setter of the flag that indicates
	// if the CTML transport property model has already been parsed.
	public static TransportParseStatus transportParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML transport property model parser methods.
	public static ITransportParser iTransportParser;

	// A member variable created to hold the instance of the class that
	// can store the phase array. This information
	// is codified in the mechanism OWL ontology that is being created.
	public static PhaseArray phaseArray;

	// A member variable created to hold the instance of the class that
	// maintains a link to the getter and setter of the flag that indicates
	// if the CTML phase array have already been parsed.
	public static PhaseArrayParseStatus phaseArrayParseStatus;

	// A member variable created to hold the instance of the class that
	// includes the CTML phase array parser methods.
	public static IPhaseArrayParser iPhaseArrayParser;

	public static ElementDataParseStatus elementDataParseStatus;
	
	public static IElementDataParser iElementDataParser;

	public static ElementData elementData;

	public static ElementParseStatus elementParseStatus;
	
	public static IElementParser iElementParser;

	public static Element elementDataElement;
	
	public static SpeciesDataParseStatus speciesDataParseStatus;
	
	public static ISpeciesDataParser iSpeciesDataParser;

	public static SpeciesData speciesData;
	
	public static SpeciesParseStatus speciesParseStatus;
	
	public static ISpeciesParser iSpeciesParser;

	public static Species species;
	
	public static DensityParseStatus densityParseStatus;
	
	public static IDensityParser iSpeciesDensityParser;

	public static Density speciesDensity;	
	
	public static SizeParseStatus speciesSizeParseStatus;
	
	public static ISizeParser iSpeciesSizeParser;

	public static Size speciesSize;
	
	public static SpeciesThermoParseStatus speciesThermoParseStatus;
	
	public static ISpeciesThermoParser iSpeciesThermoParser;

	public static SpeciesThermo speciesThermo;
	
	public static NASAPolynomialParseStatus nasaPolyParseStatus;
	
	public static INASAPolynomialParser iNasaPolyParser;

	public static NASA nasa;
	
	public static CoefficientArrayParseStatus coeffArrayParseStatus;
	
	public static ICoefficentArrayParser iCoeffArrayParser;

	public static FloatArray coeffArray;
	
	public static SpeciesTransportParseStatus speciesTransportParseStatus;
	
	public static ISpeciesTransportParser iSpeciesTransportParser;

	public static SpeciesTransport speciesTransport;
	
	public static STRING speciesTxString;
	
	public static LJWellDepth speciesTxLJWellDepth;
	
	public static LJDiameter speciesTxLJDiameter;
	
	public static DipoleMoment speciesTxDipoleMoment;
	
	public static Polarizability speciesTxPolarizability;
	
	public static RotRelax speciesTxRotRelax;
	
	public static ReactionDataParseStatus reactionDataParseStatus;
	
	public static IReactionDataParser iReactionDataParser;

	public static ReactionData reactionData;
	
	public static ReactionParseStatus reactionParseStatus;
	
	public static IReactionParser iReactionParser;

	public static Reaction reaction;
	
	public static ReactionOrderParseStatus reactionOrderParseStatus;
	public static IReactionOrderParser iReactionOrderParser;
	public static ReactionOrder reactionOrder;
	
	public static RateCoefficientParseStatus rateCoeffParseStatus;
	public static RateCoefficient rateCoefficient;
	
	public static RateCoeffArrayParseStatus rateCoeffArrayParseStatus;
	public static IRateCoeffArrayParser iRateCoeffArrayParser;
	public static RateCoeffFloatArray rateCoeffArray;
	
	public static IRateCoeffTempMinParser iRateCoeffTMinParser;
	public static TMin reactionTMin;
	
	public static IRateCoeffTempMaxParser iRateCoeffTMaxParser;
	public static TMax reactionTMax;
	
	public static IRateCoeffPressureMinParser iRateCoeffPMinParser;
	public static PMin reactionPMin;
	
	public static IRateCoeffPressureMaxParser iRateCoeffPMaxParser;
	public static PMax reactionPMax;

	public static ArrheniusParseStatus arrheniusParseStatus;
	public static IArrheniusParser iArrheniusParser;
	public static Arrhenius arrhenius;
	
	public static ArrheniusCoefficientA arrheniusCoeffA;
	
	public static ArrheniusCoefficientb arrheniusCoeffB;
	
	public static ArrheniusCoefficientE arrheniusCoeffE;
	
	public static ArrheniusCoefficientP arrheniusCoeffP;
	
	public static CoverageParseStatus coverageParseStatus;
	public static ICoverageParser iCoverageParser;
	public static Coverage coverage;
	
	public static CoverageParameterA coverageA;
	
	public static CoverageParameterE coverageE;
	
	public static CoverageParameterM coverageM;
	
	public static LandauTellerParseStatus landauTellerParseStatus;
	public static ILandauTellerParser iLandauTellerParser;
	public static LandauTeller landauTeller;
	
	public static LandauTellerCoefficientB landauTellerB;
	
	public static LandauTellerCoefficientC landauTellerC;

	public static FallOffParseStatus fallOffParseStatus;
	public static IFallOffParser iFallOffParser;
	public static FallOff fallOff;
	
	public static EfficiencyParseStatus efficiencyParseStatus;
	public static IEfficiencyParser iEfficiencyParser;
	public static Efficiencies efficiencies;
	
	// Declared to serve as the id for instances of the Phase class.
	public static long phaseInstanceId = System.nanoTime();

	// Declared to serve as the id for the instance of the 
	// Reaction Mechanism class.
	public static long mechanismInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of the Material class.
	public static long materialInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of the Element Metadata class.
	public static long elementMetaDataInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of the Species class.
	public static long speciesInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of the Species class.
	public static String speciesId;
	
	// Declared to serve as the sequence or serial number for instances of 
	// the Species class.
	public static long speciesSerialNo;

	// Declared to serve as the sequence or serial number for instances of 
	// the Chemical Reaction class.
	public static long reactionSerialNo;

	// Declared to serve as the id for instances of the Element Specification class.
	public static long elementSpecificationInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of the Product Specification class.
	public static long productSpecificationInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of the Reactant Specification class.
	public static long reactantSpecificationInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of the Species Metadata class.
	public static long speciesMetaDataInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of the Reaction class.
	public static long reactionInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of the Reaction Metadata class.
	public static long reactionMetaDataInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of NASA Polynomial
	// Coefficient.
	public static long nasaPolyCoeffsInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of transport parameter.
	public static long idTransportParameter = System.nanoTime();
	
	// Declared to serve as the id for instances of Reaction Order.
	public static long reactionOrderInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of Arrhenius coefficients.
	public static long rateCoeffArrheniusInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of Sticking coefficients.
	public static long rateCoeffStickingInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of Chebyshev coefficients.
	public static long rateCoeffChebInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of Coverage coefficients.
	public static long rateCoeffCovDepInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of Coverage coefficients.
	public static long rateCoeffLanTellerInstanceId = System.nanoTime();

	// Declared to serve as the id for instances of third body efficiency of
	// species in pressure dependent reactions.
	public static long reactionEfficiencyInstanceId = System.nanoTime();
	
	// Declared to serve as the id for instances of FallOff model coefficients.
	public static long rateCoeffFallOffInstanceId = System.nanoTime();
	
	public static final String EMPTY = "";
	public static final String HASH = "#";
	public static final String SPACE = " ";
	public static final String COLON = ":";
	public static final String UNDERSCORE = "_";
	public static final String BACKSLASH = "/";
	public static final String FRONTSLASH = "\\";
	// As reactions codified in the same reactionArray belong to the same
	// phase, the phaseArray vs phase HashMap is created while parsing
	// the phase blocks of CTML. The goal is to retrieve the phase of a
	// reaction while parsing the reaction blocks and then to put the reaction
	// under the phase using belongsToPhase relation.
	public static HashMap<String, String> reactionDataIdVsPhaseMap;	
	// When a site phase exists in a material, we create an instance of 
	// that material. To reuse the instance for additional site phases or 
	// bulk phases of that material, we maintain the material name vs material
	// instance hash map.
	public static HashMap<String, String> materialVsInstanceMap;	
	// Holds the id of the gas phase instance of a mechanism and stores
	// this as the value of the following two kyes separately:
	// GAS_species_data and GAS_reaction_data.  
	public static HashMap<String, String> gasPhaseDataSrcVsInstanceMap;	
	// We assume that a species belongs to only one phase, therefore, when
	// we find that a species with the same label appearing in multiple phases
	// we treat them as different species and assign different ids to them.
	// This speciesPhaseMap maintains the species-phase map.
	public static HashMap<String, String> speciesPhaseMap;
	// While parsing a phase, we create a unique OWL instance id for each 
	// species found there.
	public static HashMap<String, String> speciesUniqueIDMap;
	// Maintains the map between the reaction array data source and species
	// array datasrc. It is created to retrieve the coverage species unique
	// id while parsing coverage species, reactants and  products.
	public static HashMap<String, String> reactionArrayVsSpeciesArrayMap;
	// Maintains the map between the reaction array data source and material.
	// It is created to establish included in relations between reactions found 
	// in the aforementioned data source and their materials. 
	public static HashMap<String, String> reactionArrayVsMaterialMap;
	// Declared to reuse the same NASA Polynomial Coefficient instance
	// when Tmax, Tmin, P0, name, size and coefficients are the same
	// for two different species   
	public static HashMap<String, String> nasaCoeffsInstanceMap;
	
	// A member variable created to hold the instance of the class that
	// implemented OWL constructs, i.e. class, instance, object property
	// and data, creation methods. 
	public static IOwlConstructWriter iOwlConstructWriter;
	
	public static IMetadataWriter iCtmlMetadataWriter;
	public static IPhaseWriter iPhaseWriter;
	public static IElementWriter iElementWriter;
	public static ISpeciesWriter iSpeciesWriter;
	public static IReactionWriter iReactionWriter;
	public static IRateCoefficientWriter iRateCoeffWriter;
	
	public static IMetadataConverter iMetaDataConverter;
	public static IPhaseConverter iPhaseConverter;
	public static IElementConverter iElementConverter;
	public static ISpeciesConverter iSpeciesConverter;
	public static IReactionConverter iReactionConverter;
	
	public static String reactionType;
	public static String phaseType;
	public static HashMap<String, String> speciesVsPhaseClassMap;
	
	public static OWLDataFactory dataFactory = OWLManager.getOWLDataFactory();
	public static OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	public static OWLOntology ontology;
	public static IRI ontologyIRI;
	// Create an instance of an OWL API reasoner (we use the OWL API
	// built-in StructuralReasoner for the purpose of demonstration
	// here)
    public static StructuralReasonerFactory reasonerFactory;
    public static QueryEngine engine;
	public static String basePath;
	public static String mechanismName;

	public static boolean needsToCreateMechanism = true;

	public static boolean createdPhase = false;

	public static boolean createdTransportParameter = false;
	
	public static boolean createdReaction = false;
	
	public static boolean createdArrheniusArrhenius = false;
	
	public static boolean createdArrheniusSticking = false;
	
	public static boolean createdReactionOrder = false;
	
	public static boolean createdArrhenius = false;
	
	public static boolean createdCoverageDependency = false;
	
	public static boolean createdLandauTeller = false;
		
	public static boolean createdFallOff = false;
	
	public static IInitCtmlConverter initCtmlConverter;
	//================OWL to CTML conversion: Global================//
	//================Variable Declaration Started=================//
	public static IMetadataQuery iMetadataQuery;
	public static IPhaseQuery iPhaseQuery;
	public static IElementQuery iElementQuery;
	public static ISpeciesQuery iSpeciesQuery;
	public static IReactionQuery iReactionQuery;
	// Defined to reuse the phase class while querying property values of
	// its instances.
	public static String phaseClass;
	// Once the above query is performed, the list of results is stored in 
	// the following global variable. It reduces the amount of message 
	// passing.
	public static ArrayList<String> queryResult;
	// Phase OWL instance ids are preserved to retreive species which 
	// belong to a specific phase.
	public static ArrayList<String> phaseOwlIds;
	public static ArrayList<Phase> queriedPhaseList;
	public static ArrayList<Element> queriedElementList;
	public static ArrayList<SpeciesData> queriedSpeciesDataList;
	public static ArrayList<Species> queriedSpeciesList;
	public static ArrayList<ReactionData> queriedReactionDataList;
	public static ArrayList<Reaction> queriedReactionList;
	public static List<String> uniqueSpeciesDataIds;
	public static final String RDFS = "rdfs";
	public static final String RDFS_LABEL = "label";
	public static final String RDFS_COMMENT = "comment";
	public static final String RDFS_URL = "http://www.w3.org/2000/01/rdf-schema#";
	public static final String RDF = "rdf";
	public static final String RDF_TYPE = "type";
	public static final String RDF_URL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";	
	public static final String OWL = "owl";
	public static final String OWL_VERSIONINFO = "versionInfo";
	public static final String OWL_URL = "http://www.w3.org/2002/07/owl#";
	public static final String DUBLIN_CORE = "dc";
	public static final String DUBLIN_CORE_ID = "identifier";
	public static final String DUBLIN_CORE_URL = "http://purl.org/dc/elements/1.1/";
	public static final String GEOSPARQL = "geosparql";
	public static final String GEOSPARQL_DIMENSION = "dimension";
	public static final String GEOSPARQL_URL = "http://www.opengis.net/ont/geosparql#";
	public static final String LANDAUTELLER_YES = "yes";
	// Declared to merge species of site and bulk phases of the same material
	// (for example, MATERIAL1 and MATERIAL2)
	public static String previousSpeciesMetadataId;
	// Creates a species data instance
	public static SpeciesData speciesDataInOwl;
	// Creates a list of species
	public ArrayList<Species> speciesInOwlList;
	// Creates a list of comments
	public ArrayList<CtmlComment> ctmlComments;
	// Creates a list of reaction
	public ArrayList<Reaction> reactionInOwlList;
	public static ArrayList<String> materialOwlIds;
	// Declared to separate reactions of different materials (for example, 
	// MATERIAL1 and MATERIAL2)	
	public static String previousReactionMetadataId;
	// Creates a reaction data instance
	public static ReactionData reactionDataInOwl;
	// The Gas Phase OWL instance id is preserved to retreive reactions which 
	// belong to the phase. It is well-understood that there can be only one
	// gas phase in a mechanism. 
	public static String gasPhaseOwlId;
	public static String reactionMetadataOwlId;
	public static String owlFileName;
	public static HashMap<String, String> objectVsSourceComment;
	public static int phaseSequence;
	public static int elementDataSequence;
	public static int elementSequence;
	public static int speciesDataSequence;
	public static int speciesSequence;
	public static int reactionDataSequence;
	public static int reactionSequence;
	//================OWL to CTML conversion: Global================//
	//================Variable Declaration Ended====================//
}
