package uk.ac.cam.cares.jps.agent.file_management.marshallr.datadriven;

import java.io.IOException;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.ExecutableModel;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSMarshaller;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgentException;

public class MoDSMarshallerDataDriven extends MoDSMarshaller {
	private static Logger logger = LoggerFactory.getLogger(MoDSMarshallerDataDriven.class);
	private MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty;
	
	public MoDSMarshallerDataDriven(MoDSDataDrivenAgentProperty modsDataDrivenAgentProperty) {
		super(modsDataDrivenAgentProperty);
		this.modsDataDrivenAgentProperty = modsDataDrivenAgentProperty;
	}
	
	@Override
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSDataDrivenAgentException {
		ModelKineticsSRMDataDriven kineticsSRM = new ModelKineticsSRMDataDriven(modsDataDrivenAgentProperty);
		ExecutableModel exeModel = kineticsSRM.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		kineticsSRM.formFiles(exeModel, otherOptions);
		kineticsSRM.setUpMoDS();
		kineticsSRM.placeScript();
		
		logger.info("Model kineticsSRM was added to the MoDS job.");
	}
	
	@Override
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSDataDrivenAgentException {
		ModelCanteraLFSDataDriven canteraLFS = new ModelCanteraLFSDataDriven(modsDataDrivenAgentProperty);
		ExecutableModel exeModel = canteraLFS.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		canteraLFS.formFiles(exeModel, otherOptions);
		canteraLFS.setUpMoDS();
		canteraLFS.placeScript();
		
		logger.info("Model canteraLFS was added to the MoDS job.");
	}
	
	
	//new METHOD
	//experimentIRI is CSV now
	@Override
	//	public void plugInModelDataDriven(List<String> dataVar, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSDataDrivenAgentException;

	public void plugInModelDataDriven(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSDataDrivenAgentException {
		ModelSurrogateDataDriven modsSurrogate = new ModelSurrogateDataDriven(modsDataDrivenAgentProperty);
		ExecutableModel exeModel = modsSurrogate.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		modsSurrogate.formFiles(exeModel, otherOptions);
		modsSurrogate.setUpMoDS();
		
		logger.info("Model canteraLFS was added to the MoDS job.");
	}
}
