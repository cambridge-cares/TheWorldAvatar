package uk.ac.cam.cares.jps.agent.file_management.marshallr.moo;

import java.io.IOException;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.ExecutableModel;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSMarshaller;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;

public class MoDSMarshallerMoo extends MoDSMarshaller {
	private static Logger logger = LoggerFactory.getLogger(MoDSMarshallerMoo.class);
	private MoDSMooAgentProperty MoDSMooAgentProperty;
	
	public MoDSMarshallerMoo(MoDSMooAgentProperty MoDSMooAgentProperty) {
		super(MoDSMooAgentProperty);
		this.MoDSMooAgentProperty = MoDSMooAgentProperty;
	}
	
	@Override
	public void plugInKinetics(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException {
		ModelKineticsSRMMoo kineticsSRM = new ModelKineticsSRMMoo(MoDSMooAgentProperty);
		ExecutableModel exeModel = kineticsSRM.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		kineticsSRM.formFiles(exeModel, otherOptions);
		kineticsSRM.setUpMoDS();
		kineticsSRM.placeScript();
		
		logger.info("Model kineticsSRM was added to the MoDS job.");
	}
	
	@Override
	public void plugInCantera(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException {
		ModelCanteraLFSMoo canteraLFS = new ModelCanteraLFSMoo(MoDSMooAgentProperty);
		ExecutableModel exeModel = canteraLFS.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		canteraLFS.formFiles(exeModel, otherOptions);
		canteraLFS.setUpMoDS();
		canteraLFS.placeScript();
		
		logger.info("Model canteraLFS was added to the MoDS job.");
	}
	
	@Override
	public void plugInModelMoo(List<String> experimentIRI, String mechanismIRI, List<String> reactionIRIList, String otherOptions) throws IOException, MoDSMooAgentException {
		ModelMoo modsMoo = new ModelMoo(MoDSMooAgentProperty);
		ExecutableModel exeModel = modsMoo.formExecutableModel(experimentIRI, mechanismIRI, reactionIRIList);
		modsMoo.formFiles(exeModel, otherOptions);
		modsMoo.setUpMoDS();
		
		logger.info("Model MOO was added to the MoDS job.");
	}
}
