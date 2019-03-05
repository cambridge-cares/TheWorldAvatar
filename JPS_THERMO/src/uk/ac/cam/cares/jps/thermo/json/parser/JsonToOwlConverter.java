package uk.ac.cam.cares.jps.thermo.json.parser;

import java.io.IOException;
import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.controller.OntoKin;
import com.cmclinnovations.ontochem.controller.OntoKinFactory;
import com.cmclinnovations.ontochem.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochem.model.exception.OntoException;

/**
 * 
 * @author NK510 This class implements method to convert json file into owl
 *         file. Input json file is result of NASA polynomials calculations done by 
 *         using Python code.
 *
 */

public class JsonToOwlConverter {

	private ApplicationContext applicationContext;
	private AppConfigOperationControl opCtrl;

	public void convertJsonIntoOwl(String jsonFilePath, String catalinaFolderPath) throws IOException {

		/**
		 * @author NK This source code (method) is implemented by Dr M.S.F. Farazi.
		 * e-mail:  <msff2@cam.ac.uk>
		 */

		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (opCtrl == null) {
			opCtrl = applicationContext.getBean(AppConfigOperationControl.class);
		}

		ArrayList<String> sourceFiles = new ArrayList<String>();

		sourceFiles.add(jsonFilePath);

		if (sourceFiles != null) {
			OntoKin ontoKin = OntoKinFactory.getOntoKin(opCtrl.getOpReadCompChem(), opCtrl.getOpWriteOwl(), sourceFiles,
					catalinaFolderPath);
			try {
				ontoKin.convert();
			} catch (OWLOntologyCreationException | OntoException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

	}

}