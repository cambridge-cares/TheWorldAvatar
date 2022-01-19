package uk.ac.cam.cares.jps.ontomatch.properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.env.Environment;

/***
 * Helper that loads all ontomatch related config params from property file
 *
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
@Configuration
@PropertySource("classpath:/ontomatch.properties")
public class OntomatchProperties {
	public final static String PY_NAME_LEXICALPROCESSOR = "py.path.lexicalProcessor";
	public final static String MATCH_TERM_FUNCTION_NAME = "py.function.name.term";
	public final static String MATCH_INDIVIDUAL_FUNCTION_NAME = "py.function.name.individual";
	public final static String PY_NAME_VALUEMATCHER = "py.path.valueMatcher";
	public final static String PY_NAME_DOMAINMATCHER = "py.path.domainMatcher";
	public final static String PY_NAME_STRINGMATCHER = "py.path.stringMatcher";
	public final static String PY_NAME_BOWMATCHER = "py.path.bowMatcher";
	public final static String PY_NAME_ISTRINGMATCHER = "py.path.istringMatcher";
	public final static String PY_NAME_IBOWMATCHER = "py.path.ibowMatcher";
	public final static String PY_NAME_ONETOONECARDI = "py.path.onetooneCardi";
	public static final String PY_NAME_LOADTBOX = "py.path.loadtbox";

	public final static String TOPICMODEL_TRAININGDOCUMENTS_PATH = "topicmodel.trainingdocuments.path";
	public final static String TOPICMODEL_DICTIONARY_PATH = "topicmodel.dictionary.path";
	public final static String TOPICMODEL_CORPUS_PATH = "topicmodel.corpus.path";
	public final static String TOPICMODEL_MODEL_PATH = "topicmodel.model.path";
	public final static String TRAINING_DOCS_NAME ="trainingdocs.name";
	public final static String SERVER_URL ="server.url";
    public final static String OM_KB_PATH = "ontomatch.kb.address";
    public final static String OM_KB_URL = "ontomatch.kb.url";
    public final static String OWL_EXT= "owl.extension";
    public final static String SUCCESS_FLAG= "success.flag";
	public static final String CARDINALITYFILTERING_TMP_ALIGNMENT_PATH = "caridinalityfiltering.tmp.alignment.path";
	public static final String TEMP_ABOXFILE_NAME = "temp.aboxfile.name";
	public static final String VENV_NAME = "venv.name";

	
    @Autowired
	private Environment env;
	private static ApplicationContext applicationContext;
	private static OntomatchProperties instance = null;

	/***
	 *  get the singleton instance of this class
	 * @return singleton instance
	 */
	public static OntomatchProperties getInstance() {
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (instance == null) {
			instance = applicationContext.getBean(OntomatchProperties.class);
		}

		return instance;
	}

	/***
	 * get the property in property file according to name
	 * @param name
	 * @return property
	 */
	public String getProperty(String name) {
		return env.getRequiredProperty(name);
	}

}
