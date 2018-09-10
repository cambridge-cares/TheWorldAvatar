package uk.ac.cam.cares.jps.agents.ontology;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;

public enum MSM {

	Service() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			return null;
		}

	},
	
	Operation() {
		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			return null;
		}

	},
	
	MessagePart() {
		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			return null;
		}

	},
	
	MessageContent() {
		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			return null;
		}
	},
	
	hasOperation() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},

	hasInput() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasOutput() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasMandatoryPart() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasOptionalPart() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	// TODO-AE URGENT replace this by hasType
	modelReference() {

		@Override
		public String IRI() {
			return SAWSDL + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(SAWSDL + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(SAWSDL + this.name());
		}

	},
	
	hasType() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	isArray() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasName() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasObjectValue() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasDataValue() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	
	// TODO-AE maybe change name to rest:hasAddress, see MSM diagram, then also change name in Java classes Service and Operation etc. 
	hasHttpUrl() {

		@Override
		public String IRI() {
			return MSM + this.name();
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	};

	public abstract String IRI();

	public abstract Resource Node();

	public abstract Property Property();

	public static final String MSM = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#";
	// TODO-AE URGENT discard this
	public static final String SAWSDL = "http://www.w3.org/ns/sawsdl#";

	private MSM() {
	}
}
