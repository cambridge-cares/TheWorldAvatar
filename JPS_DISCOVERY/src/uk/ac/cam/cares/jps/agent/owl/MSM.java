package uk.ac.cam.cares.jps.agent.owl;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
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
		public com.hp.hpl.jena.rdf.model.Property Property() {
			// TODO Auto-generated method stub
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
		public com.hp.hpl.jena.rdf.model.Property Property() {
			// TODO Auto-generated method stub
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
		public com.hp.hpl.jena.rdf.model.Property Property() {
			// TODO Auto-generated method stub
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
		public com.hp.hpl.jena.rdf.model.Property Property() {
			// TODO Auto-generated method stub
			return null;
		}
	},
	
	hasOperation() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},

	hasInput() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasOutput() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasMandatoryPart() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	hasOptionalPart() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(MSM + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(MSM + this.name());
		}

	},
	
	modelReference() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(SAWSDL + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(SAWSDL + this.name());
		}

	},
	
	hasValue() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(ONTOAGENT + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(ONTOAGENT + this.name());
		}

	},
	
	hasDatatypeValue() {

		@Override
		public String IRI() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(ONTOAGENT + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(ONTOAGENT + this.name());
		}

	},
	
	
	// TODO-AE maybe change name to rest:hasAddress, see MSM diagram, then also change name in Java classes Service and Operation etc. 
	hasHttpUrl() {

		@Override
		public String IRI() {
			return null;
		}

		@Override
		public Resource Node() {
			Model model = ModelFactory.createDefaultModel();
			return model.createResource(ONTOAGENT + this.name());
		}

		@Override
		public com.hp.hpl.jena.rdf.model.Property Property() {
			Model model = ModelFactory.createDefaultModel();
			return model.createProperty(ONTOAGENT + this.name());
		}

	}


	;

	public abstract String IRI();

	public abstract Resource Node();

	public abstract com.hp.hpl.jena.rdf.model.Property Property();

	public static final String MSM = "http://iserve.kmi.open.ac.uk/ns/msm#";
	public static final String SAWSDL = "http://www.w3.org/ns/sawsdl#";
	public static final String ONTOAGENT = "http://www.theworldavatar.com/ontology/OntoAgent.owl#";

	private MSM() {

	}
}
