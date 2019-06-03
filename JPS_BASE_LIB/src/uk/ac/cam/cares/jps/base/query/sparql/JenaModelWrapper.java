package uk.ac.cam.cares.jps.base.query.sparql;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.NodeIterator;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class JenaModelWrapper {

	private OntModel model = null;
	private String iri = null;
	private String startSubject = null;
	
	public JenaModelWrapper(OntModel model, String iri) {
		this.model = model;
		this.iri = iri;
	}

	public OntModel getModel() {
		return model;
	}
	
	public RDFNode getPropertyValue(String startSubject, String... path) {
		
		this.startSubject = startSubject;
		
		Individual currentSubject = model.getIndividual(startSubject);
		Property currentProp = null; 
		RDFNode currentObject = null;
		
		int i = 0;
		while (i < path.length - 1) {
		
			String propAsString = concat(path[i], path[i+1]);
			
			// check whether the next string is a class or the next property
			boolean noClassRestriction = true;
			String classAsString = null;
			if (i+2 < path.length) {
				String prefix = PrefixToUrlMap.getPrefixUrl(path[i+2]);
				if (prefix == null) {
					// i.e. no prefix for property but a class URL is given
					noClassRestriction = false;
					classAsString = path[i+2];
				} 
			}
	
			currentProp = model.getProperty(propAsString);

			List<RDFNode> objects = listProperyValues(currentSubject, currentProp);
			if (objects.isEmpty()) {
				currentObject = null;
				break;
			} 
			
			// navigate one property further
			if (noClassRestriction) {
				currentObject = objects.get(0);
			} else {
				OntClass c = model.getOntClass(classAsString);
				for (RDFNode current : objects) {
					if (((OntResource) current).hasRDFType(c)) {
						currentObject = current;
						break;
					}
				}
			}
		
			if (i < path.length - 2) {
				// continue navigation
				currentSubject = ((OntResource) currentObject).asIndividual();
			} 
			
			if (noClassRestriction) {
				i += 2;
			} else {
				i += 3;
			}
		}
		
		return currentObject;
	}
	
	public RDFNode getPropertyValueWithoutType(String startSubject, String... path) {
		
		this.startSubject = startSubject;
		
		Individual currentSubject = model.getIndividual(startSubject);
		Property currentProp = null; 
		RDFNode currentObject = null;
		
		for (int i=0; i<path.length; i=i+2) {
			String propAsString = concat(path[i], path[i+1]);
			currentProp = model.getProperty(propAsString);
			currentObject = currentSubject.getPropertyValue(currentProp);
			if (i < path.length - 2) {
				// navigate one property further
				currentSubject = ((OntResource) currentObject).asIndividual();
			} 
		}
		
		return currentObject;
	}
	
	public RDFNode setPropertyValue(String startSubject, Object destObject, String... path) {
		
		this.startSubject = startSubject;
		
		Individual currentSubject = model.getIndividual(startSubject);
		Property currentProp = null; 
		
		int i = 0;
		while (i < path.length - 1) {
			
			String propAsString = concat(path[i], path[i+1]);
			
			// check whether the next string is a class or the next property
			boolean noClassRestriction = true;
			String classAsString = null;
			if (i+2 < path.length) {
				String prefix = PrefixToUrlMap.getPrefixUrl(path[i+2]);
				if (prefix == null) {
					// i.e. no prefix for property but a class URL
					noClassRestriction = false;
					classAsString = path[i+2];
				} 
			}
			
			boolean isObjectProp = true;
			currentProp = model.getObjectProperty(propAsString);
			if (currentProp == null) {
				currentProp = model.getDatatypeProperty(propAsString);
				isObjectProp = false;
			}
			
			if (currentProp == null) {
				throw new JPSRuntimeException("property not found, property = " + propAsString);
			}
			
			if (i < path.length - 2) {
				// navigate one property further
				//RDFNode object = currentSubject.getPropertyValue(currentProp);
				List<RDFNode> objects = listProperyValues(currentSubject, currentProp);
				
				if (!objects.isEmpty()) {
					
					RDFNode object = null;
					if (noClassRestriction) {
						object = objects.get(0);
					} else {
						OntClass c = model.getOntClass(classAsString);
						for (RDFNode current : objects) {
							if (((OntResource) current).hasRDFType(c)) {
								object = current;
								break;
							}
						}
					}
				
					currentSubject = ((OntResource) object).asIndividual();
				} else {
					RDFNode newObject = createObject(currentSubject, currentProp, i, path);
					currentSubject.setPropertyValue(currentProp, newObject);
					currentSubject = ((OntResource) newObject).asIndividual();
				}
			} else {
				// don't navigate any more but change the object at the end of the entire navigation path
				currentSubject.removeAll(currentProp);
				if (isObjectProp) {
					
					if (destObject instanceof Individual) {
						currentSubject.setPropertyValue(currentProp, (Individual) destObject);
					} else {
						// destObject must be the URI of an individual
						OntClass c = ((ObjectProperty) currentProp).getRange().asClass();
						Individual object = c.createIndividual((String) destObject);
						currentSubject.setPropertyValue(currentProp, object);
					}
					
				} else {
					Literal literal = model.createTypedLiteral(destObject);
					currentSubject.setPropertyValue(currentProp, literal);
				}
			}
			
			if (noClassRestriction) {
				i += 2;
			} else {
				i += 3;
			}
		}
		
		return currentSubject.getPropertyValue(currentProp);
	}
	
	private List<RDFNode> listProperyValues(Individual subject, Property property) {
		
		List<RDFNode> result = new ArrayList<RDFNode>();
		
		NodeIterator it = subject.listPropertyValues(property);
		while (it.hasNext()) {
			RDFNode node = it.nextNode();
			result.add(node);
		}
		
		return result;
	}
	
	public RDFNode setPropertyValueWithoutTypes(String startSubject, Object destObject, String... path) {
		
		this.startSubject = startSubject;
		
		Individual currentSubject = model.getIndividual(startSubject);
		Property currentProp = null; 
		
		for (int i=0; i<path.length; i=i+2) {
			String propAsString = concat(path[i], path[i+1]);
			boolean isObjectProp = true;
			currentProp = model.getObjectProperty(propAsString);
			if (currentProp == null) {
				currentProp = model.getDatatypeProperty(propAsString);
				isObjectProp = false;
			}
			
			if (currentProp == null) {
				throw new JPSRuntimeException("property not found, property = " + propAsString);
			}
			
			if (i < path.length - 2) {
				// navigate one property further
				RDFNode object = currentSubject.getPropertyValue(currentProp);
				if (object != null) {
					currentSubject = ((OntResource) object).asIndividual();
				} else {
					object = createObject(currentSubject, currentProp, i, path);
					currentSubject.setPropertyValue(currentProp, object);
					currentSubject = ((OntResource) object).asIndividual();
				}
			} else {
				// don't navigate any more but change the object at the end of the entire navigation path
				currentSubject.removeAll(currentProp);
				if (isObjectProp) {
					
					if (destObject instanceof Individual) {
						currentSubject.setPropertyValue(currentProp, (Individual) destObject);
					} else {
						// destObject must be the URI of an individual
						OntClass c = ((ObjectProperty) currentProp).getRange().asClass();
						Individual object = c.createIndividual((String) destObject);
						currentSubject.setPropertyValue(currentProp, object);
					}
					
				} else {
					Literal literal = model.createTypedLiteral(destObject);
					currentSubject.setPropertyValue(currentProp, literal);
				}
			}
		}
		
		return currentSubject.getPropertyValue(currentProp);
	}
	
	private RDFNode createObject(Individual subject, Property prop, int position, String[] path) {
		
		// for creating an object we have to specify a class
		// check whether the class of the object is defined as part of the path
		if (position < path.length - 2) {
			if (Paths.CLASS.equals(path[position + 2])) {
				String className = concat(path[position + 3], path[position + 4]);
				OntClass c = model.getOntClass(className);
				return c.createIndividual(newIri());
			}
		}
		
		// check whether the class is given by the domain of the successive property
		if (position < path.length - 2) {
			String propName = concat(path[position + 2], path[position + 3]);
			OntResource domain = null;
			ObjectProperty succProp = model.getObjectProperty(propName);
			if (succProp != null) {
				domain = succProp.getDomain();
			} else {
				DatatypeProperty succdProp = model.getDatatypeProperty(propName);
				domain = succdProp.getDomain();
			}
			if (domain != null) {
				OntClass c =  domain.asClass();
				return c.createIndividual(newIri());
			}
		}
		
		// check whether the class is given by the range of the current property
		OntResource range = ((ObjectProperty) prop).getRange();
		if (range != null) {
			OntClass c =  range.asClass();
			return c.createIndividual(newIri());
		}
		return null;
	}
	
	public static String concat(String prefix, String name) {
		return PrefixToUrlMap.getPrefixUrl(prefix) + name;
	}
	
	public Individual createIndividual(String prefix, String className) {
		String fullIri = concat(prefix, className);
		return model.getOntClass(fullIri).createIndividual();
	}
	
	private String newIri() {
		if (iri != null) {
			return newIri(iri);
		}
		
		return newIri(startSubject);
	}
	
	/**
	 * Creates a new IRI by appending an UUID after the given IRI. 
	 * If the given IRI has a fragment part (starting with #) then
	 * the fragment part is removed before.
	 * 
	 * @param iri
	 * @return
	 */
	public static String newIri(String iri) {
		String path = "";
		int i = iri.indexOf("#");
		if (i>0) {
			path = iri.substring(0, i) + "#";
		} else {
			path = iri + "#";
		}

		return path + UUID.randomUUID().toString();
	}
	
	public void removeSubtree(String startSubject, String... path) {
		
		this.startSubject = startSubject;
		
		RDFNode nodeBeforeLast = null;
		if (path.length <= 2) {
			
			nodeBeforeLast = model.getIndividual(startSubject);
			
		} else {
			
			String[] pathMinusLast = new String[path.length - 2];
			for (int i=0; i<pathMinusLast.length; i++) {
				pathMinusLast[i] = path[i];
			}
			nodeBeforeLast = getPropertyValue(startSubject, pathMinusLast);
		} 
		
		String lastPropName = concat(path[path.length - 2], path[path.length - 1]);
		Property lastProp = model.getProperty(lastPropName);
		
		// remove the subtree with root lastNode
		RDFNode lastNode = nodeBeforeLast.asResource().getPropertyResourceValue(lastProp); 
		removeSubtreeRecursively(lastNode, 10);
		
		// remove the property between nodeBeforeLast and lastNode
		Statement statement = nodeBeforeLast.asResource().getProperty(lastProp);
		model.remove(statement);
	}
	
	private void removeSubtreeRecursively(RDFNode node, int counter) {
		
		if (counter < 1) {
			return;
		}
		
		if (node.isResource()) {
			Resource r = node.asResource();
			StmtIterator it = r.listProperties();
			while (it.hasNext()) {
				Statement statement = it.next();
				RDFNode object = statement.getObject();
				removeSubtreeRecursively(object, counter - 1);
			}
			r.removeProperties();
		}
	}
} 
