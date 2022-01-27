package uk.ac.cam.cares.jps.ontomatch.alignment;
/**
 * object:Measurement is one entry in an list of alignment
 * 
 *
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
public class Measurement {
 private String entity1;
 private String entity2;
 private String measure;
 
 /***
  * empty constructor
  */
 public Measurement() {
	 this.entity1 = null;
	 this.entity2 = null;
	 this.measure = null;
 }
 
 /**
  * Constructor
  * @param fields
  */
 public Measurement(String[] fields) {
	 this.entity1 = fields[0];
	 this.entity2 = fields[1];
	 this.measure = fields[2];
 }
	
 /**
  * return the combined name string of the entity pair as ID, which defines one unique measurement
  * @return
  */
 public String getIdentity() {
	 return getShort(this.entity1)+getShort(this.entity2);
 }
 
 /**
  * return the shortened name from a IRI string
  * @param IRI
  * @return
  */
 protected String getShort(String IRI) {
	 String[] segs = IRI.split("#");
	 return segs[segs.length-1];
 }
 
 /***
  * return the fields in one measurement as a list of string
  * @return
  */
 public String[] getFields() {
	 String[] fields =  {this.entity1, this.entity2, this.measure};
	 return fields;
 }
	
}
