//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.7 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2018.05.10 at 04:17:37 PM BST 
//


package uk.ac.cam.ceb.como.io.chem.file.jaxb;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * 
 *                 
 * <pre>
 * &lt;?xml version="1.0" encoding="UTF-8"?&gt;&lt;h:div xmlns:h="http://www.w3.org/1999/xhtml" class="summary"&gt;fragment normally contains molecules&lt;/h:div&gt;
 * </pre>
 * 
 *             
 * 
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice maxOccurs="unbounded" minOccurs="0">
 *           &lt;element ref="{http://www.xml-cml.org/schema}metadataList"/>
 *           &lt;element ref="{http://www.xml-cml.org/schema}label"/>
 *           &lt;element ref="{http://www.xml-cml.org/schema}molecule"/>
 *           &lt;element ref="{http://www.xml-cml.org/schema}fragmentList"/>
 *           &lt;element ref="{http://www.xml-cml.org/schema}join"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}title"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}ref"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}role"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}dictRef"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}countExpression"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}id"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}convention"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "metadataListOrLabelOrMolecule"
})
@XmlRootElement(name = "fragment")
public class Fragment {

    @XmlElements({
        @XmlElement(name = "metadataList", type = MetadataList.class),
        @XmlElement(name = "label", type = Label.class),
        @XmlElement(name = "molecule", type = Molecule.class),
        @XmlElement(name = "fragmentList", type = FragmentList.class),
        @XmlElement(name = "join", type = Join.class)
    })
    protected List<java.lang.Object> metadataListOrLabelOrMolecule;
    @XmlAttribute(name = "title")
    protected java.lang.String title;
    @XmlAttribute(name = "ref")
    protected java.lang.String ref;
    @XmlAttribute(name = "role")
    protected java.lang.String role;
    @XmlAttribute(name = "dictRef")
    protected java.lang.String dictRef;
    @XmlAttribute(name = "countExpression")
    protected java.lang.String countExpression;
    @XmlAttribute(name = "id")
    protected java.lang.String id;
    @XmlAttribute(name = "convention")
    protected java.lang.String convention;

    /**
     * Gets the value of the metadataListOrLabelOrMolecule property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the metadataListOrLabelOrMolecule property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getMetadataListOrLabelOrMolecule().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MetadataList }
     * {@link Label }
     * {@link Molecule }
     * {@link FragmentList }
     * {@link Join }
     * 
     * 
     */
    public List<java.lang.Object> getMetadataListOrLabelOrMolecule() {
        if (metadataListOrLabelOrMolecule == null) {
            metadataListOrLabelOrMolecule = new ArrayList<java.lang.Object>();
        }
        return this.metadataListOrLabelOrMolecule;
    }

    /**
     * Gets the value of the title property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getTitle() {
        return title;
    }

    /**
     * Sets the value of the title property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setTitle(java.lang.String value) {
        this.title = value;
    }

    /**
     * Gets the value of the ref property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getRef() {
        return ref;
    }

    /**
     * Sets the value of the ref property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setRef(java.lang.String value) {
        this.ref = value;
    }

    /**
     * Gets the value of the role property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getRole() {
        return role;
    }

    /**
     * Sets the value of the role property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setRole(java.lang.String value) {
        this.role = value;
    }

    /**
     * Gets the value of the dictRef property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getDictRef() {
        return dictRef;
    }

    /**
     * Sets the value of the dictRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setDictRef(java.lang.String value) {
        this.dictRef = value;
    }

    /**
     * Gets the value of the countExpression property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getCountExpression() {
        return countExpression;
    }

    /**
     * Sets the value of the countExpression property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setCountExpression(java.lang.String value) {
        this.countExpression = value;
    }

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setId(java.lang.String value) {
        this.id = value;
    }

    /**
     * Gets the value of the convention property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getConvention() {
        return convention;
    }

    /**
     * Sets the value of the convention property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setConvention(java.lang.String value) {
        this.convention = value;
    }

}
