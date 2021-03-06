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
 * &lt;?xml version="1.0" encoding="UTF-8"?&gt;&lt;h:div xmlns:h="http://www.w3.org/1999/xhtml"&gt;A reactant will normally be identified by name(s), formula, or molecule and at least one of these should normally be given. Amount(s) of reactant can be given after this identification and can describe mass, volume, etc. but not stoichiometr.&lt;/h:div&gt;
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
 *         &lt;element ref="{http://www.xml-cml.org/schema}metadataList" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;choice maxOccurs="unbounded" minOccurs="0">
 *           &lt;element ref="{http://www.xml-cml.org/schema}identifier"/>
 *           &lt;element ref="{http://www.xml-cml.org/schema}label"/>
 *           &lt;element ref="{http://www.xml-cml.org/schema}name"/>
 *         &lt;/choice>
 *         &lt;element ref="{http://www.xml-cml.org/schema}molecule" minOccurs="0"/>
 *         &lt;element ref="{http://www.xml-cml.org/schema}electron" minOccurs="0"/>
 *         &lt;element ref="{http://www.xml-cml.org/schema}substance" minOccurs="0"/>
 *         &lt;element ref="{http://www.xml-cml.org/schema}substanceList" minOccurs="0"/>
 *         &lt;element ref="{http://www.xml-cml.org/schema}formula" minOccurs="0"/>
 *         &lt;element ref="{http://www.xml-cml.org/schema}amount" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}count"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}title"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}dictRef"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}ref"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}id"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}role"/>
 *       &lt;attGroup ref="{http://www.xml-cml.org/schema}state"/>
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
    "metadataList",
    "identifierOrLabelOrName",
    "molecule",
    "electron",
    "substance",
    "substanceList",
    "formula",
    "amount"
})
@XmlRootElement(name = "reactant")
public class Reactant {

    protected List<MetadataList> metadataList;
    @XmlElements({
        @XmlElement(name = "identifier", type = Identifier.class),
        @XmlElement(name = "label", type = Label.class),
        @XmlElement(name = "name", type = Name.class)
    })
    protected List<java.lang.Object> identifierOrLabelOrName;
    protected Molecule molecule;
    protected Electron electron;
    protected Substance substance;
    protected SubstanceList substanceList;
    protected Formula formula;
    protected List<Amount> amount;
    @XmlAttribute(name = "count")
    protected Double count;
    @XmlAttribute(name = "title")
    protected java.lang.String title;
    @XmlAttribute(name = "dictRef")
    protected java.lang.String dictRef;
    @XmlAttribute(name = "ref")
    protected java.lang.String ref;
    @XmlAttribute(name = "id")
    protected java.lang.String id;
    @XmlAttribute(name = "role")
    protected java.lang.String role;
    @XmlAttribute(name = "state")
    protected java.lang.String state;
    @XmlAttribute(name = "convention")
    protected java.lang.String convention;

    /**
     * Gets the value of the metadataList property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the metadataList property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getMetadataList().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MetadataList }
     * 
     * 
     */
    public List<MetadataList> getMetadataList() {
        if (metadataList == null) {
            metadataList = new ArrayList<MetadataList>();
        }
        return this.metadataList;
    }

    /**
     * Gets the value of the identifierOrLabelOrName property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the identifierOrLabelOrName property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getIdentifierOrLabelOrName().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Identifier }
     * {@link Label }
     * {@link Name }
     * 
     * 
     */
    public List<java.lang.Object> getIdentifierOrLabelOrName() {
        if (identifierOrLabelOrName == null) {
            identifierOrLabelOrName = new ArrayList<java.lang.Object>();
        }
        return this.identifierOrLabelOrName;
    }

    /**
     * Gets the value of the molecule property.
     * 
     * @return
     *     possible object is
     *     {@link Molecule }
     *     
     */
    public Molecule getMolecule() {
        return molecule;
    }

    /**
     * Sets the value of the molecule property.
     * 
     * @param value
     *     allowed object is
     *     {@link Molecule }
     *     
     */
    public void setMolecule(Molecule value) {
        this.molecule = value;
    }

    /**
     * Gets the value of the electron property.
     * 
     * @return
     *     possible object is
     *     {@link Electron }
     *     
     */
    public Electron getElectron() {
        return electron;
    }

    /**
     * Sets the value of the electron property.
     * 
     * @param value
     *     allowed object is
     *     {@link Electron }
     *     
     */
    public void setElectron(Electron value) {
        this.electron = value;
    }

    /**
     * Gets the value of the substance property.
     * 
     * @return
     *     possible object is
     *     {@link Substance }
     *     
     */
    public Substance getSubstance() {
        return substance;
    }

    /**
     * Sets the value of the substance property.
     * 
     * @param value
     *     allowed object is
     *     {@link Substance }
     *     
     */
    public void setSubstance(Substance value) {
        this.substance = value;
    }

    /**
     * Gets the value of the substanceList property.
     * 
     * @return
     *     possible object is
     *     {@link SubstanceList }
     *     
     */
    public SubstanceList getSubstanceList() {
        return substanceList;
    }

    /**
     * Sets the value of the substanceList property.
     * 
     * @param value
     *     allowed object is
     *     {@link SubstanceList }
     *     
     */
    public void setSubstanceList(SubstanceList value) {
        this.substanceList = value;
    }

    /**
     * Gets the value of the formula property.
     * 
     * @return
     *     possible object is
     *     {@link Formula }
     *     
     */
    public Formula getFormula() {
        return formula;
    }

    /**
     * Sets the value of the formula property.
     * 
     * @param value
     *     allowed object is
     *     {@link Formula }
     *     
     */
    public void setFormula(Formula value) {
        this.formula = value;
    }

    /**
     * Gets the value of the amount property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the amount property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAmount().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Amount }
     * 
     * 
     */
    public List<Amount> getAmount() {
        if (amount == null) {
            amount = new ArrayList<Amount>();
        }
        return this.amount;
    }

    /**
     * Gets the value of the count property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getCount() {
        return count;
    }

    /**
     * Sets the value of the count property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setCount(Double value) {
        this.count = value;
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
     * Gets the value of the state property.
     * 
     * @return
     *     possible object is
     *     {@link java.lang.String }
     *     
     */
    public java.lang.String getState() {
        return state;
    }

    /**
     * Sets the value of the state property.
     * 
     * @param value
     *     allowed object is
     *     {@link java.lang.String }
     *     
     */
    public void setState(java.lang.String value) {
        this.state = value;
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
