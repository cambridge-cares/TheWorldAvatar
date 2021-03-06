//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.7 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2018.05.10 at 04:17:37 PM BST 
//


package uk.ac.cam.ceb.como.io.chem.file.jaxb;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for inheritType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="inheritType">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="merge"/>
 *     &lt;enumeration value="replace"/>
 *     &lt;enumeration value="delete"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "inheritType")
@XmlEnum
public enum InheritType {


    /**
     * 
     *               
     * <pre>
     * &lt;?xml version="1.0" encoding="UTF-8"?&gt;&lt;h:div xmlns:h="http://www.w3.org/1999/xhtml" class="summary"&gt;Values from this element will be merged.&lt;/h:div&gt;
     * </pre>
     * 
     *               
     * <pre>
     * &lt;?xml version="1.0" encoding="UTF-8"?&gt;&lt;h:div xmlns:h="http://www.w3.org/1999/xhtml" class="description"&gt;The merging is element-specific with the intention that information from the current element will not conflict with the existing information. It is an error if there is a conflict.&lt;/h:div&gt;
     * </pre>
     * 
     *             
     * 
     */
    @XmlEnumValue("merge")
    MERGE("merge"),

    /**
     * 
     *               
     * <pre>
     * &lt;?xml version="1.0" encoding="UTF-8"?&gt;&lt;h:div xmlns:h="http://www.w3.org/1999/xhtml" class="summary"&gt;Values from this element will replace existing information.&lt;/h:div&gt;
     * </pre>
     * 
     *               
     * <pre>
     * &lt;?xml version="1.0" encoding="UTF-8"?&gt;&lt;h:div xmlns:h="http://www.w3.org/1999/xhtml" class="description"&gt;The merging is element-specific with the intention that information from the current element will replace the existing information.&lt;/h:div&gt;
     * </pre>
     * 
     *             
     * 
     */
    @XmlEnumValue("replace")
    REPLACE("replace"),

    /**
     * 
     *               
     * <pre>
     * &lt;?xml version="1.0" encoding="UTF-8"?&gt;&lt;h:div xmlns:h="http://www.w3.org/1999/xhtml" class="summary"&gt;Components of this element will de deleted if they exist.&lt;/h:div&gt;
     * </pre>
     * 
     *             
     * 
     */
    @XmlEnumValue("delete")
    DELETE("delete");
    private final java.lang.String value;

    InheritType(java.lang.String v) {
        value = v;
    }

    public java.lang.String value() {
        return value;
    }

    public static InheritType fromValue(java.lang.String v) {
        for (InheritType c: InheritType.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
