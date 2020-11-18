package com.cmclinnovations.prime.species;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.xml.sax.Attributes;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        
        Scanner in  = new Scanner(System.in);
        ArrayList<String> inputs = new ArrayList<String>();
        System.out.println("Enter the classes for elem:");
        while (!in.hasNextDouble()) {
        	inputs.add(in.next());
        }
        String inputElem = inputs.get(0);
        List<String> inputAttrib = inputs.subList(1, inputs.size());
        
        
        //System.out.println(inputs);
        //classGenerator(inputs);
        //elementGenerator(inputs);
        //elementListGenerator(inputs);
        //parseStatusGenerator(inputs); // self and attributes
        //parserElemGenerator(inputs);
        parserAttribGenerator(inputElem, inputAttrib);
//        converterStateGenerator(inputs);
//        System.out.println("-----------------------------------------------");
//        initConverterGenerator(inputs);
    }
    
    private static void classGenerator(ArrayList<String> clazes) {
    	for (String claz : clazes) {
    		char c[] = claz.toCharArray();
    		c[0] = Character.toUpperCase(c[0]);
    		String clazU = new String(c);
    		System.out.println("@XmlAttribute");
    		System.out.println("private String "+claz+";");
    		System.out.println("public String get"+clazU+"() {");
    		System.out.println("return "+claz+"; }");
    		System.out.println("public void set"+clazU+"(String "+claz+") { ");
    		System.out.println("this."+claz+" = "+claz+"; }\n");
    	}
    }
    
    private static void elementGenerator(ArrayList<String> elements) {
    	for (String element : elements) {
    		char c[] = element.toCharArray();
    		c[0] = Character.toUpperCase(c[0]);
    		String elementU = new String(c);
    		System.out.println("@XmlElement");
    		System.out.println("private "+elementU+" "+element+";");
    		System.out.println("public "+elementU+" get"+elementU+"() {");
    		System.out.println("return "+element+"; }");
    		System.out.println("public void set"+elementU+"("+elementU+" "+element+") { ");
    		System.out.println("this."+element+" = "+element+"; }\n");
    	}
    }
    
    private static void elementListGenerator(ArrayList<String> elements) {
    	for (String element : elements) {
    		char c[] = element.toCharArray();
    		c[0] = Character.toUpperCase(c[0]);
    		String elementU = new String(c);
    		String elementList = element+"List";
    		System.out.println("@XmlElement");
    		System.out.println("private ArrayList<"+elementU+"> "+elementList+";");
    		System.out.println("public ArrayList<"+elementU+"> get"+elementU+"() {");
    		System.out.println("return "+elementList+"; }");
    		System.out.println("public void set"+elementU+"(ArrayList<"+elementU+"> "+elementList+") { ");
    		System.out.println("this."+elementList+" = "+elementList+"; }\n");
    	}
    }
    
    private static void parseStatusGenerator(ArrayList<String> parses) {
    	for (String parse : parses) {
    		char c[] = parse.toCharArray();
    		c[0] = Character.toUpperCase(c[0]);
    		String U = new String(c);
    		System.out.println("boolean "+parse+" = false;");
    		System.out.println("public boolean is"+U+"() {");
    		System.out.println("return "+parse+"; }");
    		System.out.println("public void set"+U+"(boolean "+parse+") {");
    		System.out.println("this."+parse+" = "+parse+"; }\n");
    	}
    }
    
    private static void parserElemGenerator(ArrayList<String> elems) {
    	for (String elem : elems) {
    		char c[] = elem.toCharArray();
    		c[0] = Character.toUpperCase(c[0]);
    		String U = new String(c);
    		System.out.println("private void parseElement(String qName, Attributes attributes) {");
    		System.out.println("if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElem"+U+"())) {");
    		System.out.println(elem+"ParseStatus.set"+U+"(true); } }\n");
    	}
    }
    
    private static void parserAttribGenerator(String elem, List<String> attribs) {
    	char c[] = elem.toCharArray();
		c[0] = Character.toUpperCase(c[0]);
		String elemU = new String(c);
    	System.out.println("private void parseAttributes(String qName, Attributes attributes) {");
    	System.out.println("if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElem"+elemU+"())) {");
    	for (String attrib : attribs) {
    		char cc[] = attrib.toCharArray();
    		cc[0] = Character.toUpperCase(cc[0]);
    		String attribU = new String(cc);
    		System.out.println("String "+attrib+" = attributes.getValue(primeSpeciesVocabulary.getAttrib"+attribU+"());");
    		System.out.println("if ("+attrib+" != null) {");
    		System.out.println(elem+".set"+attribU+"("+attrib+");");
    		System.out.println(elem+"ParseStatus.set"+attribU+"(true);");
    		System.out.println(elem+"ParseStatus.set"+elemU+"(true); }\n");
    	}
    	System.out.println("} }\n");
    }
    
    private static void converterStateGenerator(ArrayList<String> elems) {
    	for (String elem : elems) {
    		char c[] = elem.toCharArray();
    		c[0] = Character.toUpperCase(c[0]);
    		String U = new String(c);
    		System.out.println("public static "+U+" "+elem+";");
    		System.out.println("public static "+U+"ParseStatus "+elem+"ParseStatus;");
    		System.out.println("public static I"+U+"Parser i"+U+"Parser;");
    		System.out.println("public static I"+U+"Converter i"+U+"Converter;");
    		System.out.println("public static I"+U+"Writer i"+U+"Writer;\n");
    	}
    }
    
    private static void initConverterGenerator(ArrayList<String> elems) {
    	for (String elem : elems) {
    		char c[] = elem.toCharArray();
    		c[0] = Character.toUpperCase(c[0]);
    		String U = new String(c);
    		System.out.println(elem+" = new "+U+"();");
    		System.out.println(elem+"ParseStatus = new "+U+"ParseStatus();");
    		System.out.println("i"+U+"Parser = new "+U+"Parser();");
    		System.out.println("i"+U+"Converter = new "+U+"Converter();");
    		System.out.println("i"+U+"Writer = new "+U+"Writer();\n");
    	}
    }
    
    
    
//    copyright content bibliographyLink preferredKey chemicalIdentifier chemicalComposition additionalDataItem name atom component coal speciesLink amount uncertainty
//    1
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemCopyright())) {
//    copyrightParseStatus.setCopyright(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemContent())) {
//    contentParseStatus.setContent(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemBibliographyLink())) {
//    bibliographyLinkParseStatus.setBibliographyLink(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemPreferredKey())) {
//    preferredKeyParseStatus.setPreferredKey(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemChemicalIdentifier())) {
//    chemicalIdentifierParseStatus.setChemicalIdentifier(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemChemicalComposition())) {
//    chemicalCompositionParseStatus.setChemicalComposition(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAdditionalDataItem())) {
//    additionalDataItemParseStatus.setAdditionalDataItem(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemName())) {
//    nameParseStatus.setName(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAtom())) {
//    atomParseStatus.setAtom(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemComponent())) {
//    componentParseStatus.setComponent(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemCoal())) {
//    coalParseStatus.setCoal(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemSpeciesLink())) {
//    speciesLinkParseStatus.setSpeciesLink(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAmount())) {
//    amountParseStatus.setAmount(true); } }
//
//    private void parseElement(String qName, Attributes attributes) {
//    if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemUncertainty())) {
//    uncertaintyParseStatus.setUncertainty(true); } }

}
