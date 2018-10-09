package uk.ac.cam.cares.jps.servicespool.test;

import org.json.JSONException;
import org.json.JSONObject;

public class TestBuildingData {

	public static String buildingsInString  = "{ \r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_3FA056EB-3F9A-48B0-95A2-7C8CC2319B7E\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79826.1484375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0-95A2-7C8CC2319B7E\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454925.78125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"22.083000000000002\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"140.28130867746722\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"17.13276583276476\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"77.84151104831635\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_35ED6FD1-5876-4F03-996D-8C885FD11057\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79973.71875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"3-996D-8C885FD11057\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454782.125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.737\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"62.69621896421969\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"43.933992586863255\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"12.194571766421577\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_B42C1246-F744-4AC3-80EE-1768B1729924\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79921.453125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"3-80EE-1768B1729924\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454748.5625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"8.495000000000001\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"62.40695834723154\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.763834586891047\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"36.02745619798401\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_9D576B3B-9D3C-4E50-812D-FDCA5F55198A\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79785.65625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0-812D-FDCA5F55198A\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454908.8125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"24.349\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"102.71120642156859\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"29.960374440069312\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"88.9055006696234\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_6FA9E00C-A79E-408C-9AD4-2880E3A60972\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79794.1484375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"C-9AD4-2880E3A60972\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454786.21875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"37.544\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"1\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0.0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0.0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"21.994791406845472\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_ABE4C2D7-C6E0-4B95-9A3C-E4457B796F3C\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79787.640625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"5-9A3C-E4457B796F3C\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454766.46875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"2.9479999999999995\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"64.06391913991176\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"2.1863527662027455\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"3.5010548773057315\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_0FC3F2FF-A572-4AAE-B7C2-EA3BE1FA1C12\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79711.0390625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"E-B7C2-EA3BE1FA1C12\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454754.625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"3.7880000000000003\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"157.60015169277182\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"12.127251223923013\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"45.69805327094259\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_C1C1997A-43EC-4F7C-B62B-5F115CAD5117\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79925.125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"C-B62B-5F115CAD5117\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454780.5625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"9.981\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"62.66250519556777\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"14.484646410741185\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"12.18068970379577\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/PecipitationOfDen_Haag934759441\" : { \r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Precipitation\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasIntensity\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0.0\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_81AB074F-EEB6-4006-87B6-6BB5E80E692B\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79909.65625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"6-87B6-6BB5E80E692B\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454806.6875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.207999999999998\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"153.6083092001291\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"16.543349307636653\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"11.54724764706535\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://test.com/upperPoint\" : { \r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/ontology/Point\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"455190\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"80000\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_240BBC7B-CE2C-420D-89E5-CFD6A2C0C044\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79954.203125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"D-89E5-CFD6A2C0C044\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454785.0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"7.461\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"63.27490516231601\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"23.616948568285327\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"20.012886473470036\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_97612518-F080-48FE-B48C-D3600B96BB1E\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79904.6640625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"E-B48C-D3600B96BB1E\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454824.03125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.174\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"153.7192098064336\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.196489685096378\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"23.943691319508492\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/WindOfDen_Haag934759441\" : { \r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasSpeed\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"6.7\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Wind\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasDirection\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"250\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_5638ACFF-363C-409E-996B-2F423D46A9E7\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79923.5546875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"E-996B-2F423D46A9E7\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454830.8125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.332\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"153.63927720769354\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"5.6796918563345296\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"21.55373604732266\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_4611BF50-156F-458A-92EC-A11EEF98471F\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79948.859375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"A-92EC-A11EEF98471F\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454803.5625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.178\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"63.123883789801624\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"12.070734017488169\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"27.02887001805893\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_3C20D25F-1A9B-4C9B-B55F-7097636D6CAF\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79965.609375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"B-B55F-7097636D6CAF\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454823.625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.44\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"102.65180382686528\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"24.58331644715868\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"11.842227082992297\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/WeatherOfDen_Haag934759441\" : { \r\n" + 
			"    \"http://dbpedia.org/ontology/locationCity\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"http://dbpedia.org/resource/The_Hague\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasPrecipitation\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"http://www.theworldavatar.com/PecipitationOfDen_Haag934759441\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasWind\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"http://www.theworldavatar.com/WindOfDen_Haag934759441\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.theworldavatar.com/weather.owl#hasIcon\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"04d\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasWeatherCondition\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Cloud\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasHumidity\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"63\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasTemperature\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"17\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://test.com/lowerPoint\" : { \r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/ontology/Point\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454670\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79480\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings3.owl#BuildingGUID_E09C7CD0-2088-4F9F-AA4E-C0ACB582D77F\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79952.265625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"F-AA4E-C0ACB582D77F\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454688.28125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"15.303\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"63.355762445880245\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"35.599823062466385\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"45.50402680069232\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_7CA23259-87DB-4FF2-913B-DA4D82A5D3AE\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79949.6875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"2-913B-DA4D82A5D3AE\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454828.59375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.344999999999999\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"11.911813136216717\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"21.671364403851815\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"22.899354404194593\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Cloud\" : { \r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherCondition\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings4.owl#BuildingGUID_BD5DD2CF-FFE1-4F39-9704-70D673C4744B\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79759.09375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"9-9704-70D673C4744B\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454874.8125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"16.275000000000002\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"102.65273150454249\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"12.297131127957927\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"50.932170565715694\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_43E9C3A8-46CF-46A1-9CF1-5DC457A9EF50\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79937.171875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"1-9CF1-5DC457A9EF50\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454811.875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.181000000000001\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"153.62997382056363\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"13.256393494502742\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"14.370313859138786\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_94405F3C-FB53-4EC8-93A1-5F95FEC74CBD\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79974.6171875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"8-93A1-5F95FEC74CBD\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454818.59375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.44\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"12.613126977268506\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"8.0351608360033\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"30.517742062059675\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_DDA5FD88-34C5-4C44-A485-CA001E91C7FD\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79735.75\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"4-A485-CA001E91C7FD\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454692.1875\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"3.8180000000000005\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"70.85011770550763\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"36.6426969005909\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.097839849636468\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_00564872-9958-4788-997C-1E86F79AF802\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79914.75\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"8-997C-1E86F79AF802\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454796.25\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.181\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"153.6062361437698\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"16.71367695325049\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"11.687001263590085\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_73624930-7901-4F47-84FF-C1D0FC2E8991\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79966.4453125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"7-84FF-C1D0FC2E8991\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454775.03125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.067\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"63.84253387496758\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"19.94850881146744\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"6.876083011288113\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_952A0A82-9CEB-4F21-836D-3845D068012A\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79914.0703125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"1-836D-3845D068012A\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454775.5625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"9.948\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"62.533516186061\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"15.847145562234376\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"11.992398715566665\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings2.owl#BuildingGUID_2DDCBBF1-041C-40EE-AE1C-96C90F63DAF2\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79931.8359375\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"E-AE1C-96C90F63DAF2\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454820.0625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.209\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"63.307463192052545\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"5.097374782828319\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.99169886085205\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://test.com/aRegionInstance\" : { \r\n" + 
			"    \"http://test.com/Property/upperPoint\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"http://test.com/upperPoint\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/lowerPoint\" : [ { \r\n" + 
			"      \"type\" : \"uri\" ,\r\n" + 
			"      \"value\" : \"http://test.com/lowerPoint\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/ontology/Region\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"   ,\r\n" + 
			"  \"http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings1.owl#BuildingGUID_E4705286-9AFA-433E-9105-4F953FA50FE1\" : { \r\n" + 
			"    \"http://test.com/Property/hasBuildingX\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"79891.390625\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"http://test.com/Ontology/Building\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingName\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"E-9105-4F953FA50FE1\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingY\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"454821.53125\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingHeigth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"10.163\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingType\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"0\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#long\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingAngel\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"63.8860873697093\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingWidth\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"16.6691075064258\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ] ,\r\n" + 
			"    \"http://test.com/Property/hasBuildingLength\" : [ { \r\n" + 
			"      \"type\" : \"literal\" ,\r\n" + 
			"      \"value\" : \"12.42453784351756\" ,\r\n" + 
			"      \"datatype\" : \"http://www.w3.org/2001/XMLSchema#double\"\r\n" + 
			"    }\r\n" + 
			"     ]\r\n" + 
			"  }\r\n" + 
			"}\r\n" + 
			"\r\n";
	
	public static JSONObject getBuildingData() {
		
		try {
			return new JSONObject(buildingsInString);
		} catch (JSONException e) {
			return new JSONObject();
		}
	}
	
}
