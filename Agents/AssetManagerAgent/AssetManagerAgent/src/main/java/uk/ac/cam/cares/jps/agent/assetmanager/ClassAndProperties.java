package uk.ac.cam.cares.jps.agent.assetmanager;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class ClassAndProperties {
        // prefix
	public static final String ONTODEV = "https://www.theworldavatar.com/kg/ontodevice/";
    public static final String ONTOLAB = "https://www.theworldavatar.com/kg/ontolab/";
    public static final String ONTOSYSTEM = "https://www.theworldavatar.com/kg/ontotechnicalsystem/";
    public static final String ONTOINMA = "https://www.theworldavatar.com/kg/ontoinma/";
    public static final String ONTOEPE = "https://www.theworldavatar.com/kg/ontoelecpowerequipment/";
    public static final String ONTOASSET = "https://www.theworldavatar.com/kg/ontoassetmanagement/";
    public static final String ONTOBIM = "https://www.theworldavatar.com/kg/ontobim/";
    public static final String ONTOEMS = "https://www.theworldavatar.com/kg/ontoems/";

    public static final String P_DEV = ONTODEV;
    public static final String P_LAB = ONTOLAB;
    public static final String P_SYS = ONTOSYSTEM;
    public static final String P_INMA = ONTOINMA;
    public static final String P_ASSET = ONTOASSET;
    public static final String P_EPE = ONTOEPE;
    public static final String P_BIM = ONTOBIM;
    public static final String P_EMS = ONTOEMS;
    public static final String P_SAREF = "https://saref.etsi.org/core/";
    public static final String P_OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String P_FIBO_AAP = "https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/";
    public static final String P_FIBO_ORG = "https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/";
    public static final String P_BOT = "https://w3id.org/bot#";
    public static final String P_P2P_ITEM ="https://purl.org/p2p-o/item#"; 
    public static final String P_P2P_DOCLINE = "https://purl.org/p2p-o/documentline#"; 
    public static final String P_P2P_INVOICE = "https://purl.org/p2p-o/invoice#";
    public static final String P_TIME = "http://www.w3.org/2006/time#";
    
    public static final Prefix Pref_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    public static final Prefix Pref_LAB = SparqlBuilder.prefix("ontolab",iri(ONTOLAB));
    public static final Prefix Pref_SYS = SparqlBuilder.prefix("ontosystem",iri(ONTOSYSTEM));
    public static final Prefix Pref_INMA = SparqlBuilder.prefix("ontoinma",iri(ONTOINMA));
    public static final Prefix Pref_ASSET = SparqlBuilder.prefix("ontoassetmanagement",iri(ONTOASSET));
    public static final Prefix Pref_EPE = SparqlBuilder.prefix("ontoelecpowerequipment",iri(ONTOEPE));
    public static final Prefix Pref_BIM = SparqlBuilder.prefix("ontobim", iri(ONTOBIM));
    public static final Prefix Pref_EMS = SparqlBuilder.prefix("ontoems", iri(ONTOEMS));
    public static final Prefix Pref_SAREF = SparqlBuilder.prefix("saref", iri(P_SAREF));
    public static final Prefix Pref_OM = SparqlBuilder.prefix("om2", iri(P_OM));
    public static final Prefix Pref_FIBO_AAP = SparqlBuilder.prefix("FIBOaap", iri(P_FIBO_AAP));
    public static final Prefix Pref_FIBO_ORG_FORMAL = SparqlBuilder.prefix("FIBOorgFormal",iri(P_FIBO_ORG+"FormalOrganizations/"));
    public static final Prefix Pref_FIBO_ORG_ORGS = SparqlBuilder.prefix("FIBOorgOrg",iri(P_FIBO_ORG+"Organizations/"));
    public static final Prefix Pref_BOT = SparqlBuilder.prefix("bot", iri(P_BOT));
    public static final Prefix Pref_P2P_ITEM = SparqlBuilder.prefix("P2Pitem", iri(P_P2P_ITEM)); 
    public static final Prefix Pref_P2P_DOCLINE = SparqlBuilder.prefix("P2Pdocline", iri(P_P2P_DOCLINE)); 
    public static final Prefix Pref_P2P_INVOICE = SparqlBuilder.prefix("P2Pinvoice", iri(P_P2P_INVOICE)); 
    public static final Prefix Pref_TIME = SparqlBuilder.prefix("time", iri(P_TIME));


    //properties
    public static final Iri hasModel = Pref_SAREF.iri("hasModel");

    public static final Iri hasItemInventoryIdentifier = Pref_ASSET.iri("hasItemInventoryIdentifier");
    public static final Iri references = Pref_ASSET.iri("references");
    public static final Iri assignedTo = Pref_ASSET.iri("assignedTo");
    public static final Iri serialNumber = Pref_ASSET.iri("serialNumber");
    public static final Iri isStoredIn = Pref_ASSET.iri("isStoredIn");
    public static final Iri availableAt = Pref_ASSET.iri("availableAt");
    public static final Iri isSuppliedBy = Pref_ASSET.iri("isSuppliedBy");
    public static final Iri isManufacturedBy = Pref_ASSET.iri("isManufacturedBy");
    public static final Iri isLocatedIn = Pref_ASSET.iri("isLocatedIn");
    public static final Iri isLocatedAt = Pref_ASSET.iri("isLocatedAt");
    public static final Iri hasAllocatedWorkspace = Pref_ASSET.iri("hasAllocatedWorkspace");
    public static final Iri hasIdentifier = Pref_ASSET.iri("hasIdentifier");
    public static final Iri hasDeliveryOrderLine = Pref_ASSET.iri("hasDeliveryOrderLine");
    public static final Iri hasPurchaseOrderLine = Pref_ASSET.iri("hasPurchaseOrderLine");
    public static final Iri deliveryOrderNumber = Pref_ASSET.iri("deliveryOrderNumber");
    public static final Iri purchaseOrderNumber = Pref_ASSET.iri("purchaseOrderNumber");
    public static final Iri hasPriceDetails = Pref_ASSET.iri("hasPriceDetails");
    public static final Iri hasWorkspaceIdentifier = Pref_ASSET.iri("hasWorkspaceIdentifier");
    public static final Iri hasCurrentLocation = Pref_ASSET.iri("hasCurrentLocation");
    public static final Iri hasFurnitureIdentifier = Pref_ASSET.iri("hasFurnitureIdentifier");
    public static final Iri hasProjectIdentifier = Pref_ASSET.iri("hasProjectIdentifier");
    public static final Iri hasGrant = Pref_ASSET.iri("hasGrant");
    public static final Iri hasAccount = Pref_ASSET.iri("hasAccount");
    public static final Iri hasBudget = Pref_ASSET.iri("hasBudget");
    public static final Iri hasBudgetCategory = Pref_ASSET.iri("hasBudgetCategory");
    public static final Iri hasServiceCategory = Pref_ASSET.iri("hasServiceCategory");
    public static final Iri hasServiceCategoryIdentifier = Pref_ASSET.iri("hasServiceCategoryIdentifier");
    public static final Iri hasServiceCode = Pref_ASSET.iri("hasServiceCode");
    public static final Iri hasServiceCodeIdentifier = Pref_ASSET.iri("hasServiceCodeIdentifier");
    public static final Iri allocatedTo = Pref_ASSET.iri("allocatedTo");
    public static final Iri hasMaintenanceSchedule = Pref_ASSET.iri("hasMaintenanceSchedule");
    public static final Iri hasTask = Pref_ASSET.iri("hasTask");
    public static final Iri performedAt = Pref_ASSET.iri("performedAt");
    public static final Iri scheduledFor = Pref_ASSET.iri("scheduledFor");
    public static final Iri isPerformedBy = Pref_ASSET.iri("isPerformedBy");
    public static final Iri hasInverval = Pref_ASSET.iri("hasInverval");
    public static final Iri hasImageURI = Pref_ASSET.iri("hasImageURI");

    public static final Iri hasDataSheet = Pref_DEV.iri("hasDataSheet");
    public static final Iri hasPrice = Pref_DEV.iri("hasPrice");
    
    public static final Iri hasRoom = Pref_BIM.iri("hasRoom");
    public static final Iri hasIfcRepresentation = Pref_BIM.iri("hasIfcRepresentation");
    public static final Iri hasFacility = Pref_BIM.iri("hasFacility");

    public static final Iri hasValue = Pref_OM.iri("hasValue");
    public static final Iri hasUnit = Pref_OM.iri("hasUnit");
    public static final Iri hasNumericalValue = Pref_OM.iri("hasNumericalValue");

    public static final Iri itemName = Pref_P2P_ITEM.iri("itemName");
    public static final Iri hasAttribute = Pref_P2P_ITEM.iri("hasAttribute");
    public static final Iri attributeName = Pref_P2P_ITEM.iri("attributeName");
    public static final Iri attributeValue = Pref_P2P_ITEM.iri("attributeValue");
    public static final Iri invoiceNumber = Pref_P2P_INVOICE.iri("invoiceNumber");
    public static final Iri hasInvoiceLine = Pref_P2P_INVOICE.iri("hasInvoiceLine");
    public static final Iri hasItem = Pref_P2P_DOCLINE.iri("hasItem");
    public static final Iri InvoicedQuantity = Pref_P2P_DOCLINE.iri("InvoicedQuantity"); 

    public static final Iri hasPersonName = Pref_FIBO_AAP.iri("hasPersonName");
    public static final Iri hasLegalName = iri("https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/hasLegalName");

    public static final Iri hasName = iri("https://www.omg.org/spec/Commons/Designators/hasName");

    public static final Iri containsElement = Pref_BOT.iri("containsElement");
    public static final Iri hasStorey = Pref_BOT.iri("hasStorey");
    public static final Iri containsSystem = Pref_SYS.iri("containsSystem");

    public static final Iri hasDurationDescription = Pref_TIME.iri("hasDurationDescription");
    public static final Iri years = Pref_TIME.iri("years");
    public static final Iri months = Pref_TIME.iri("months");
    public static final Iri inXSDDateTimeStamp= Pref_TIME.iri("inXSDDateTimeStamp");


    //External classes
    public static final Iri Item = Pref_P2P_ITEM.iri("Item");
    
    public static final Iri SpecSheet = Pref_ASSET.iri("SpecSheet");
    public static final Iri Manual = Pref_ASSET.iri("Manual");
    public static final Iri Workspace = Pref_ASSET.iri("Workspace");
    public static final Iri ServiceCategory = Pref_ASSET.iri("ServiceCategory");
    public static final Iri DeliveryOrder = Pref_ASSET.iri("DeliveryOrder");
    public static final Iri DeliveryOrderLine = Pref_ASSET.iri("DeliveryOrderLine"); 
    public static final Iri PurchaseOrder = Pref_ASSET.iri("PurchaseOrder");
    public static final Iri PurchaseOrderLine = Pref_ASSET.iri("PurchaseOrderLine");
    public static final Iri E_Invoice = Pref_ASSET.iri("E-Invoice");
    public static final Iri InvoiceLine = Pref_ASSET.iri("InvoiceLine");
    public static final Iri PriceDetails = Pref_ASSET.iri("PriceDetails");
    public static final Iri HomeTotalDiscountedAfterTaxPrice = Pref_ASSET.iri("HomeTotalDiscountedAfterTaxPrice");
    public static final Iri MaintenanceSchedule = Pref_ASSET.iri("MaintenanceSchedule");
    public static final Iri MaintenanceTask = Pref_ASSET.iri("MaintenanceTask");
    public static final Iri hasInterval = Pref_ASSET.iri("hasInterval");

    public static final Iri Person = Pref_FIBO_AAP.iri("Person");
    public static final Iri PersonName = Pref_FIBO_AAP.iri("PersonName");
    public static final Iri FormalOrganization = Pref_FIBO_ORG_FORMAL.iri("FormalOrganization");
    public static final Iri OrganizationName = Pref_FIBO_ORG_ORGS.iri("OrganizationName");
    public static final Iri IndependentParty = iri("https://spec.edmcouncil.org/fibo/ontology/FND/Parties/Parties/IndependentParty");

    public static final Iri AmountOfMoney = Pref_OM.iri("AmountOfMoney");
    public static final Iri Measure = Pref_OM.iri("Measure");
    public static final Iri SingaporeDollar = Pref_OM.iri("SingaporeDollar");

    public static final Iri Instant = Pref_TIME.iri("Instant"); 
    public static final Iri Interval = Pref_TIME.iri("Interval");
    public static final Iri DurationDescription = Pref_TIME.iri("DurationDescription");
    
    public static final String BuiltInCabinetString = P_ASSET + "BuiltInCabinet";
    public static final String PedestalCabinetString = P_ASSET + "PedestalCabinet";
    public static final String CabinetString = P_ASSET + "Cabinet";

    public static final String SingaporeDollarString = P_OM + "SingaporeDollar";

     

    public ClassAndProperties(){

    }
}
