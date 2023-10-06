package uk.ac.cam.cares.jps.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AssetInfoConstant {
    // BASIC INFO KEYS
    static public final String BASIC_SECTION_TITLE = "Basic";
    static public final String REFERENCE_LABEL = "Reference Label";
    static public final String TYPE = "Type";
    static public final String ASSIGNED_TO = "Assigned to";
    static public final String IRI = "IRI";
    static public final String INVENTORY_ID = "Inventory ID";

    // LOCATION KEYS
    static public final String LOCATION_SECTION_TITLE = "Location";
    static public final String BUILDING = "Building";
    static public final String FACILITY = "Facility";
    static public final String LOCATED_IN = "Located in";
    static public final String SEAT_LOCATION = "Seat Location";
    static public final String STORED_IN = "Stored in";

    // SUPPLIER & MANUFACTURE KEYS
    static public final String SUPPLIER_SECTION_TITLE = "Supplier";
    static public final String VENDOR = "Vendor";
    static public final String MANUFACTURER = "Manufacturer";
    static public final String MANUFACTURE_URL = "Manufacture URL";
    static public final String SERIAL_NUMBER = "Serial No.";
    static public final String MODEL_NUMBER = "Model No.";

    // ITEM KEYS
    static public final String ITEM_SECTION_TITLE = "Item";
    static public final String PURCHASE_PRICE = "Purchase Price";
    static public final String ITEM_NAME = "Item Name";
    static public final String ITEM_DESCRIPTION = "Item Description";
    static public final String PRICE_UNIT = "Unit";
    static public final String SERVICE_CATEGORY = "Service Category";
    static public final String SERVICE_CODE = "Service Code";

    // DOC LINE INFO KEYS
    static public final String PURCHASE_SECTION_TITLE = "Purchase";
    static public final String PURCHASE_REQUEST_NUMBER = "Purchase Request No.";
    static public final String PURCHASE_ORDER_NUMBER = "Purchase Order No.";
    static public final String INVOICE_NUMBER = "Invoice No.";
    static public final String DELIVERY_ORDER_NUMBER = "Delivery Order No.";

    // DATASHEET KEYS
    static public final String SPEC_SHEET_SECTION_TITLE = "Spec Sheet";
    static public final String SPEC_SHEET_PAGE_NO = "Spec Sheet Page No.";
    static public final String SPEC_SHEET = "Spec Sheet";
    static public final String SPEC_SHEET_FILE_URI = "Spec Sheet File Uri";

    static public final String MANUAL_SECTION_TITLE = "Manual";
    static public final String MANUAL_URL = "Manual URL";
    static public final String MANUAL = "Manual";
    static public final String MANUAL_FILE_URI = "Manual File Uri";

    // MAINTENANCE KEYS
    static public final String MAINTENANCE_SECTION_TITLE = "Maintenance";
    static public final String MAINTENANCE_SCHEDULE_FOR = "Maintenance scheduled for";
    static public final String MAINTENANCE_MAINTAINED_BY = "Maintained by";
    static public final String MAINTENANCE_INTERVAL = "Maintenance Interval";

    static public final String OTHERS_SECTION_TITLE = "Others";

    // ORDERED KEYS
    static public final List<String> basicInfoOrder = new ArrayList<>(Arrays.asList(REFERENCE_LABEL, TYPE, ASSIGNED_TO, IRI, INVENTORY_ID));
    static public final List<String> locationInfoOrder = new ArrayList<>(Arrays.asList(BUILDING, FACILITY, LOCATED_IN, SEAT_LOCATION, STORED_IN));
    static public final List<String> supplierInfoOrder = new ArrayList<>(Arrays.asList(VENDOR, MANUFACTURER, MANUFACTURE_URL, SERIAL_NUMBER, MODEL_NUMBER));
    static public final List<String> priceInfoOrder = new ArrayList<>(Arrays.asList(PURCHASE_PRICE));
    static public final List<String> itemInfoOrder = new ArrayList<>(Arrays.asList(ITEM_NAME, ITEM_DESCRIPTION, PURCHASE_PRICE, PRICE_UNIT, SERVICE_CATEGORY, SERVICE_CODE));
    static public final List<String> docLineInfoOrder = new ArrayList<>(Arrays.asList(PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER));
    static public final List<String> maintenanceInfoOrder = new ArrayList<>(Arrays.asList(MAINTENANCE_SCHEDULE_FOR, MAINTENANCE_MAINTAINED_BY, MAINTENANCE_INTERVAL));
    static public final List<String> manualInfoOrder = new ArrayList<>(Arrays.asList(MANUAL, MANUAL_URL));
    static public final List<String> specSheetInfoOrder = new ArrayList<>(Arrays.asList(SPEC_SHEET, SPEC_SHEET_PAGE_NO));


//    static public final List<String> otherInfoFromAssetAgentKeys = Arrays.asList(TYPE, ASSIGNED_TO, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER, BUILDING, FACILITY, LOCATED_IN, SEAT_LOCATION, STORED_IN);
    static public final List<String> otherInfoFromAssetAgentKeys = Arrays.asList(TYPE, ASSIGNED_TO, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER);
    static public final String HAS_TIME_SERIES = "hasTimeSeries";
}
