package uk.ac.cam.cares.jps.data.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.ASSIGNED_TO;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.BUILDING;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.DELIVERY_ORDER_NUMBER;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.FACILITY;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.INVOICE_NUMBER;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.COMMENTS;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_COMMENT;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_FILE_URI;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_URL;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUFACTURER;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MODEL_NUMBER;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.PURCHASE_ORDER_NUMBER;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.PURCHASE_PRICE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.REFERENCE_LABEL;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.ROOM;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SERIAL_NUMBER;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SERVICE_CATEGORY;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SERVICE_CODE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_COMMENT;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_FILE_URI;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_PAGE_NO;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.STORED_IN;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.TYPE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.VENDOR;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.WORKSPACE;

import com.android.volley.ServerError;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.disposables.Disposable;
import io.reactivex.exceptions.UndeliverableException;
import io.reactivex.plugins.RxJavaPlugins;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.setting.SettingRepository;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.network.assetinfo.AssetNetworkSource;
import uk.ac.cam.cares.jps.network.datasheet.DataSheetNetworkSource;

public class AssetInfoRepository {
    private final Logger LOGGER = Logger.getLogger(AssetInfoRepository.class);

    AssetNetworkSource assetNetworkSource;
    DataSheetNetworkSource dataSheetNetworkSource;
    SettingRepository settingRepository;
    List<String> visibleProperties = new ArrayList<>();
    AssetInfo assetInfo;
    Map<String, String> keyConversionTable = getKeyConversionTable();

    @Inject
    public AssetInfoRepository(AssetNetworkSource assetNetworkSource, SettingRepository settingRepository, DataSheetNetworkSource dataSheetNetworkSource) {
        this.assetNetworkSource = assetNetworkSource;
        this.settingRepository = settingRepository;
        this.dataSheetNetworkSource = dataSheetNetworkSource;
    }

    public void getAssetInfoByIri(String iri, RepositoryCallback<AssetInfo> callback) {
        Completable assetNetworkCall = Completable.create(emitter -> assetNetworkSource.getAssetInfoByIri(iri, asset -> {
            assetInfo = new AssetInfo(asset.getProperties());
            emitter.onComplete();
        }, emitter::onError));

        Completable settingLocalCall = Completable.create(emitter -> settingRepository.getSettings(new RepositoryCallback<Map<String, Integer>>() {
            @Override
            public void onSuccess(Map<String, Integer> result) {
                visibleProperties.clear();
                for (Map.Entry<String, Integer> entry : result.entrySet()) {
                    if (Arrays.asList("inAppNotification", "mail").contains(entry.getKey())) {
                        continue;
                    }

                    if (entry.getValue() == 1) {
                        visibleProperties.add(entry.getKey());
                    }
                }
                emitter.onComplete();
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error(error.getMessage());
                // use default settings
                visibleProperties = new ArrayList<>(settingRepository.getDefaultSettings().keySet());
                emitter.onComplete();
            }
        }));

        Completable combinedCompletable = Completable.mergeArray(assetNetworkCall, settingLocalCall);
        Disposable disposable = combinedCompletable.subscribe(
                () -> {
                    LOGGER.info("Both asset and setting async call are return");
                    AssetInfo newAssetInfo = getAssetInfoWithVisibleProperties();
                    callback.onSuccess(newAssetInfo);
                },
                error -> {
                    LOGGER.error(error.getMessage());
                    if (error instanceof ServerError && ((ServerError)error).networkResponse.statusCode == 500) {
                        callback.onFailure(new Throwable("Invalid IRI"));
                    } else {
                        callback.onFailure(new Throwable("Network Issue"));
                    }
                }
        );
    }

    private AssetInfo getAssetInfoWithVisibleProperties() {
        AssetInfo result = new AssetInfo();
        for (String visibleProperties : visibleProperties) {
            if (assetInfo.getProperty(visibleProperties) != null) {
                result.addProperties(visibleProperties, assetInfo.getProperty(visibleProperties));
            }
        }
        result.addProperties(HAS_TIME_SERIES, assetInfo.getProperty(HAS_TIME_SERIES));
        return  result;
    }

    public void createNewAsset(AssetInfo assetInfo, RepositoryCallback<JSONObject> callback) {
        try {
            JSONObject assetData = new JSONObject();
            assetData.put(keyConversionTable.get("Prefix"), "");
            assetData.put(keyConversionTable.get(TYPE), assetInfo.getProperty(TYPE));
            assetData.put(keyConversionTable.get("ID"), "");
            assetData.put(keyConversionTable.get(REFERENCE_LABEL), assetInfo.getProperty(REFERENCE_LABEL));

            assetData.put(keyConversionTable.get(SERIAL_NUMBER), assetInfo.getProperty(SERIAL_NUMBER));
            assetData.put(keyConversionTable.get(MODEL_NUMBER), assetInfo.getProperty(MODEL_NUMBER));
            assetData.put(keyConversionTable.get(VENDOR), assetInfo.getProperty(VENDOR));
            assetData.put(keyConversionTable.get(MANUFACTURER), assetInfo.getProperty(MANUFACTURER));

            assetData.put(keyConversionTable.get("SpecSheet"), "");
            assetData.put(keyConversionTable.get(SPEC_SHEET_PAGE_NO), assetInfo.getProperty(SPEC_SHEET_PAGE_NO));
            assetData.put(keyConversionTable.get("Manual"), "");
            assetData.put(keyConversionTable.get(MANUAL_URL), assetInfo.getProperty(MANUAL_URL));

            assetData.put(keyConversionTable.get(BUILDING), assetInfo.getProperty(BUILDING));
            assetData.put(keyConversionTable.get(FACILITY), assetInfo.getProperty(FACILITY));
            assetData.put(keyConversionTable.get(ROOM), assetInfo.getProperty(ROOM));
            assetData.put(keyConversionTable.get(WORKSPACE), assetInfo.getProperty(WORKSPACE));
            assetData.put(keyConversionTable.get(STORED_IN), assetInfo.getProperty(STORED_IN));

            assetData.put(keyConversionTable.get(ASSIGNED_TO), assetInfo.getProperty(ASSIGNED_TO));

            assetData.put(keyConversionTable.get(COMMENTS), assetInfo.getProperty(COMMENTS));
            assetData.put(keyConversionTable.get(INVOICE_NUMBER), assetInfo.getProperty(INVOICE_NUMBER));
            assetData.put(keyConversionTable.get(DELIVERY_ORDER_NUMBER), assetInfo.getProperty(DELIVERY_ORDER_NUMBER));
            assetData.put(keyConversionTable.get(PURCHASE_ORDER_NUMBER), assetInfo.getProperty(PURCHASE_ORDER_NUMBER));
            assetData.put(keyConversionTable.get(SERVICE_CATEGORY), assetInfo.getProperty(SERVICE_CATEGORY));
            assetData.put(keyConversionTable.get(SERVICE_CODE), assetInfo.getProperty(SERVICE_CODE));
            assetData.put(keyConversionTable.get(PURCHASE_PRICE), assetInfo.getProperty(PURCHASE_PRICE));

            JSONObject param = new JSONObject();
            param.put("assetData", assetData);

            assetNetworkSource.addAsset(param, response -> addDataSheet(assetInfo, response, callback), callback::onFailure);

        } catch (JSONException e) {
            throw new RuntimeException(e);
        }

    }

    private Map<String, String> getKeyConversionTable() {
        // AssetInfo key to agent request key
        Map<String, String> table = new HashMap<>();
        table.put("Prefix", "Prefix");      // leave blank
        table.put(TYPE, "AssetClass");
        table.put("ID", "ID");              // leave blank
        table.put(REFERENCE_LABEL, "Name");

        table.put(SERIAL_NUMBER, "serialNum");
        table.put(MODEL_NUMBER, "modelNumber");
        table.put(VENDOR, "SupplierName");
        table.put(MANUFACTURER, "ManufacturerName");

        table.put("SpecSheet", "SpecSheet");    // leave blank
        table.put(SPEC_SHEET_PAGE_NO, "SpecSheetPage");
        table.put("Manual", "Manual");      // leave blank
        table.put(MANUAL_URL, "ManualURL");

        table.put(BUILDING, "BuildingLocation");
        table.put(FACILITY, "FacilityLocation");
        table.put(ROOM, "RoomLocation");
        table.put(WORKSPACE, "WorkspaceName");
        table.put(STORED_IN, "storage");

        table.put(ASSIGNED_TO, "AssignedTo");

        table.put(COMMENTS, "ItemComment");
        table.put(INVOICE_NUMBER, "invoiceNum");
        table.put(DELIVERY_ORDER_NUMBER, "DeliveryOrderNum");
        table.put(PURCHASE_ORDER_NUMBER, "PurchaseOrderNum");
        table.put(SERVICE_CATEGORY, "BudgetCat");
        table.put(SERVICE_CODE, "ServiceCode");
        table.put(PURCHASE_PRICE, "price");

        return table;
    }

    private void addDataSheet(AssetInfo assetInfo, JSONObject newElement, RepositoryCallback<JSONObject> callback) {
        RxJavaPlugins.setErrorHandler(throwable -> {
            if (throwable instanceof UndeliverableException) {
                LOGGER.info("Both network call failed. Ignore this RxJava exception.");
            } else {
                if (Thread.currentThread().getUncaughtExceptionHandler() != null) {
                    Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), throwable);
                }
            }
        });

        try {
            String id = newElement.getString("ID");

            Completable specSheetNetworkCall = Single.just("").flatMapCompletable(s -> Completable.complete());
            if (!assetInfo.getProperty(SPEC_SHEET_FILE_URI).isEmpty()) {
                JSONObject specSheetData = new JSONObject();
                specSheetData.put("targetID", id);
                specSheetData.put("comments", assetInfo.getProperty(SPEC_SHEET_COMMENT));
                specSheetData.put("documentType", "SpecSheet");
                specSheetData.put("fileUri", assetInfo.getProperty(SPEC_SHEET_FILE_URI));
                specSheetNetworkCall = Completable.create(emitter -> dataSheetNetworkSource.addDataSheet(specSheetData, isSuccess -> emitter.onComplete(), emitter::onError));
            }

            Completable manualNetworkCall = Single.just("").flatMapCompletable(s -> Completable.complete());
            if (!assetInfo.getProperty(MANUAL_FILE_URI).isEmpty()) {
                JSONObject manualData = new JSONObject();
                manualData.put("targetID", id);
                manualData.put("comments", assetInfo.getProperty(MANUAL_COMMENT));
                manualData.put("documentType", "Manual");
                manualData.put("fileUri", assetInfo.getProperty(MANUAL_FILE_URI));
                manualNetworkCall = Completable.create(emitter -> dataSheetNetworkSource.addDataSheet(manualData, isSuccess -> emitter.onComplete(), emitter::onError));
            }

            Completable combinedCompletable = Completable.mergeArray(specSheetNetworkCall, manualNetworkCall);
            Disposable disposable = combinedCompletable.subscribe(() -> {
                callback.onSuccess(newElement);
            }, error -> {
                callback.onFailure(error);
            });

        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    public void setAssetInfo(AssetInfo assetInfo) {
        this.assetInfo = assetInfo;
    }

    public AssetInfo getAssetInfo() {
        return this.assetInfo;
    }
}