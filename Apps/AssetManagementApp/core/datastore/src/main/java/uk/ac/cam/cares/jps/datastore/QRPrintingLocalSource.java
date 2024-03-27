package uk.ac.cam.cares.jps.datastore;

import android.content.Context;

import androidx.datastore.rxjava2.RxDataStore;
import androidx.datastore.rxjava2.RxDataStoreBuilder;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.stream.Collectors;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.Flowable;
import io.reactivex.Single;
import uk.ac.cam.cares.jps.model.PrintItem;

public class QRPrintingLocalSource {
    private static final Logger LOGGER = Logger.getLogger(QRPrintingLocalSource.class);
    private Flowable<List<PrintItem>> printItemFlow;
    private RxDataStore<ItemList> printItemsStore;

    public QRPrintingLocalSource(@ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();
        printItemsStore = new RxDataStoreBuilder<>(applicationContext, "QR_Item_List.pb", new ItemListSerializer()).build();
        printItemFlow = printItemsStore.data().map(itemList ->
                itemList.getItemsList()
                        .stream().map(protoPrintItem ->
                        new PrintItem(protoPrintItem.getInventoryID(),
                                protoPrintItem.getLabel(),
                                protoPrintItem.getIri()))
                        .collect(Collectors.toList()));
    }

    public Flowable<List<PrintItem>> getPrintItemFlow() {
        return printItemFlow;
    }

    public void writePrintItems(List<PrintItem> items) {
        // convert the model PrintItem to protobuf's model
        List<uk.ac.cam.cares.jps.datastore.PrintItem> protoItemList = items.stream()
                .map(item -> uk.ac.cam.cares.jps.datastore.PrintItem.newBuilder()
                        .setIri(item.getIri())
                        .setInventoryID(item.getInventoryID())
                        .setLabel(item.getLabel())
                        .build())
                .collect(Collectors.toList());

        printItemsStore.updateDataAsync(itemList -> {
            itemList = itemList.toBuilder().clearItems().addAllItems(protoItemList).build();
            return Single.just(itemList);
        });
    }

    public void writePrintItems(PrintItem item) {
        uk.ac.cam.cares.jps.datastore.PrintItem protoItem = uk.ac.cam.cares.jps.datastore.PrintItem.newBuilder()
                .setIri(item.getIri())
                .setInventoryID(item.getInventoryID())
                .setLabel(item.getLabel())
                .build();

        printItemsStore.updateDataAsync(itemList -> {
            itemList = itemList.toBuilder().addItems(protoItem).build();
            return Single.just(itemList);
        });
    }
}
