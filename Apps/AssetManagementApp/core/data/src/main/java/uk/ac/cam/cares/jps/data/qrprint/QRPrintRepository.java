package uk.ac.cam.cares.jps.data.qrprint;

import java.util.List;

import io.reactivex.disposables.Disposable;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.datastore.QRPrintingLocalSource;
import uk.ac.cam.cares.jps.model.PrintItem;
import uk.ac.cam.cares.jps.network.qrprint.QRPrintingNetworkSource;

public class QRPrintRepository {
    QRPrintingLocalSource localSource;
    QRPrintingNetworkSource networkSource;
    Disposable disposable;

    public QRPrintRepository(QRPrintingLocalSource localSource, QRPrintingNetworkSource networkSource) {
        this.localSource = localSource;
        this.networkSource = networkSource;
    }

    public void getAllPrintItems(RepositoryCallback<List<PrintItem>> callback) {

        disposable = localSource.getPrintItemFlow().subscribe(
                items -> {
                    callback.onSuccess(items);
                    disposable.dispose();
                },
                callback::onFailure
        );
    }


    public void updatePrintList(List<PrintItem> items) {
        localSource.writePrintItems(items);
    }

    public void updatePrintList(PrintItem item) {
        localSource.writePrintItems(item);
    }

    public void printQRCodes(List<PrintItem> items, RepositoryCallback<Boolean> callback) {
        networkSource.bulkPrintQRCodes(items, callback::onSuccess, callback::onFailure);
    }

}
