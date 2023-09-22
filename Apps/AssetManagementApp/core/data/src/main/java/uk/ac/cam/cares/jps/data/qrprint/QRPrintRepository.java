package uk.ac.cam.cares.jps.data.qrprint;

import java.util.List;

import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.datastore.QRPrintingLocalSource;
import uk.ac.cam.cares.jps.model.PrintItem;

public class QRPrintRepository {
    QRPrintingLocalSource localSource;
    Disposable disposable;

    public QRPrintRepository(QRPrintingLocalSource localSource) {
        this.localSource = localSource;
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

}
