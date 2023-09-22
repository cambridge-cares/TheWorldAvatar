package uk.ac.cam.cares.jps.data.qrprint;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.datastore.QRPrintingLocalSource;

public class QRPrintRepository {
    List<PrintItem> items = new ArrayList<>();
    QRPrintingLocalSource localSource;

    public QRPrintRepository(QRPrintingLocalSource localSource) {
        this.localSource = localSource;
    }

    public void getAllPrintItems(RepositoryCallback<List<PrintItem>> callback) {
        items.add(new PrintItem("2021-02-08/961", "Monitor", "iri", false));
        items.add(new PrintItem("2021-02-00/967", "Coffee Machine", "iri", false));
        items.add(new PrintItem("2021-01-08/950", "Monitor2", "iri", false));
        items.add(new PrintItem("2023-02-05/955", "Laptop", "iri", true));
        items.add(new PrintItem("2020-02-01/961", "Server", "iri", true));
        callback.onSuccess(items);
    }
}
