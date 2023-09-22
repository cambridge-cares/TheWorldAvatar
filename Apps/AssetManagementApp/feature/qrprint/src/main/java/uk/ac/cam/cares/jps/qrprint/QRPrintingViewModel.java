package uk.ac.cam.cares.jps.qrprint;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.qrprint.QRPrintRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.model.PrintItem;

@HiltViewModel
public class QRPrintingViewModel extends ViewModel {

    private Logger LOGGER = Logger.getLogger(QRPrintingViewModel.class);
    private MutableLiveData<List<PrintItem>> printingList = new MutableLiveData<>(new ArrayList<>());
    private MutableLiveData<List<PrintItem>> unprintedList = new MutableLiveData<>(new ArrayList<>());
//    private MutableLiveData<List<PrintItemModel>> printedList = new MutableLiveData<>(new ArrayList<>());

    private QRPrintRepository repository;

    @Inject
    QRPrintingViewModel(QRPrintRepository repository) {
        this.repository = repository;
    }

    public MutableLiveData<List<PrintItem>> getPrintingList() {
        return printingList;
    }

    public MutableLiveData<List<PrintItem>> getUnprintedList() {
        return unprintedList;
    }

//    public MutableLiveData<List<PrintItemModel>> getPrintedList() {
//        return printedList;
//    }

    public void addPrintingItem(PrintItem item) {
        List<PrintItem> newList = printingList.getValue();
        newList.add(item);
        printingList.setValue(newList);
    }

    public void removePrintingItem(PrintItem item) {
        List<PrintItem> newList = printingList.getValue();
        newList.remove(item);
        printingList.setValue(newList);
    }

//    public void addPrintedItem(PrintItemModel item) {
//        List<PrintItemModel> newList = printedList.getValue();
//        newList.add(item);
//        printedList.setValue(newList);
//    }

    public void addUnprintedItem(PrintItem item) {
        List<PrintItem> newList = unprintedList.getValue();
        newList.add(item);
        unprintedList.setValue(newList);
    }

    public void removeUnprintedItem(PrintItem item) {
        List<PrintItem> newList = unprintedList.getValue();
        newList.remove(item);
        unprintedList.setValue(newList);
    }

    public void getAllPrintItems() {
        RepositoryCallback<List<PrintItem>> callback = new RepositoryCallback<List<PrintItem>>() {
            @Override
            public void onSuccess(List<PrintItem> result) {
                List<PrintItem> newPrintingList = new ArrayList<>();
//                List<PrintItemModel> newPrintedList = new ArrayList<>();

                for (PrintItem item : result) {
                    if (item.getStatus()) {
//                        newPrintedList.add(item);
                    } else {
                        newPrintingList.add(item);
                    }
                }
                printingList.postValue(newPrintingList);
//                printedList.postValue(newPrintedList);
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error("not able to get printing list from repository");
            }
        };
        repository.getAllPrintItems(callback);
    }

    public void printSelectedItems() {
        // todo: send request to agent to print the list of qr codes

        // update the local storage for unprinted items
        repository.updatePrintList(unprintedList.getValue());
        printingList.setValue(new ArrayList<>());
    }
}
