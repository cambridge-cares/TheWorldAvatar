package uk.ac.cam.cares.jps.qrprint;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.PrintItemModel;
import uk.ac.cam.cares.jps.data.QRPrintRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;

@HiltViewModel
public class QRPrintingViewModel extends ViewModel {

    private Logger LOGGER = Logger.getLogger(QRPrintingViewModel.class);
    private MutableLiveData<List<PrintItemModel>> printingList = new MutableLiveData<>(new ArrayList<>());
    private MutableLiveData<List<PrintItemModel>> unprintedList = new MutableLiveData<>(new ArrayList<>());
    private MutableLiveData<List<PrintItemModel>> printedList = new MutableLiveData<>(new ArrayList<>());

    private QRPrintRepository repository;

    @Inject
    QRPrintingViewModel(QRPrintRepository repository) {
        this.repository = repository;
    }

    public MutableLiveData<List<PrintItemModel>> getPrintingList() {
        return printingList;
    }

    public MutableLiveData<List<PrintItemModel>> getUnprintedList() {
        return unprintedList;
    }

    public MutableLiveData<List<PrintItemModel>> getPrintedList() {
        return printedList;
    }

    public void addPrintingItem(PrintItemModel item) {
        List<PrintItemModel> newList = printingList.getValue();
        newList.add(item);
        printingList.setValue(newList);
    }

    public void removePrintingItem(PrintItemModel item) {
        List<PrintItemModel> newList = printingList.getValue();
        newList.remove(item);
        printingList.setValue(newList);
    }

    public void addPrintedItem(PrintItemModel item) {
        List<PrintItemModel> newList = printedList.getValue();
        newList.add(item);
        printedList.setValue(newList);
    }

    public void addUnprintedItem(PrintItemModel item) {
        List<PrintItemModel> newList = unprintedList.getValue();
        newList.add(item);
        unprintedList.setValue(newList);
    }

    public void removeUnprintedItem(PrintItemModel item) {
        List<PrintItemModel> newList = unprintedList.getValue();
        newList.remove(item);
        unprintedList.setValue(newList);
    }

    public void getAllPrintItems() {
        RepositoryCallback<List<PrintItemModel>> callback = new RepositoryCallback<List<PrintItemModel>>() {
            @Override
            public void onSuccess(List<PrintItemModel> result) {
                List<PrintItemModel> newPrintingList = new ArrayList<>();
                List<PrintItemModel> newPrintedList = new ArrayList<>();

                for (PrintItemModel item : result) {
                    if (item.getStatus()) {
                        newPrintedList.add(item);
                    } else {
                        newPrintingList.add(item);
                    }
                }
                printingList.postValue(newPrintingList);
                printedList.postValue(newPrintedList);
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error("not able to get printing list from repository");
            }
        };
        repository.getAllPrintItems(callback);
    }
}
