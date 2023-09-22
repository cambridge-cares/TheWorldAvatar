package uk.ac.cam.cares.jps.mailbox;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.android.volley.Response;

import org.apache.log4j.Logger;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.mail.Mail;
import uk.ac.cam.cares.jps.data.mail.MailRepository;

@HiltViewModel
public class MailboxViewModel extends ViewModel {

    private static final Logger LOGGER = Logger.getLogger(MailboxViewModel.class);
    private final MutableLiveData<List<Mail>> mailList = new MutableLiveData<>();
    private final MutableLiveData<String> error = new MutableLiveData<>();
    MailRepository repository;

    @Inject
    public MailboxViewModel(MailRepository repository) {
        this.repository = repository;
    }

    public MutableLiveData<List<Mail>> getMailList() {
        return mailList;
    }

    public MutableLiveData<String> getError() {
        return error;
    }

    public Mail getMailByIri(String iri) {
        return null;
    }

    public void retrieveMailListFromRepository() {
        // todo: see whether it is better to not use volley.Response in ui component
        Response.Listener<List<Mail>> onSuccess = response -> mailList.postValue(response);
        Response.ErrorListener onError = error -> {
            LOGGER.error(error.getMessage());
            this.error.postValue(String.valueOf(R.string.network_error));
        };
        repository.getMailList(onSuccess, onError);
    }
}
