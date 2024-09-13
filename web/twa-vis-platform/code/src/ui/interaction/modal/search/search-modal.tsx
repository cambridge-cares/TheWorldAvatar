"use client";
import styles from './search.modal.module.css';

import React, { useRef, useState } from 'react';
import Modal from 'react-modal';

import MaterialIconButton from 'ui/graphic/icon/icon-button';
import { FormComponent } from 'ui/interaction/form/form';
import { PathNames } from 'io/config/routes';
import { HttpResponse } from 'utils/server-actions';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import ResponseComponent from 'ui/text/response/response';

interface SearchModalProps {
  id: string;
  stack: string;
  show: boolean,
  setShowState: React.Dispatch<React.SetStateAction<boolean>>;
}

/**
 * A modal component for users to interact with a form for search criterias while on the registry.
 */
export default function SearchModal(props: Readonly<SearchModalProps>) {
  Modal.setAppElement("#globalContainer");
  const [isSubmitting, setIsSubmitting] = useState<boolean>(false);
  const [response, setResponse] = useState<HttpResponse>(null);
  const formRef: React.MutableRefObject<HTMLFormElement> = useRef<HTMLFormElement>();

  const handleFormSubmittingChange = (submitting: boolean) => {
    setIsSubmitting(submitting);
  };

  const onSubmit = () => {
    if (formRef.current) {
      formRef.current.requestSubmit();
    }
  };

  return (
    <Modal
      isOpen={props.show}
      overlayClassName={styles.overlay}
      className={styles.modal}
    >
      <div className={styles.container}>
        <section className={styles["section-title"]}>
          <h1>SEARCH CRITERIA</h1>
          <button type="button" className={styles["close-button"]} onClick={() => props.setShowState(false)}>
            <MaterialIconButton
              iconName="cancel"
              iconStyles={[styles["icon"]]}
            />
          </button>
        </section>
        <section className={styles["section-contents"]}>
          <FormComponent
            formRef={formRef}
            entityType={props.id}
            formType={PathNames.SEARCH}
            agentApi={`${props.stack}/vis-backend-agent`}
            setResponse={setResponse}
            onSubmittingChange={handleFormSubmittingChange}
          />
        </section>
        <section className={styles["section-footer"]}>
          {isSubmitting && <LoadingSpinner isSmall={false} />}
          {!isSubmitting && (<ResponseComponent response={response} />)}
          <MaterialIconButton
            iconName={"search"}
            className={styles["section-footer-button"]}
            iconStyles={[styles["icon"]]}
            text={{
              styles: [styles["button-text"]],
              content: "SUBMIT"
            }}
            onClick={onSubmit}
          />
        </section>
      </div>
    </Modal>
  );
}