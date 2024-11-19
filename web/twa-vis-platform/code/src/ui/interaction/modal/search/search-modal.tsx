"use client";
import styles from './search.modal.module.css';

import React, { useEffect, useRef, useState } from 'react';
import Modal from 'react-modal';
import { useDispatch } from 'react-redux';

import { setFilterFeatureIris } from 'state/map-feature-slice';
import { SEARCH_FORM_TYPE } from 'types/form';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import { FormComponent } from 'ui/interaction/form/form';
import { HttpResponse } from 'utils/server-actions';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import ResponseComponent from 'ui/text/response/response';

interface SearchModalProps {
  id: string;
  stack: string;
  show: boolean,
  setShowState: React.Dispatch<React.SetStateAction<boolean>>;
}

export const SHOW_ALL_FEATURE_INDICATOR: string = "all";

/**
 * A modal component for users to interact with a form for search criterias while on the registry.
 */
export default function SearchModal(props: Readonly<SearchModalProps>) {
  Modal.setAppElement("#globalContainer");
  const dispatch = useDispatch();
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

  // Show all features upon click
  const showAllFeatures = () => {
    dispatch(setFilterFeatureIris([SHOW_ALL_FEATURE_INDICATOR]));
    setTimeout(() => props.setShowState(false), 1000);
  };

  // Closes the search modal only if response is successfull
  useEffect(() => {
    if (response?.success) {
      setTimeout(() => props.setShowState(false), 2000);
    }
  }, [response]);

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
            formType={SEARCH_FORM_TYPE}
            agentApi={`${props.stack}/vis-backend-agent`}
            setResponse={setResponse}
            onSubmittingChange={handleFormSubmittingChange}
          />
        </section>
        <section className={styles["section-footer"]}>
          {isSubmitting && <LoadingSpinner isSmall={false} />}
          {!isSubmitting && (<ResponseComponent response={response} />)}
          <div className={styles["footer-button-row"]}>
            <MaterialIconButton
              iconName={"search"}
              className={styles["section-footer-button"]}
              iconStyles={[styles["icon"]]}
              text={{
                styles: [styles["button-text"]],
                content: "SEARCH"
              }}
              onClick={onSubmit}
            />
            <MaterialIconButton
              iconName={"select_all"}
              className={styles["section-footer-button"]}
              iconStyles={[styles["icon"]]}
              text={{
                styles: [styles["button-text"]],
                content: "SHOW ALL"
              }}
              onClick={showAllFeatures}
            />
          </div>
        </section>
      </div>
    </Modal>
  );
}