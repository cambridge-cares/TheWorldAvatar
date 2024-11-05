"use client";

import styles from './form.module.css';

import React, { useRef, useState } from 'react';
import { useDispatch } from 'react-redux';
import { usePathname, useRouter } from 'next/navigation';

import useRefresh from 'hooks/useRefresh';
import { Paths } from 'io/config/routes';
import { setIsOpen } from 'state/modal-slice';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { FormComponent } from 'ui/interaction/form/form';
import ReturnButton from 'ui/navigation/return/return';
import ResponseComponent from 'ui/text/response/response';
import { HttpResponse } from 'utils/server-actions';
import { getAfterDelimiter } from 'utils/client-utils';

interface FormContainerComponentProps {
  entityType: string;
  formType: string;
  agentApi: string;
}

/**
 * Renders a form container page for entities.
 * 
 * @param {string} entityType The type of entity.
 * @param {string} formType The type of form such as add, update, delete, and view.
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 */
export default function FormContainerComponent(props: Readonly<FormContainerComponentProps>) {
  const router = useRouter();
  const dispatch = useDispatch();

  const [refreshFlag, triggerRefresh] = useRefresh();
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

  const onReturn = () => {
    dispatch(setIsOpen(false));
    router.back();
  };

  const showReturnButton: boolean = props.formType === Paths.REGISTRY || !!response;
  const id: string = getAfterDelimiter(usePathname(), "/");

  // An event handler that will navigate to the required form when clicked
  const openDeleteModal = (e: React.MouseEvent) => {
    e.preventDefault();
    const url: string = `../../delete/${props.entityType}/${id}`;
    router.push(url);
  };

  const openEditModal = (e: React.MouseEvent) => {
    e.preventDefault();
    const url: string = `../../edit/${props.entityType}/${id}`;
    router.push(url);
  };

  const closeTab = () => {
    window.close(); // Closes the tab
    router.back(); // Required to close the intercepted modal as the tab cannot be closed
  };

  return (
    <div className={styles["container"]}>
      <div className={`${styles["form-title"]} ${styles["form-row"]}`}>
        <ReturnButton />
        <span>{`${props.formType.toUpperCase()} ${props.entityType.toUpperCase().replace("_", " ")}`}</span>
      </div>
      <div className={styles["form-contents"]}>
        {refreshFlag ? <LoadingSpinner isSmall={false} /> :
          <FormComponent
            formRef={formRef}
            entityType={props.entityType}
            formType={props.formType}
            agentApi={props.agentApi}
            setResponse={setResponse}
            onSubmittingChange={handleFormSubmittingChange}
          />
        }
      </div>
      <div className={styles["form-footer"]}>
        {!isSubmitting && !response && <MaterialIconButton
          iconName={"cached"}
          iconStyles={[styles["form-button-icon"]]}
          onClick={triggerRefresh}
        />}
        {isSubmitting && <LoadingSpinner isSmall={false} />}
        {!isSubmitting && response && (<ResponseComponent response={response} />)}
        <div className={styles["form-row"]}>
          {props.formType === Paths.REGISTRY && <MaterialIconButton
            iconName={"edit"}
            className={styles["form-button"]}
            iconStyles={[styles["form-button-icon"]]}
            text={{
              styles: [styles["form-button-text"]],
              content: "EDIT"
            }}
            onClick={openEditModal}
          />}
          {props.formType === Paths.REGISTRY && <MaterialIconButton
            iconName={"delete"}
            className={styles["form-button"]}
            iconStyles={[styles["form-button-icon"]]}
            text={{
              styles: [styles["form-button-text"]],
              content: "DELETE"
            }}
            onClick={openDeleteModal}
          />
          }
          <MaterialIconButton
            iconName={showReturnButton ? "logout" : "publish"}
            className={styles["form-button"]}
            iconStyles={[styles["form-button-icon"]]}
            text={{
              styles: [styles["form-button-text"]],
              content: showReturnButton ? "RETURN" : "SUBMIT"
            }}
            onClick={showReturnButton ? props.formType === Paths.REGISTRY_DELETE ? closeTab : onReturn : onSubmit}
          />
        </div>
      </div>
    </div>
  );
}
