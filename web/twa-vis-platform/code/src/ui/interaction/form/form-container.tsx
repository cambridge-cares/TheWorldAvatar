"use client";

import styles from './form.module.css';

import React, { useRef, useState } from 'react';
import { useDispatch } from 'react-redux';
import { useRouter } from 'next/navigation';

import { Paths } from 'io/config/routes';
import { setIsOpen } from 'state/modal-slice';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { FormComponent } from 'ui/interaction/form/form';
import ReturnButton from 'ui/navigation/return/return';
import ResponseComponent from 'ui/text/response/response';
import { HttpResponse } from 'utils/server-actions';

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

  return (
    <div className={styles["container"]}>
      <div className={styles["form-title"]}>
        <ReturnButton />
        <span>{`${props.formType.toUpperCase()} ${props.entityType.toUpperCase().replace("_", " ")}`}</span>
      </div>
      <div className={styles["form-contents"]}>
        <FormComponent
          formRef={formRef}
          entityType={props.entityType}
          formType={props.formType}
          agentApi={props.agentApi}
          setResponse={setResponse}
          onSubmittingChange={handleFormSubmittingChange}
        />
      </div>
      <div className={styles["form-footer"]}>
        {isSubmitting && <LoadingSpinner isSmall={false} />}
        {!isSubmitting && (<ResponseComponent response={response} />)}
        <MaterialIconButton
          iconName={showReturnButton ? "logout" : "publish"}
          className={styles["form-button"]}
          iconStyles={[styles["form-button-icon"]]}
          text={{
            styles: [styles["form-button-text"]],
            content: showReturnButton ? "RETURN" : "SUBMIT"
          }}
          onClick={showReturnButton ? onReturn : onSubmit}
        />
      </div>
    </div>
  );
}
