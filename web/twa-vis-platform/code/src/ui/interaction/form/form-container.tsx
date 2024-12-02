"use client";

import styles from './form.module.css';

import React, { useEffect, useRef, useState } from 'react';
import { useDispatch } from 'react-redux';
import { usePathname, useRouter } from 'next/navigation';

import useRefresh from 'hooks/useRefresh';
import { Paths } from 'io/config/routes';
import { setIsOpen } from 'state/modal-slice';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import { FormComponent } from 'ui/interaction/form/form';
import ActionButton from 'ui/interaction/action/action';
import ReturnButton from 'ui/navigation/return/return';
import ResponseComponent from 'ui/text/response/response';
import { HttpResponse, sendGetRequest, sendPostRequest } from 'utils/server-actions';
import { getAfterDelimiter } from 'utils/client-utils';
import { genBooleanClickHandler } from 'utils/event-handler';
import { ENTITY_STATUS, FORM_STATES } from './form-utils';
import { ApiResponse, JsonObject } from 'types/json';
import { FormTemplate } from './template/form-template';
import { remarksShape } from 'types/form';
import { FieldValues, SubmitHandler } from 'react-hook-form';

interface FormContainerComponentProps {
  entityType: string;
  formType: string;
  agentApi: string;
  isPrimaryEntity?: boolean;
}

/**
 * Renders a form container page for entities.
 * 
 * @param {string} entityType The type of entity.
 * @param {string} formType The type of form such as add, update, delete, and view.
 * @param {string} agentApi The target agent endpoint for any registry related functionalities.
 * @param {boolean} isPrimaryEntity An optional indicator if the form is targeting a primary entity.
 */
export default function FormContainerComponent(props: Readonly<FormContainerComponentProps>) {
  const router = useRouter();
  const dispatch = useDispatch();

  const [refreshFlag, triggerRefresh] = useRefresh();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [isRescindAction, setIsRescindAction] = useState<boolean>(false);
  const [isTerminateAction, setIsTerminateAction] = useState<boolean>(false);
  const [status, setStatus] = useState<ApiResponse>(null);
  const [response, setResponse] = useState<HttpResponse>(null);
  const formRef: React.MutableRefObject<HTMLFormElement> = useRef<HTMLFormElement>();

  const id: string = getAfterDelimiter(usePathname(), "/");
  const showReturnButton: boolean = props.formType === Paths.REGISTRY || !!response;

  // An event handler that will navigate to the required form when clicked
  const openDeleteModal: React.MouseEventHandler<HTMLDivElement> = (e: React.MouseEvent) => {
    e.preventDefault();
    const url: string = `../../delete/${props.entityType}/${id}`;
    router.push(url);
  };

  const openEditModal: React.MouseEventHandler<HTMLDivElement> = (e: React.MouseEvent) => {
    e.preventDefault();
    const url: string = `../../edit/${props.entityType}/${id}`;
    router.push(url);
  };

  // Rescind the target contract
  const rescindContract: SubmitHandler<FieldValues> = async (formData: FieldValues) => {
    rescindOrTerminateAction(formData, `${props.agentApi}/contracts/archive/rescind`);
  }

  // Terminate the target contract
  const terminateContract: SubmitHandler<FieldValues> = async (formData: FieldValues) => {
    rescindOrTerminateAction(formData, `${props.agentApi}/contracts/archive/terminate`);
  }

  // Reusable action method to rescind or terminate the contract
  const rescindOrTerminateAction = async (formData: FieldValues, endpoint: string) => {
    // Add contract and date field
    formData[FORM_STATES.CONTRACT] = status.iri;
    formData[FORM_STATES.DATE] = new Date().toISOString().split("T")[0];
    const response: HttpResponse = await sendPostRequest(endpoint, JSON.stringify(formData));
    setResponse(response);
  }

  // Action when approve button is clicked
  const onApproval: React.MouseEventHandler<HTMLDivElement> = async () => {
    setIsLoading(true);
    const reqBody: JsonObject = {
      contract: status.iri,
      remarks: "Contract has been approved successfully!"
    }
    const response: HttpResponse = await sendPostRequest(`${props.agentApi}/contracts/service/commence`, JSON.stringify(reqBody));
    setResponse(response);
    setIsLoading(false);
    setTimeout(() => {
      dispatch(setIsOpen(false));
      router.back();
    }, 2000);
  };

  const onReturn: React.MouseEventHandler<HTMLDivElement> = () => {
    dispatch(setIsOpen(false));
    router.back();
  };

  const closeTab: React.MouseEventHandler<HTMLDivElement> = () => {
    window.close(); // Closes the tab
    router.back(); // Required to close the intercepted modal as the tab cannot be closed
  };

  const onSubmit: React.MouseEventHandler<HTMLDivElement> = () => {
    if (formRef.current) {
      formRef.current.requestSubmit();
    }
  };

  useEffect(() => {
    // Declare an async function that retrieves the contract status for a view page
    const getContractStatus = async (): Promise<void> => {
      const response: string = await sendGetRequest(`${props.agentApi}/contracts/status/${id}`);
      setStatus(JSON.parse(response));
    }

    if (props.isPrimaryEntity && !status &&
      (props.formType === Paths.REGISTRY || props.formType === Paths.REGISTRY_DELETE || props.formType === Paths.REGISTRY_EDIT)
    ) {
      getContractStatus();
    }
  }, []);

  return (
    <div className={styles["container"]}>
      <div className={`${styles["form-title"]} ${styles["form-row"]}`}>
        <ReturnButton />
        <span>{`${props.formType.toUpperCase()} ${props.entityType.toUpperCase().replace("_", " ")}`}</span>
      </div>
      <div className={styles["form-contents"]}>
        {!(isRescindAction || isTerminateAction) && (refreshFlag ? <LoadingSpinner isSmall={false} /> :
          <FormComponent
            formRef={formRef}
            entityType={props.entityType}
            formType={props.formType}
            agentApi={props.agentApi}
            setResponse={setResponse}
            primaryInstance={status?.iri}
            isPrimaryEntity={props.isPrimaryEntity}
          />)}
        {(isRescindAction || isTerminateAction) && <FormTemplate
          agentApi={props.agentApi}
          entityType={isRescindAction ? "rescission" : "termination"}
          formRef={formRef}
          fields={[remarksShape]}
          submitAction={isRescindAction ? rescindContract : terminateContract}
        />}
      </div>
      <div className={styles["form-footer"]}>
        {!formRef.current?.formState?.isSubmitting && !response && <MaterialIconButton
          iconName={"cached"}
          iconStyles={[styles["form-button-icon"]]}
          onClick={triggerRefresh}
        />}
        {formRef.current?.formState?.isSubmitting || isLoading && <LoadingSpinner isSmall={false} />}
        {!formRef.current?.formState?.isSubmitting && response && (<ResponseComponent response={response} />)}
        <div className={styles["form-row"]}>
          {props.formType === Paths.REGISTRY && !response && status?.message === ENTITY_STATUS.ACTIVE && <ActionButton
            icon={"error"}
            title={"RESCIND"}
            className={styles["footer-button"]}
            onClick={genBooleanClickHandler(setIsRescindAction)}
          />}
          {props.formType === Paths.REGISTRY && !response && status?.message === ENTITY_STATUS.ACTIVE &&
            <ActionButton
              icon={"cancel"}
              title={"TERMINATE"}
              className={styles["footer-button"]}
              onClick={genBooleanClickHandler(setIsTerminateAction)}
            />}
          {props.formType === Paths.REGISTRY && !response && status?.message === ENTITY_STATUS.PENDING && <MaterialIconButton
            iconName={"done_outline"}
            className={styles["form-button"]}
            iconStyles={[styles["form-button-icon"]]}
            text={{
              styles: [styles["form-button-text"]],
              content: "APPROVE"
            }}
            onClick={onApproval}
          />}
          {props.formType === Paths.REGISTRY && !response && status?.message === ENTITY_STATUS.PENDING && <MaterialIconButton
            iconName={"edit"}
            className={styles["form-button"]}
            iconStyles={[styles["form-button-icon"]]}
            text={{
              styles: [styles["form-button-text"]],
              content: "EDIT"
            }}
            onClick={openEditModal}
          />}
          {props.formType === Paths.REGISTRY && !response && status?.message === ENTITY_STATUS.PENDING && <MaterialIconButton
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
            iconName={(isRescindAction || isTerminateAction || !showReturnButton) && !response ? "publish" : "logout"}
            className={styles["form-button"]}
            iconStyles={[styles["form-button-icon"]]}
            text={{
              styles: [styles["form-button-text"]],
              content: (isRescindAction || isTerminateAction || !showReturnButton) && !response ? "SUBMIT" : "RETURN"
            }}
            onClick={(isRescindAction || isTerminateAction) && !response ? onSubmit :
              showReturnButton ? props.formType === Paths.REGISTRY_DELETE ? closeTab : onReturn : onSubmit}
          />
        </div>
      </div>
    </div>
  );
}
