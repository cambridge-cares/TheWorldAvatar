"use client";
import styles from './task.modal.module.css';

import React, { useEffect, useRef, useState } from 'react';
import Modal from 'react-modal';
import { FieldValues, SubmitHandler } from 'react-hook-form';

import { remarksShape } from 'types/form';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import LoadingSpinner from 'ui/graphic/loader/spinner';
import ActionButton from 'ui/interaction/action/action';
import ResponseComponent from 'ui/text/response/response';
import { FormTemplate } from 'ui/interaction/form/template/form-template';
import { FORM_STATES } from 'ui/interaction/form/form-utils';
import { genBooleanClickHandler } from 'utils/event-handler';
import { HttpResponse, sendPostRequest } from 'utils/server-actions';

interface TaskModalProps {
  id: string;
  taskStatus: string;
  date: string;
  registryAgentApi: string;
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  setTaskId: React.Dispatch<React.SetStateAction<string>>;
  setTaskStatus: React.Dispatch<React.SetStateAction<string>>;
}

/**
 * A modal component for users to interact with their tasks while on the registry.
 * 
 * @param {string} id The identifier for the task.
 * @param {string} taskStatus The current status of the task.
 * @param {string} date The selected date.
 * @param {string} registryAgentApi The target endpoint for the default registry agent.
 * @param {boolean} isOpen Indicator if the this modal should be opened.
 * @param setIsOpen Method to close or open the modal.
 * @param setTaskId Method to update the task id state.
 * @param setTaskStatus Method to update the task status state.
 */
export default function TaskModal(props: Readonly<TaskModalProps>) {
  Modal.setAppElement("#globalContainer");

  const formRef: React.MutableRefObject<HTMLFormElement> = useRef<HTMLFormElement>();
  const [isCompleteAction, setIsCompleteAction] = useState<boolean>(false);
  const [isCancelAction, setIsCancelAction] = useState<boolean>(false);
  const [isReportAction, setIsReportAction] = useState<boolean>(false);
  // Form actions
  const [response, setResponse] = useState<HttpResponse>(null);

  // Closes the modal on click
  const onClose: React.MouseEventHandler<HTMLDivElement> = () => {
    props.setIsOpen(false);
    props.setTaskId(null);
    props.setTaskStatus(null);
  };

  // Lodges a new report
  const reportTask: SubmitHandler<FieldValues> = async (formData: FieldValues) => {
    reportOrCancelAction(formData, `${props.registryAgentApi}/contracts/service/report`);
  }

  // Cancel a scheduled service
  const cancelTask: SubmitHandler<FieldValues> = async (formData: FieldValues) => {
    reportOrCancelAction(formData, `${props.registryAgentApi}/contracts/service/cancel`);
  }

  // Reusable action method to report or cancel the service task
  const reportOrCancelAction = async (formData: FieldValues, endpoint: string) => {
    // Add contract and date field
    formData[FORM_STATES.CONTRACT] = props.id;
    formData[FORM_STATES.DATE] = props.date;
    const response: HttpResponse = await sendPostRequest(endpoint, JSON.stringify(formData));
    setResponse(response);
  }

  const onSubmit: React.MouseEventHandler<HTMLDivElement> = () => {
    if (formRef.current) {
      formRef.current.requestSubmit();
    }
  };

  // Closes the modal only if response is successfull
  useEffect(() => {
    if (response?.success) {
      setTimeout(() => props.setIsOpen(false), 2000);
    }
  }, [response]);

  return (
    <Modal
      isOpen={props.isOpen}
      overlayClassName={styles.overlay}
      className={styles.modal}
    >
      <div className={styles.container}>
        <section className={styles["section-title"]}>
          <h1>ACTIONS</h1>
        </section>
        <section className={styles["section-contents"]}>
          {isCompleteAction && <p className={styles["instructions"]}>
            THIS ACTION IS UNDER CONSTRUCTION
          </p>}
          {isCancelAction && <p className={styles["instructions"]}>
            Cancel the scheduled service on {props.date}. <br /> Please provide a reason for the cancellation:
          </p>}
          {isReportAction && <p className={styles["instructions"]}>
            Report an issue with the service on {props.date}. <br /> Please include the reason in your report:
          </p>}
          {(isReportAction || isCancelAction) && <FormTemplate
            agentApi={props.registryAgentApi}
            entityType={isReportAction ? "report" : "cancellation"}
            formRef={formRef}
            fields={[remarksShape]}
            submitAction={isReportAction ? reportTask : cancelTask}
          />}
          {/*Show action buttons before they are clicked*/}
          {props.taskStatus.toLowerCase().trim() == "in progress" &&
            !(isCancelAction || isCompleteAction || isReportAction) && <>
              <p className={styles["instructions"]}>Please choose an action for the service on {props.date}:</p>
              <ActionButton
                icon={"done_outline"}
                title={"COMPLETE"}
                onClick={genBooleanClickHandler(setIsCompleteAction)}
              />
              <ActionButton
                icon={"cancel"}
                title={"CANCEL"}
                onClick={genBooleanClickHandler(setIsCancelAction)}
              />
              <ActionButton
                icon={"report"}
                title={"REPORT"}
                onClick={genBooleanClickHandler(setIsReportAction)}
              />
            </>
          }
        </section>
        <section className={styles["section-footer"]}>
          {formRef.current?.formState?.isSubmitting && <LoadingSpinner isSmall={false} />}
          {!formRef.current?.formState?.isSubmitting && (<ResponseComponent response={response} />)}
          <div className={styles["footer-button-row"]}>
            <MaterialIconButton
              iconName={"keyboard_return"}
              className={styles["section-footer-button"]}
              iconStyles={[styles["icon"]]}
              text={{
                styles: [styles["button-text"]],
                content: "RETURN"
              }}
              onClick={onClose}
            />
            {(isCancelAction || isCompleteAction || isReportAction) && <MaterialIconButton
              iconName={"publish"}
              className={styles["section-footer-button"]}
              iconStyles={[styles["icon"]]}
              text={{
                styles: [styles["button-text"]],
                content: "SUBMIT"
              }}
              onClick={onSubmit}
            />}
          </div>
        </section>
      </div>
    </Modal>
  );
}