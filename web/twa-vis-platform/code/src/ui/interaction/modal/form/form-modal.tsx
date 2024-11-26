"use client";
import styles from './form.modal.module.css';

import React, { useEffect } from 'react';
import Modal from 'react-modal';
import { useDispatch, useSelector } from 'react-redux';

import { getIsOpenState, setIsOpen } from 'state/modal-slice';

interface FormModalProps {
  children: React.ReactNode;
}

/**
 * A modal component for users to interact with a form while on the registry.
 */
export default function FormModal(props: Readonly<FormModalProps>) {
  Modal.setAppElement("#globalContainer");
  const isOpen: boolean = useSelector(getIsOpenState);
  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setIsOpen(true));
  }, [])

  return (
    <Modal
      isOpen={isOpen}
      overlayClassName={styles.overlay}
      className={styles.modal}
    >
      {props.children}
    </Modal>
  );
}