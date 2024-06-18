import React from 'react';
import Image from 'next/image';
import styles from './acknowledgements.module.css';
import ReturnButton from '../../ui/navigation/return/return';

const AcknowledgementsPage: React.FC = () => {
    return (
        <div className={styles.acknowledgementsContainer}>
            <Image
                src="/images/credo-misc/acknowledgements.svg"
                width={1750}
                height={1050}
                alt="Acknowledgements"
            />
        </div>
    );
};

export default AcknowledgementsPage;