
import React from 'react';
import { Accordion, AccordionSummary, AccordionDetails, Typography } from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import styles from './bottom-panel.module.css'
import Flow from './graph/Flow';

const BottomPanel = () => {
    return (
        <div className={styles.bottomPanel}>
            <Accordion>
                <AccordionSummary
                    expandIcon={<ExpandMoreIcon />}
                    aria-controls="panel-content"
                    id="panel-header"
                >
                    <Typography>Info and Context</Typography>
                </AccordionSummary>
                <AccordionDetails>
                    <Flow />
                </AccordionDetails>
            </Accordion>
        </div>
    );
};

export default BottomPanel;