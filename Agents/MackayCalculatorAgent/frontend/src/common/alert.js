import { useState } from 'react';
import Alert from 'react-bootstrap/Alert';

function AlertError(props) {
  if (props.displayerr) {
    return (
      <Alert variant="danger" onClose={() => props.setDisplayErr(false)} dismissible>
        <Alert.Heading>Server Error</Alert.Heading>
        <p>
          The server does not respond at the moment.
        </p>
      </Alert>
    );
  }
  return <div></div>;
}

export default AlertError;