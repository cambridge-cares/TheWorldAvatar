import React from 'react';

import { HttpResponse } from 'utils/server-actions';

interface ResponseComponentProps<> {
  response: HttpResponse;
}

/**
 * Renders the response message for dialogs after submission.
 * 
 * @param {HttpResponse} response Response to display.
 */
export default function ResponseComponent(props: Readonly<ResponseComponentProps>) {
  if (props.response) {
    const textColor: string = props.response?.success ? "#52B7A5" : "#D7653D";
    return (
      <div style={{ color: textColor, overflowY: "auto", width: "100%" }}>
        <span>{props.response.message}</span>
        {!props.response.success && (<>
          <br /><br /><span>Contact your technical team if assistance is required.</span>
        </>)}
      </div>
    );
  } else {
    return <div></div>;
  }
}