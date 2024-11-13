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
      <div style={{ color: textColor, overflowY: "auto", height: "5vh", width: "100%" }}>
        {props.response.message}
        <br />
        {!props.response.success ? "Contact your technical team if assistance is required." : ""}
      </div>
    );
  } else {
    return <div></div>;
  }
}