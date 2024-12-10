"use client";

import React from 'react';

import { sendGetRequest } from 'utils/server-actions';
import ActionButton from '../action';

interface DownloadButtonProps extends React.HTMLAttributes<HTMLButtonElement> {
  agentApi: string;
}

/**
 * This component renders a download button for downloading a CSV content .
 * 
 * @param {string} agentApi The target endpoint to retrieve the require csv contents.
 */
export function DownloadButton({ agentApi, ...rest }: Readonly<DownloadButtonProps>) {
  const handleCsvDownload = async () => {
    // Fetch contents and transform it into a url for download
    const csvContents: string = await sendGetRequest(agentApi);
    const blob: Blob = new Blob([csvContents], { type: "text/csv;charset=utf-8;" });
    const url: string = URL.createObjectURL(blob);

    // Create a temporary anchor element to activate download
    const a: HTMLAnchorElement = document.createElement("a");
    a.href = url;
    a.download = "export.csv";
    document.body.appendChild(a);
    // Trigger the download manually
    a.click();
    a.remove();
    // Cleanup: Revoke the Object URL after the download
    window.URL.revokeObjectURL(url);
  };

  return (
    <ActionButton
      icon="download"
      className={`${rest.className}`}
      title="export data"
      onClick={handleCsvDownload}
    />
  );
}