from flask import Flask, request
import win32print, win32api
from base64 import b64decode
import os

#TODO Make printer name changeable on runtime
#printer_name = "RICOH MP C4504 PCL 6" # or get_printer_names()[0]

PRINTER_ERROR_STATES = (
    win32print.PRINTER_STATUS_NO_TONER,
    win32print.PRINTER_STATUS_NOT_AVAILABLE,
    win32print.PRINTER_STATUS_OFFLINE,
    win32print.PRINTER_STATUS_OUT_OF_MEMORY,
    win32print.PRINTER_STATUS_OUTPUT_BIN_FULL,
    win32print.PRINTER_STATUS_PAGE_PUNT,
    win32print.PRINTER_STATUS_PAPER_JAM,
    win32print.PRINTER_STATUS_PAPER_OUT,
    win32print.PRINTER_STATUS_PAPER_PROBLEM,
)

def printer_errorneous_state(printer, error_states=PRINTER_ERROR_STATES):
    prn_opts = win32print.GetPrinter(printer)
    status_opts = prn_opts[18]
    for error_state in error_states:
        if status_opts & error_state:
            return error_state
    return 0

def getPrinterName ():
    name = os.environ.get("PRINTERSERVER_PRINTER")
    if name is None:
        return "Environment variable PRINTERSERVER_PRINTER for printer name does not exist", 500
    else:
        return name, 200

def getPrinterStatus():
    printer_name, resp_code = getPrinterName()
    if resp_code!= 200:
        return printer_name, resp_code
    prn = win32print.OpenPrinter(printer_name)
    error = printer_errorneous_state(prn)
    win32print.ClosePrinter(prn)
    if error:
        return"ERROR occurred: " + error
    else:
        return "Printer OK..."

    

def dumpPDF (data) :
    bytes = b64decode(data, validate=True)

    # Perform a basic validation to make sure that the result is a valid PDF file
    if bytes[0:4] != b'%PDF':
        return 'Missing the PDF file signature'

    # Write the PDF contents to a local file
    f = open('file.pdf', 'wb')
    f.write(bytes)
    f.close()


app = Flask(__name__)

@app.route('/print', methods=['POST'])
def result():
    #print(request.json)  # json (if content-type of application/json is sent with the request)
    printer_name, resp_code = getPrinterName()
    if resp_code!= 200:
        return printer_name, resp_code
    
    data = request.json["rawPDF"]
    dumpPDF(data)

    status = getPrinterStatus()
    if status == "Printer OK...":
        pdf_file = open('file.pdf', 'rb')
        pdf_data = pdf_file.read()
        prn = win32print.OpenPrinter(printer_name)
        printer_job = win32print.StartDocPrinter(prn, 1, ("file.pdf", None, "RAW"))
        win32print.StartPagePrinter(prn)
        win32print.WritePrinter(prn, pdf_data)
        win32print.EndPagePrinter(prn)
        win32print.EndDocPrinter(prn)
        win32print.ClosePrinter(prn)
        return "Print Success!", 200
    
    else:
        return "Printer is having an issue: " + status, 500


@app.route('/', methods=['GET'])
def getStatus():
    return f"<p>Printer status: {getPrinterStatus()}</p>"

if __name__ == "__main__":
    app.run()