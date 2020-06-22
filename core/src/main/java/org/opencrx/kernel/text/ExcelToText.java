/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ExcelToText
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2007-2014, CRIXP Corp., Switzerland
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of CRIXP Corp. nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.kernel.text;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.apache.poi.hssf.eventusermodel.HSSFEventFactory;
import org.apache.poi.hssf.eventusermodel.HSSFListener;
import org.apache.poi.hssf.eventusermodel.HSSFRequest;
import org.apache.poi.hssf.record.BoundSheetRecord;
import org.apache.poi.hssf.record.CellValueRecordInterface;
import org.apache.poi.hssf.record.DSFRecord;
import org.apache.poi.hssf.record.LabelSSTRecord;
import org.apache.poi.hssf.record.NumberRecord;
import org.apache.poi.hssf.record.Record;
import org.apache.poi.hssf.record.SSTRecord;
import org.apache.poi.hssf.record.StringRecord;
import org.apache.poi.hssf.record.TextObjectRecord;
import org.apache.poi.hssf.usermodel.HSSFRichTextString;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.exception.BasicException;

/**
 * ExcelToText
 *
 */
public class ExcelToText implements HSSFListener {

    /**
     * Constructor.
     * 
     */
    public ExcelToText(
    ) {
        this.text = new StringBuilder();
    }

    /**
     * Extract text from XLS.
     * 
     * @param document
     * @return
     * @throws ServiceException
     */
    public Reader parse(
        InputStream document
    ) throws ServiceException {
    	try {
	        this.text.setLength(0);
	        POIFSFileSystem fs = new POIFSFileSystem(document);
	        InputStream workbook = fs.createDocumentInputStream("Workbook");
	        HSSFRequest request = new HSSFRequest();
	        request.addListenerForAllRecords(this);
	        HSSFEventFactory eventFactory = new HSSFEventFactory();
	        try {
		        eventFactory.processEvents(
		            request, 
		            workbook
		        );
	        } catch(Exception e) {
	        	throw new ServiceException(e);
	        } catch(NoSuchMethodError e) {
	        	throw new ServiceException(
	        		BasicException.toExceptionStack(e)
	        	);
	        }
	        workbook.close();
	        return new StringReader(
	            this.text.toString()
	        );
    	} catch(IOException e) {
    		throw new ServiceException(e);
    	}
    }

    /**
     * This method listens for incoming records and handles them as required.
     * 
     * @param record
     *        The record that was found while reading.
     */
    public void processRecord(
        Record record
    ) {
        int curentRow = 0;
        if (this.sstrec != null) {
            try {
                if (record instanceof CellValueRecordInterface) {
                    if (record instanceof LabelSSTRecord) {
                        LabelSSTRecord bof2 = (LabelSSTRecord) record;
                        int rowNum = ((CellValueRecordInterface) record).getRow();
                        if (curentRow < rowNum) {
                            this.text.append(" ");
                            curentRow = rowNum;
                        }
                        try {
                            String value = sstrec.getString(bof2.getSSTIndex()).toString();
                            this.text.append(value + " ");
                        } catch(Exception e) {}
                    }
                    if (record instanceof NumberRecord) {
                        NumberRecord bof2 = (NumberRecord) record;
                        int rowNum = ((CellValueRecordInterface) record).getRow();
                        if (curentRow < rowNum) {
                            this.text.append(" ");
                            curentRow = rowNum;
                        }
                        String value = (new Double(bof2.getValue())).toString();
                        this.text.append(value + " ");
                    }
                }
            } 
            catch(Exception ex) {}
            catch(NoSuchMethodError e) {}
        }
        switch (record.getSid()) {
            case BoundSheetRecord.sid:
                BoundSheetRecord bsr = (BoundSheetRecord) record;
                this.text.append(bsr.getSheetname() + " ");
                break;
            case SSTRecord.sid:
                this.sstrec = (SSTRecord) record;
                break;
            case DSFRecord.sid:
                break;
    
            case TextObjectRecord.sid:
                TextObjectRecord textObjectRecord = (TextObjectRecord) record;
                HSSFRichTextString q = textObjectRecord.getStr();
                String st = q.getString();
                if (st != null && !"".equals(st)) {
                    int ind = this.text.indexOf(st);
                    if (ind == -1) {
                        this.text.append(st + " ");
                    }
                }
                break;
    
            case StringRecord.sid:
                StringRecord selectionRecord = (StringRecord) record;
                String str = selectionRecord.getString();
                if (str != null && !"".equals(str)) {
                    int ind = this.text.indexOf(str);
                    if (ind == -1) {
                        this.text.append(str + " ");
                    }
                }
                break;
        }
    }

    // -----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected final StringBuilder text;
    protected SSTRecord sstrec = null;

}
