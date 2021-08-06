package salesreport;
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: InvoicePositionExporter
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2016, CRIXP Corp., Switzerland
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
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.contract1.cci2.InvoicePositionQuery;
import org.opencrx.kernel.contract1.cci2.PostalAddressQuery;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.InvoicePosition;
import org.opencrx.kernel.contract1.jmi1.PostalAddress;
import org.opencrx.kernel.workflow1.jmi1.ExporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunExportResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.kernel.log.SysLog;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class SalesReportExporter {

	public static enum Param {
		
		TARGET_XRI(0);

		private Param(
			int index
		) {
			this.index = index;
		}
		
		public int getIndex(
		) {
			return this.index;
		}
		
		private final int index;
	}
	
	/**
	 * Export invoice positions as CSV.
	 * 
	 * @param in
	 * @return
	 */
	public static RunExportResult runExport(
		ExporterTask exporterTask,
		String[] params
	) {
		try {
			SysLog.warning("params=" + Arrays.asList(params));
			PersistenceManager pm = JDOHelper.getPersistenceManager(exporterTask);
			org.opencrx.kernel.contract1.jmi1.Segment contractSegment = (org.opencrx.kernel.contract1.jmi1.Segment)pm.getObjectById(
				new Path(params[Param.TARGET_XRI.getIndex()])
			);
			String file = "invoicedate,productid,productname,productline,quantity,priceuom,priceperunit,currency,customer,postalcode,postalcountry,salesrep\n";
			String fileName = "InvoicePositions-" + DateTimeFormat.BASIC_UTC_FORMAT.format(new Date()) + ".csv";
			String fileMimeType = "text/csv";
            InvoicePositionQuery invoicePositionQuery = (InvoicePositionQuery)PersistenceHelper.newQuery(
	    		pm.getExtent(InvoicePosition.class),
	    		contractSegment.refGetPath().getDescendant("invoice", ":*", "position", ":*")
	    	);
            invoicePositionQuery.product().isNonNull(); 
            invoicePositionQuery.priceUom().isNonNull();
            invoicePositionQuery.pricePerUnit().isNonNull();
			invoicePositionQuery.forAllDisabled().isFalse();
			for(InvoicePosition invoicePosition: contractSegment.<InvoicePosition>getExtent(invoicePositionQuery)) {
				Invoice invoice = (Invoice)pm.getObjectById(invoicePosition.refGetPath().getPrefix(7));
				PostalAddressQuery shippingAddressQuery = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
				shippingAddressQuery.thereExistsUsage().equalTo((short)10200);
				List<PostalAddress> shippingAddresses = invoice.getAddress(shippingAddressQuery);
				PostalAddress shippingAddress = shippingAddresses.isEmpty() ? null : (PostalAddress)shippingAddresses.iterator().next();
				try {
					if(
						shippingAddress != null &&
						invoice.getSalesRep() != null &&
						invoice.getCustomer() != null &&
						invoice.getActiveOn() != null
					) {
						Object productLine = invoicePosition.getProduct().getExtCode0();
						if(productLine == null) {
							if(invoicePosition.getProduct().getConfigType() != null) {
								productLine = invoicePosition.getProduct().getConfigType().getName();
							}
						}
						String item =
							DateTimeFormat.EXTENDED_UTC_FORMAT.format(invoice.getActiveOn()) + "," +
							invoicePosition.getProduct().getProductNumber() + "," +
							"\"" + invoicePosition.getProduct().getName() + "\"," +
							"\"" + (productLine == null ? "Other" : productLine) + "\"," +
							invoicePosition.getQuantity() + "," +
							invoicePosition.getPriceUom().getName() + "," +
							invoicePosition.getPricePerUnit() + "," +
							invoice.getContractCurrency() + "," +
							"\"" + invoice.getCustomer().getFullName() + "\"," +
							shippingAddress.getPostalCode() + "," +
							shippingAddress.getPostalCountry() + "," +
							"\"" + invoice.getSalesRep().getFullName() + "\"\n";
						file += item;
						SysLog.warning("item", item);
					}
				} catch(Exception ignore) {}					
			}
            return (RunExportResult)Structures.create(
            	RunExportResult.class,
        		Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunExportResult.Member.file, file.toString().getBytes("UTF-8")),
        		Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunExportResult.Member.fileName, fileName),
        		Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunExportResult.Member.fileMimeType, fileMimeType),
            	Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunExportResult.Member.status, (short)0),
            	Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunExportResult.Member.statusMessage, "")
            );		
		} catch(Exception e) {
			throw new JmiServiceException(e);
		} finally {
		}
	}
	
}
