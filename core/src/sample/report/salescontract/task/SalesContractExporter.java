package salescontract;

import java.util.Arrays;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.address1.jmi1.PostalAddressable;
import org.opencrx.kernel.contract1.cci2.InvoicePositionQuery;
import org.opencrx.kernel.contract1.cci2.InvoiceQuery;
import org.opencrx.kernel.contract1.cci2.OpportunityPositionQuery;
import org.opencrx.kernel.contract1.cci2.OpportunityQuery;
import org.opencrx.kernel.contract1.cci2.PostalAddressQuery;
import org.opencrx.kernel.contract1.cci2.QuotePositionQuery;
import org.opencrx.kernel.contract1.cci2.QuoteQuery;
import org.opencrx.kernel.contract1.cci2.SalesOrderPositionQuery;
import org.opencrx.kernel.contract1.cci2.SalesOrderQuery;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.InvoicePosition;
import org.opencrx.kernel.contract1.jmi1.Opportunity;
import org.opencrx.kernel.contract1.jmi1.OpportunityPosition;
import org.opencrx.kernel.contract1.jmi1.PostalAddress;
import org.opencrx.kernel.contract1.jmi1.Quote;
import org.opencrx.kernel.contract1.jmi1.QuotePosition;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.contract1.jmi1.SalesOrderPosition;
import org.opencrx.kernel.product1.jmi1.ConfiguredProduct;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.workflow1.jmi1.ExporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunExportResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.log.SysLog;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class SalesContractExporter {

	public static enum Param {
		
		TARGET_XRI(0),
		CONTRACT_TYPE(1),
		CONTRACT_NUMBER(2);

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
	
	public static enum ContractType {
		OPPORTUNITY,
		QUOTE,
		SALES_ORDER,
		INVOICE
	}
	
	/**
	 * Map contract position to JSON.
	 * 
	 * @param position
	 * @return
	 * @throws ServiceException
	 */
	public static String toJson(
		SalesContractPosition position
	) throws ServiceException {
		Product product = (Product)((ConfiguredProduct)position).getProduct();
		return
			"{\n" +
			"    \"positionNumber\": \"" + position.getPositionNumber() + "\",\n" +
			"    \"productName\": \"" + product.getName() + "\",\n" +
			"    \"productNumber\": \"" + product.getProductNumber() + "\",\n" +
			"    \"quantity\": " + String.format("%,.2f", position.getQuantity()) + ",\n" +
			"    \"uom\": \"" + position.getPriceUom().getName() + "\",\n" +
			"    \"pricePerUnit\": " + String.format("%,.2f", position.getPricePerUnit()) + ",\n" + 
			"    \"amount\": " + String.format("%,.2f", position.getAmount()) + ",\n" +
			"    \"taxAmount\": " + String.format("%,.2f", position.getTaxAmount()) + ",\n" +
			"    \"salesTaxType\": \"" + position.getSalesTaxType().getName() + "\"\n" +
			"}";
	}

	/**
	 * Map postal street to JSON.
	 * 
	 * @param postalAddress
	 * @return
	 * @throws ServiceException
	 */
	public static String toJson(
		PostalAddressable postalAddress
	) throws ServiceException {
		return
			"{\n" +
			"    \"postalAddressLine0\": \"" + (postalAddress.getPostalAddressLine().size() > 0 ? (String)postalAddress.getPostalAddressLine().get(0) : "") + "\",\n" +
			"    \"postalAddressLine1\": \"" + (postalAddress.getPostalAddressLine().size() > 1 ? (String)postalAddress.getPostalAddressLine().get(1) : "") + "\",\n" +
			"    \"postalStreet0\": \"" + (postalAddress.getPostalStreet().size() > 0 ? (String)postalAddress.getPostalStreet().get(0) : "") + "\",\n" +
			"    \"postalStreet1\": \"" + (postalAddress.getPostalStreet().size() > 1 ? (String)postalAddress.getPostalStreet().get(1) : "") + "\",\n" +
			"    \"postalCode\": \"" + (postalAddress.getPostalCode() == null ? "" : postalAddress.getPostalCode()) + "\",\n" +
			"    \"postalCity\": \"" + (postalAddress.getPostalCity() == null ? "" : postalAddress.getPostalCity()) + "\",\n" +
			"    \"postalCountry\": " + postalAddress.getPostalCountry() + "\n" +
			"}";
	}
	
	/**
	 * Export sales contract as JSON.
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
			String contractType = params[Param.CONTRACT_TYPE.getIndex()];
			String contractNumber = params[Param.CONTRACT_NUMBER.getIndex()];
			SalesContract salesContract = null;
			if(ContractType.OPPORTUNITY.name().equalsIgnoreCase(contractType)) {
				OpportunityQuery opportunityQuery = (OpportunityQuery)pm.newQuery(Opportunity.class);
				opportunityQuery.thereExistsContractNumber().equalTo(contractNumber);
				List<Opportunity> opportunities = contractSegment.getOpportunity(opportunityQuery);
				salesContract = opportunities.isEmpty() ? null : (Quote)opportunities.iterator().next();
			} else if(ContractType.QUOTE.name().equalsIgnoreCase(contractType)) {
				QuoteQuery quoteQuery = (QuoteQuery)pm.newQuery(Quote.class);
				quoteQuery.thereExistsContractNumber().equalTo(contractNumber);
				List<Quote> quotes = contractSegment.getQuote(quoteQuery);
				salesContract = quotes.isEmpty() ? null : (Quote)quotes.iterator().next();				
			} else if(ContractType.SALES_ORDER.name().equalsIgnoreCase(contractType)) {
				SalesOrderQuery salesOrderQuery = (SalesOrderQuery)pm.newQuery(SalesOrder.class);
				salesOrderQuery.thereExistsContractNumber().equalTo(contractNumber);
				List<SalesOrder> salesOrders = contractSegment.getSalesOrder(salesOrderQuery);
				salesContract = salesOrders.isEmpty() ? null : (SalesContract)salesOrders.iterator().next();				
			} else if(ContractType.INVOICE.name().equalsIgnoreCase(contractType)) {
				InvoiceQuery invoiceQuery = (InvoiceQuery)pm.newQuery(Invoice.class);
				invoiceQuery.thereExistsContractNumber().equalTo(contractNumber);
				List<Invoice> invoices = contractSegment.getInvoice(invoiceQuery);
				salesContract = invoices.isEmpty() ? null : (SalesContract)invoices.iterator().next();
			}
			String file = "";
			String fileName = contractType + "-" + contractNumber + ".json";
			String fileMimeType = "text/json";
			if(salesContract != null) {
				PostalAddressQuery shippingAddressQuery = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
				shippingAddressQuery.thereExistsUsage().equalTo((short)10200);
				List<PostalAddress> shippingAddresses = salesContract.getAddress(shippingAddressQuery);
				PostalAddress shippingAddress = shippingAddresses.isEmpty() ? null : (PostalAddress)shippingAddresses.iterator().next();			
				file += 
					"{\n" +
					"    \"contractNumber\": \"" + salesContract.getContractNumber() + "\",\n" + 
					"    \"name\": \"" + salesContract.getName() + "\",\n" +
					"    \"description\": \"" + (salesContract.getDescription() == null ? "" : salesContract.getDescription()) + "\",\n" +
					"    \"activeOn\": \"" + (salesContract.getActiveOn() == null ? "" : DateTimeFormat.BASIC_UTC_FORMAT.format(salesContract.getActiveOn())) + "\",\n" +
					"    \"contractState\": " + salesContract.getContractState() + ",\n" +
					"    \"contractCurrency\": " + salesContract.getContractCurrency() + ",\n" +
					"    \"salesRep\": \"" + (salesContract.getSalesRep() == null ? "" : salesContract.getSalesRep().getFullName()) + "\",\n" +
					"    \"totalAmount\": " + String.format("%,.2f", salesContract.getTotalAmount()) + ",\n" +
					"    \"totalTaxAmount\": " + String.format("%,.2f", salesContract.getTotalTaxAmount()) + ",\n" +
					"    \"shippingAddress\": \n" + toJson(shippingAddress) + ",\n" +
					"    \"positions\": [";
				if(salesContract instanceof Opportunity) {
					OpportunityPositionQuery positionQuery = (OpportunityPositionQuery)pm.newQuery(OpportunityPosition.class);
					positionQuery.orderByPositionNumber().ascending();
					String sep = "";
					for(OpportunityPosition position: ((Opportunity)salesContract).<OpportunityPosition>getPosition(positionQuery)) {
						file += sep + toJson(position) + "\n";
						sep = ",";
					}
				} else if(salesContract instanceof Quote) {
					QuotePositionQuery positionQuery = (QuotePositionQuery)pm.newQuery(QuotePosition.class);
					positionQuery.orderByPositionNumber().ascending();
					String sep = "";
					for(QuotePosition position: ((Quote)salesContract).<QuotePosition>getPosition(positionQuery)) {
						file += sep + toJson(position) + "\n";
						sep = ",";
					}
				} else if(salesContract instanceof SalesOrder) {
					SalesOrderPositionQuery positionQuery = (SalesOrderPositionQuery)pm.newQuery(SalesOrderPosition.class);
					positionQuery.orderByPositionNumber().ascending();
					String sep = "";
					for(SalesOrderPosition position: ((SalesOrder)salesContract).<SalesOrderPosition>getPosition(positionQuery)) {
						file += sep + toJson(position) + "\n";
						sep = ",";
					}
				} else if(salesContract instanceof Invoice) {
					InvoicePositionQuery positionQuery = (InvoicePositionQuery)pm.newQuery(InvoicePosition.class);
					positionQuery.orderByPositionNumber().ascending();
					String sep = "";
					for(InvoicePosition position: ((Invoice)salesContract).<InvoicePosition>getPosition(positionQuery)) {
						file += sep + toJson(position) + "\n";
						sep = ",";
					}
				}
				file += 
					"    ]\n" +
					"}\n";
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
