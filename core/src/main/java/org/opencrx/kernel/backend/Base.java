/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Base
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.backend;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.URLEncoder;
import java.security.MessageDigest;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.zip.ZipInputStream;

import javax.jdo.FetchGroup;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.resource.ResourceException;
import javax.resource.cci.MappedRecord;
import javax.servlet.http.HttpServletRequest;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAssignment;
import org.opencrx.kernel.account1.jmi1.AssignedAccountAssignment;
import org.opencrx.kernel.account1.jmi1.ContactRelationship;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.RevenueReport;
import org.opencrx.kernel.activity1.jmi1.AbstractActivityParty;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.activity1.jmi1.AddressGroupMember;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.EMailRecipient;
import org.opencrx.kernel.activity1.jmi1.EMailRecipientGroup;
import org.opencrx.kernel.activity1.jmi1.IncidentParty;
import org.opencrx.kernel.activity1.jmi1.MailingRecipient;
import org.opencrx.kernel.activity1.jmi1.MailingRecipientGroup;
import org.opencrx.kernel.activity1.jmi1.MeetingParty;
import org.opencrx.kernel.activity1.jmi1.PhoneCallRecipient;
import org.opencrx.kernel.activity1.jmi1.PhoneCallRecipientGroup;
import org.opencrx.kernel.activity1.jmi1.TaskParty;
import org.opencrx.kernel.address1.jmi1.Addressable;
import org.opencrx.kernel.address1.jmi1.EMailAddressable;
import org.opencrx.kernel.address1.jmi1.PhoneNumberAddressable;
import org.opencrx.kernel.address1.jmi1.PostalAddressable;
import org.opencrx.kernel.address1.jmi1.RoomAddressable;
import org.opencrx.kernel.address1.jmi1.UriAddressable;
import org.opencrx.kernel.address1.jmi1.WebAddressable;
import org.opencrx.kernel.base.cci2.HashEntryQuery;
import org.opencrx.kernel.base.jmi1.HashEntry;
import org.opencrx.kernel.base.jmi1.Hashable;
import org.opencrx.kernel.building1.jmi1.AbstractBuildingUnit;
import org.opencrx.kernel.building1.jmi1.InventoryItem;
import org.opencrx.kernel.code1.jmi1.CodeValueEntry;
import org.opencrx.kernel.code1.jmi1.SimpleEntry;
import org.opencrx.kernel.contract1.jmi1.AbstractContract;
import org.opencrx.kernel.contract1.jmi1.ContractRole;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.opencrx.kernel.depot1.jmi1.AggregatedDepotReportItem;
import org.opencrx.kernel.depot1.jmi1.CompoundBooking;
import org.opencrx.kernel.depot1.jmi1.Depot;
import org.opencrx.kernel.depot1.jmi1.DepotContract;
import org.opencrx.kernel.depot1.jmi1.DepotEntity;
import org.opencrx.kernel.depot1.jmi1.DepotPosition;
import org.opencrx.kernel.depot1.jmi1.DepotReport;
import org.opencrx.kernel.depot1.jmi1.DepotReportItemPosition;
import org.opencrx.kernel.depot1.jmi1.SimpleBooking;
import org.opencrx.kernel.depot1.jmi1.SingleBooking;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.EnableDisableCrxObjectResult;
import org.opencrx.kernel.generic.jmi1.InvolvedObject;
import org.opencrx.kernel.generic.jmi1.Media;
import org.opencrx.kernel.home1.cci2.AlertQuery;
import org.opencrx.kernel.home1.jmi1.Alert;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.product1.jmi1.AbstractPriceLevel;
import org.opencrx.kernel.product1.jmi1.AbstractProduct;
import org.opencrx.kernel.product1.jmi1.PriceListEntry;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.opencrx.kernel.text.ExcelToText;
import org.opencrx.kernel.text.OpenOfficeToText;
import org.opencrx.kernel.text.PDFToText;
import org.opencrx.kernel.text.RTFToText;
import org.opencrx.kernel.text.WordToText;
import org.opencrx.kernel.text.XmlDocToText;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.opencrx.kernel.utils.TinyUrlUtils;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.UserObjects;
import org.openmdx.base.query.IsInCondition;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.resource.Records;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryFilterRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.WebKeys;
import org.openmdx.portal.servlet.action.SelectObjectAction;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * Base backend class.
 *
 */
public class Base extends AbstractImpl {

	/**
	 * Register backend.
	 */
	public static void register(
	) {
		registerImpl(new Base());
	}
	
	/**
	 * Get backend instance.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Base getInstance(
	) throws ServiceException {
		return getInstance(Base.class);
	}

	/**
	 * Constructor.
	 * 
	 */
	protected Base(
	) {
		
	}
	
	public static interface RestInteractionCallback {

		public void get(
			QueryRecord query,
			ResultRecord result
		) throws ResourceException;
		
		public void find(
			QueryRecord query,
			ResultRecord result
		) throws ResourceException;
		
		public void create(
			ObjectRecord object,
			ResultRecord result
		) throws ResourceException;

		public void update(
			ObjectRecord object,
			ResultRecord result
		) throws ResourceException;

	}

    /**
     * Send alert to given users.
     * 
     * @param target
     * @param toUsers
     * @param name
     * @param description
     * @param importance
     * @param resendDelayInSeconds
     * @param reference
     * @return identities of newly created alerts
     * @throws ServiceException
     */
    public List<Path> sendAlert(
    	ContextCapable target,
        String toUsers,        
        String name,
        String description,
        short importance,
        Integer resendDelayInSeconds,
        ContextCapable reference
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(target);    	
        StringTokenizer tokenizer = new StringTokenizer(
            toUsers == null ? 
                "" : 
                toUsers, ";, "
        );
        List<Path> alertIdentities = new ArrayList<Path>();
        while(tokenizer.hasMoreTokens()) {
            String toUser = tokenizer.nextToken();
            UserHome userHome = UserHomes.getInstance().getUserHome(
                toUser,
                target.refGetPath(),
                pm
            );
            if(userHome != null) {
            	Path referenceIdentity = (reference == null) || !JDOHelper.isPersistent(reference) ? 
                	target.refGetPath() : 
                	reference.refGetPath();                
                // Only create alert if there is not already an alert with the 
                // same alert reference and created within the delay period.
                PersistenceManager pmRoot = pm.getPersistenceManagerFactory().getPersistenceManager(
                	SecurityKeys.ROOT_PRINCIPAL, 
                	null
                );
                ContextCapable alertReference = (ContextCapable)pmRoot.getObjectById(referenceIdentity);
                AlertQuery alertQuery = (AlertQuery)pmRoot.newQuery(Alert.class);
                alertQuery.thereExistsReference().equalTo(alertReference);
                alertQuery.createdAt().greaterThanOrEqualTo(
                	new Date(System.currentTimeMillis() - 1000 * (resendDelayInSeconds == null ? 0 : resendDelayInSeconds.intValue()))
                );
                UserHome userHomeByRoot = (UserHome)pmRoot.getObjectById(userHome.refGetPath());
                List<Alert> existingAlerts = userHomeByRoot.getAlert(alertQuery);
                if(existingAlerts.isEmpty()) {
                	Alert alert = pmRoot.newInstance(Alert.class);
                	alert.setAlertState(new Short((short)1));
                	alert.setName(
                        name == null || name.length() == 0 ?
                            "--" : // name is mandatory
                            name
                    );
                    if(description != null) {
                        alert.setDescription(description);
                    }
                    alert.setImportance(importance);
                    alert.setReference(alertReference);
                    alert.getOwningGroup().clear();
                    alert.getOwningGroup().addAll(
                        userHomeByRoot.getOwningGroup()
                    );
                    // Only owner of alert may delete and update
                    alert.setAccessLevelDelete(SecurityKeys.ACCESS_LEVEL_PRIVATE);
                    alert.setAccessLevelUpdate(SecurityKeys.ACCESS_LEVEL_PRIVATE);
                    pmRoot.currentTransaction().begin();
                    userHomeByRoot.addAlert(
                    	this.getUidAsString(),
                    	alert
                    );
                    pmRoot.currentTransaction().commit();
                    alertIdentities.add(alert.refGetPath());
                    pmRoot.close();
                }
            }
        }
        return alertIdentities;
    }

    /**
     * Assign object to current principal.
     * 
     * @param target
     * @param overwrite
     * @param useRunAsPrincipal
     * @throws ServiceException
     */
    public void assignToMe(
        RefObject_1_0 target,
        boolean overwrite,
        boolean useRunAsPrincipal
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(target);
        String objectClass = target.refClass().refMofId();
        Model_1_0 model = Model_1Factory.getModel();
        ModelElement_1_0 classDef = model.getElement(objectClass);
        Map<String,ModelElement_1_0> attributeDefs = model.getAttributeDefs(classDef, false, true);
        List<String> principalChain = UserObjects.getPrincipalChain(pm);
        if(!principalChain.isEmpty()) {
            UserHome userHome = UserHomes.getInstance().getUserHome(
                target.refGetPath(),
                pm,
                useRunAsPrincipal
            );
            if((userHome != null) && (userHome.getContact() != null)) {
                for(String attribute: ASSIGN_TO_ME_ATTRIBUTES) {
                    if(
                        attributeDefs.keySet().contains(attribute) &&
                        (overwrite || (target.refGetValue(attribute) == null))
                    ) {
                        target.refSetValue(
                        	attribute,
                            userHome.getContact()
                        );
                    }
                }
            }
        }
    }

    /**
     * Counts the number of occurences of items in report and returns a string
     * of the form item0: n0, item1: n1, etc.
     * 
     * @param report
     * @return
     */
    public String analyseReport(
        List<String> report
    ) {
        Map<Object,Short> reportAsMap = new HashMap<Object,Short>();
        for(int i = 0; i < report.size(); i++) {
            Object key = report.get(i);
            Short count = reportAsMap.get(key);
            if(count == null) {
                count = new Short((short)0);
            }
            reportAsMap.put(
                key,
                new Short((short)(count.shortValue() + 1))
            );
        }
        return reportAsMap.toString();
    }
        
    /**
     * Create non-tiny GUI access URL for given object.
     * 
     * @param context
     * @param contextPattern
     * @param object
     * @return
     */
    public String getAccessUrl(
    	Object context,
    	String contextPattern,
    	RefObject_1_0 object
    ) {
    	return this.getAccessUrl(
    		context, 
    		contextPattern, 
    		object, 
    		false
    	);
    }
    
    /**
     * Create GUI access URL for given object.
     * 
     * @param context
     * @param contextPattern
     * @param object
     * @param asTinyUrl
     * @return
     */
    public String getAccessUrl(
    	Object context,
    	String contextPattern,
    	RefObject_1_0 object,
    	boolean asTinyUrl
    ) {
    	if(context instanceof HttpServletRequest) {
    		HttpServletRequest req = (HttpServletRequest)context;
    		String urlPrefix = req.getScheme()+ "://" + req.getServerName() + ":" + req.getServerPort();		
            Action selectObjectAction = 
                new Action(
                    SelectObjectAction.EVENT_ID, 
                    new Action.Parameter[]{
                        new Action.Parameter(Action.PARAMETER_OBJECTXRI, object.refMofId())
                    },
                    "",
                    true
                );        
            String fullUrl =
                urlPrefix + 
                req.getContextPath().replace(contextPattern, "-core-") +  "/" + 
                WebKeys.SERVLET_NAME + 
                "?event=" + SelectObjectAction.EVENT_ID + 
                "&parameter=" + selectObjectAction.getParameter();
            String url = fullUrl;
            if(asTinyUrl) {
	            String tinyUrl = TinyUrlUtils.getTinyUrl(fullUrl);
	            url = tinyUrl != null ? tinyUrl : fullUrl;
            }
            return url;
    	}
    	else if(context instanceof UserHome) {
    		UserHome userHome = (UserHome)context;
    		String urlPrefix = userHome.getWebAccessUrl();
    		if(urlPrefix != null) {
    	        Action selectObjectAction = 
    	            new Action(
    	                SelectObjectAction.EVENT_ID, 
    	                new Action.Parameter[]{
    	                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, object.refMofId())
    	                },
    	                "",
    	                true
    	            );
    	        String fullUrl =
    	            urlPrefix + (urlPrefix.endsWith("/") ? "" : "/") + 
    	            WebKeys.SERVLET_NAME + 
    	            "?event=" + SelectObjectAction.EVENT_ID + 
    	            "&parameter=" + selectObjectAction.getParameter();
    	        String url = fullUrl;
    	        if(asTinyUrl) {
	    	        String tinyUrl = TinyUrlUtils.getTinyUrl(fullUrl);
	    	        url = tinyUrl != null ? tinyUrl : fullUrl;
    	        }
    	        return url;
    		}
    		else {
    			return null;
    		}
    	}
    	else {
    		return null;
    	}
    }

    /**
     * Test if given URL matches application GUI access URL.
     * 
     * @param url
     * @return
     */
    public boolean isAccessUrl(
    	String url
    ) {
    	return
        	url.indexOf(WebKeys.SERVLET_NAME) > 0 || 
        	url.startsWith(TinyUrlUtils.PREFIX);    	
    }

    /**
     * Create media from given input stream and create or update media to given CrxObject.
     * 
     * @param object
     * @param contentType
     * @param contentName
     * @param content
     * @throws IOException
     */
    public void createOrUpdateMedia(
        CrxObject object,
        String contentType,
        String contentName,
        InputStream content
    ) throws IOException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(object);
        Collection<Media> medias = object.getMedia();
        Media media = null;
        for(Media attachment: medias) {
            if((contentName != null) && contentName.equals(attachment.getContentName())) {
                media = attachment;
                break;
            }
        }
        if(media == null) {    	    	
	        media = pm.newInstance(Media.class);
	        object.addMedia(
	            this.getUidAsString(),
	            media
	        );
	        media.setContentName(
	            contentName == null ?
	                Utils.toFilename(contentType) :
	                contentName
	        );
        }
        media.setContentMimeType(contentType);    
        media.setContent(
            BinaryLargeObjects.valueOf(content)
        );
        // inherit the object's owning groups
        media.getOwningGroup().addAll(
            object.getOwningGroup()
        );
    }
    
    /**
     * Returns toString() of the given object. If object is a collection 
     * return toString() of first element.
     * 
     * @param obj
     * @return
     */
    public String toPlain(
        Object obj
    ) {
        String s = obj == null ? "" : 
    		(obj instanceof Collection) && !((Collection<?>)obj).isEmpty() ? 
    			((Collection<?>)obj).iterator().next().toString() : 
    				obj.toString();
    	return s;
    }
    
    /**
     * Callback interface for mapping codes.
     *
     */
    public interface CodeMapper {
    	
    	public String getLocaleText(short locale);
    	
    	public String getCurrencyText(short currency, short locale);
    	
    	public String getCountryText(short country, short locale);
    	
    }

    /**
     * Return display title of given object.
     * 
     * @param refObj
     * @param codes
     * @param texts
     * @param locale
     * @param asShortTitle
     * @return
     * @throws ServiceException
     */
    public String getTitle(
    	RefObject_1_0 refObj,
    	CodeMapper codeMapper,
    	short locale,
    	boolean asShortTitle
    ) throws ServiceException {
    	if(refObj == null) {
    		return "#NULL";
    	}
    	if(JDOHelper.isNew(refObj) || !JDOHelper.isPersistent(refObj)) {
    		return this.toPlain("Untitled");
    	}
    	try {
    		PersistenceManager pm = JDOHelper.getPersistenceManager(refObj);
    		String localeAsString = "en_US";
    		if(codeMapper != null) {
	    		try {
	    			localeAsString = codeMapper.getLocaleText(locale);
	    		} catch(Exception e) {}
    		}
    		String language = "en";
    		if(localeAsString.indexOf("_") > 0) {
    			language = localeAsString.substring(0, 2);
    		}
    		DateFormat dateFormat = Utils.getDateFormat(language);
    		DateFormat timeFormat =  Utils.getTimeFormat(language);
    		DecimalFormat decimalFormat = Utils.getDecimalFormat(language);          
    		if(refObj instanceof Account) {
    			Account obj = (Account)refObj;
    			return this.toPlain(obj.getFullName());
    		} else if(refObj instanceof ProductBasePrice) {
    			ProductBasePrice obj = (ProductBasePrice)refObj;
    			try {
    				return 
    					this.toPlain(obj.getPrice() == null ? "N/A" : decimalFormat.format(obj.getPrice().doubleValue())) + 
    					this.toPlain(" ") + 
    					this.toPlain(codeMapper == null ? obj.getPriceCurrency() : codeMapper.getCurrencyText(obj.getPriceCurrency(), locale));
    			} catch(Exception e) {
    				return 
    					this.toPlain(obj.getPrice() == null ? "N/A" : decimalFormat.format(obj.getPrice().doubleValue())) + 
    					this.toPlain(" N/A");
    			}
    		} else if(refObj instanceof PriceListEntry) {
    			PriceListEntry obj = (PriceListEntry)refObj;
    			return this.toPlain(obj.getProductName());
    		} else if(refObj instanceof Activity) {
    			Activity obj = (Activity)refObj;
    			return 
    				this.toPlain(obj.getActivityNumber()).trim() + 
    				this.toPlain(": ") + 
    				this.toPlain(obj.getName());
    		} else if(refObj instanceof ActivityProcessState) {
    			ActivityProcessState obj = (ActivityProcessState)refObj;
    			return this.toPlain(obj.getName());
    		} else if(refObj instanceof ActivityFollowUp) {
    			ActivityFollowUp followUp = (ActivityFollowUp)refObj;
    			Activity activity = null;
    			// In case of NO_PERMISSION
    			try {
    				activity = (Activity)pm.getObjectById(
    					followUp.refGetPath().getParent().getParent()
    				);
    				activity.getName();
    			} catch(Exception e) {}
    			return activity == null ? "" : 
    				this.getTitle(activity, codeMapper, locale, asShortTitle) + 
    				this.toPlain(": ") + 
    				this.toPlain(followUp.getTitle());
    		} else if(refObj instanceof ActivityGroup) {
    			ActivityGroup obj = (ActivityGroup)refObj;
    			return obj == null ? this.toPlain("Untitled") : 
    				this.toPlain(obj.getName());
    		} else if(refObj instanceof ActivityGroupAssignment) {
    			ActivityGroupAssignment obj = (ActivityGroupAssignment)refObj;
    			return obj == null ? this.toPlain("Untitled") : 
    				this.getTitle(obj.getActivityGroup(), codeMapper, locale, asShortTitle);
    		} else if(refObj instanceof AddressGroupMember) {
    			AddressGroupMember member = (AddressGroupMember)refObj;
    			org.opencrx.kernel.account1.jmi1.AccountAddress address = member.getAddress();
    			if(address instanceof PhoneNumber) {
    				Account account = 
    					(Account)pm.getObjectById(
    						address.refGetPath().getParent().getParent()
    					);
    				return
    					this.getTitle(address, codeMapper, locale, asShortTitle) + 
    					this.toPlain(" / ") + 
    					this.toPlain(account.getFullName());
    			} else {
    				return address  == null ? this.toPlain("Untitled") : 
    					this.getTitle(address, codeMapper, locale, asShortTitle);                
    			}
    		} else if(refObj instanceof AbstractActivityParty) {
    			RefObject_1_0 party = null;
    			if(
    				(refObj instanceof EMailRecipient) ||
    				(refObj instanceof PhoneCallRecipient)
    			) {
    				org.opencrx.kernel.account1.jmi1.AccountAddress address = null;
    				if(refObj instanceof EMailRecipient) {
    					EMailRecipient recipient = (EMailRecipient)refObj;
    					address = recipient.getParty();
    					if(address instanceof EMailAddress) {
    						String title = this.getTitle(address, codeMapper, locale, asShortTitle);
    						if(asShortTitle) {
    							return title;
    						} else {
    							EMail email = (EMail)pm.getObjectById(
    								recipient.refGetPath().getParent().getParent()
    							);
    							String messageSubject = email.getMessageSubject() == null ? "" :
    								URLEncoder.encode(email.getMessageSubject(), "UTF-8").replace("+", "%20");
    							String messageBody = email.getMessageBody() == null ? "" :
    								URLEncoder.encode(email.getMessageBody(), "UTF-8").replace("+", "%20");
    							// Browser limit
    							if(messageBody.length() > 1500) {
    								messageBody = messageBody.substring(0, 1500);
    							}
    							return title + (title.indexOf("@") > 0 ? "?subject=" + messageSubject + "&body=" + messageBody : "");
    						}
    					}
    				} else {
    					PhoneCallRecipient recipient = (PhoneCallRecipient)refObj;
    					address = recipient.getParty();                      
    				}
    				if(address instanceof PhoneNumber) {
    					Account account = 
    						(Account)pm.getObjectById(
    							address.refGetPath().getParent().getParent()
    						);
    					return 
    						this.getTitle(address, codeMapper, locale, asShortTitle) + 
    						this.toPlain(" / ") + 
    						this.toPlain(account.getFullName());
    				} else {
    					return address  == null ? this.toPlain("Untitled") : 
    						this.getTitle(address, codeMapper, locale, asShortTitle);                
    				}
    			} else if(refObj instanceof EMailRecipientGroup) {
    				party = ((EMailRecipientGroup)refObj).getParty();
    			} else if(refObj instanceof IncidentParty) {
    				party = ((IncidentParty)refObj).getParty();
    			} else if(refObj instanceof MailingRecipient) {
    				party = ((MailingRecipient)refObj).getParty();
    			} else if(refObj instanceof MailingRecipientGroup) {
    				party = ((MailingRecipientGroup)refObj).getParty();
    			} else if(refObj instanceof MeetingParty) {
    				party = ((MeetingParty)refObj).getParty();
    			} else if(refObj instanceof TaskParty) {
    				party = ((TaskParty)refObj).getParty();
    			} else if(refObj instanceof PhoneCallRecipientGroup) {
    				party = ((PhoneCallRecipientGroup)refObj).getParty();                  
    			}
    			String emailHint = ((AbstractActivityParty)refObj).getEmailHint();
    			return party == null ? this.toPlain("Untitled") :
    				this.getTitle(party, codeMapper, locale, asShortTitle) + 
    				(emailHint == null ? "" : this.toPlain(" (" + emailHint + ")"));
    		} else if(refObj instanceof org.opencrx.kernel.contract1.jmi1.AccountAddress) {
    			return refObj.refGetValue("address") == null ? this.toPlain("Untitled") : 
    				this.getTitle((RefObject_1_0)refObj.refGetValue("address"), codeMapper, locale, asShortTitle);
    		} else if(refObj instanceof EMailAddressable) {
    			return "* " + this.toPlain(refObj.refGetValue("emailAddress"));
    		} else if(refObj instanceof SalesContractPosition) {
    			return this.toPlain(refObj.refGetValue("lineItemNumber")) + " / " + this.toPlain(refObj.refGetValue("name"));
    		} else if(refObj instanceof AbstractProduct) {
    			AbstractProduct product = (AbstractProduct)refObj;
    			return product.getProductNumber() == null || product.getProductNumber().isEmpty() || product.getProductNumber().equals(product.getName()) ? 
    				this.toPlain(product.getName()) : 
    					this.toPlain(product.getName() + 
    					this.toPlain(" / ") + 
    					this.toPlain(product.getProductNumber())); 
    		} else if(refObj instanceof PostalAddressable) {
    			String address = "";
    			int nLines = 0;
    			@SuppressWarnings("unchecked")
                List<String> postalAddressLines = (List<String>)refObj.refGetValue("postalAddressLine");
    			for(String l: postalAddressLines) {
    				String line = this.toPlain(l);
    				if(nLines > 0) address += "\n";
    				address += line;
    				nLines++;
    			}
    			@SuppressWarnings("unchecked")
                List<String> postalStreetLines = (List<String>)refObj.refGetValue("postalStreet"); 
    			for(String l: postalStreetLines) {
    				String street = this.toPlain(l);
    				if(nLines > 0) address += "\n";
    				address += street;
    				nLines++;
    			}
    			Number postalCountry = (Number)refObj.refGetValue("postalCountry");
    			String postalCountryS = postalCountry == null ? "" : 
    				codeMapper == null ? "" + postalCountry : 
    					this.toPlain(codeMapper.getCountryText(postalCountry.shortValue(), locale));
    			return
    				address + "\n" + 
    				this.toPlain(refObj.refGetValue("postalCode")) + this.toPlain(" ") + 
    				this.toPlain(refObj.refGetValue("postalCity")) + "\n" + 
    				postalCountryS;
    		} else if(refObj instanceof PhoneNumberAddressable) {
    			return "* " + this.toPlain(refObj.refGetValue("phoneNumberFull"));
    		} else if(refObj instanceof UriAddressable) {
    			String uriReference = (String)refObj.refGetValue("uriReference");
    			String uriScheme = (String)refObj.refGetValue("uriScheme");
    			return uriScheme != null && uriReference != null && !uriReference.startsWith(uriScheme)
    				? this.toPlain(uriScheme) + "://" + this.toPlain(uriReference)
    				: this.toPlain(uriReference);
    		} else if(refObj instanceof RoomAddressable) {
    			RoomAddressable obj = (RoomAddressable)refObj;
    			if(refObj instanceof Addressable) { 
    				AbstractBuildingUnit building = ((Addressable)refObj).getBuilding();
    				if(building == null) {
    					return this.toPlain(obj.getRoomNumber());
    				} else {
    					return 
    						this.getTitle(building, codeMapper, locale, asShortTitle) +
    						this.toPlain(" ") +
    						this.toPlain(obj.getRoomNumber());
    				}
    			} else {
    				return this.toPlain(obj.getRoomNumber());
    			}
    		} else if(refObj instanceof org.opencrx.kernel.generic.jmi1.Rating) {
    			return this.toPlain(refObj.refGetValue("ratingLevel"));
    		} else if(refObj instanceof RevenueReport) {
    			return this.toPlain(refObj.refGetValue("reportNumber"));
    		} else if(refObj instanceof WebAddressable) {
    			return "* " + this.toPlain(refObj.refGetValue("webUrl"));
    		} else if(refObj instanceof CodeValueEntry) {
    			return this.toPlain(refObj.refGetValue("shortText"));
    		} else if(refObj instanceof org.opencrx.kernel.generic.jmi1.Description) {
    			return this.toPlain(refObj.refGetValue("language"));
    		} else if(refObj instanceof Document) {
    			Document document = (Document)refObj;
    			if(document.getQualifiedName() != null) {
    				return this.toPlain(document.getQualifiedName());
    			} else if(document.getName() != null) {
    				return this.toPlain(document.getName());                  
    			} else {
    				return this.toPlain(document.getDocumentNumber());
    			}
    		} else if(refObj instanceof org.opencrx.kernel.generic.jmi1.DocumentAttachment) {
    			org.opencrx.kernel.generic.jmi1.DocumentAttachment documentAttachment = (org.opencrx.kernel.generic.jmi1.DocumentAttachment)refObj;
    			if(documentAttachment.getName() == null || documentAttachment.getName().isEmpty()) {
    				if(documentAttachment.getDocument() == null) {
    					return "NA";
    				} else {
    					return this.getTitle(documentAttachment.getDocument(), codeMapper, locale, asShortTitle);
    				}
    			} else {
    				return this.toPlain(documentAttachment.getName());
    			}
    		} else if(refObj instanceof Member) {
    			return (refObj.refGetValue("account") == null ? this.toPlain("Untitled") : 
    				this.getTitle((RefObject_1_0)refObj.refGetValue("account"), codeMapper, locale, asShortTitle));
    		} else if(refObj instanceof ContactRelationship) {
    			return (refObj.refGetValue("toContact") == null ? this.toPlain("Untitled") : 
    				this.getTitle((RefObject_1_0)refObj.refGetValue("toContact"), codeMapper, locale, asShortTitle));
    		} else if(refObj instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
    			org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)refObj;
    			return
    				this.getTitle(userHome.getContact(), codeMapper, locale, asShortTitle) + 
    				this.toPlain(" (") +
    				this.toPlain(userHome.refGetPath().getLastSegment().toString()) +
    				this.toPlain(")");
    		} else if(refObj instanceof org.opencrx.kernel.home1.jmi1.AccessHistory) {
    			return (refObj.refGetValue("reference") == null ? this.toPlain("Untitled") : 
    				this.getTitle((RefObject_1_0)refObj.refGetValue("reference"), codeMapper, locale, asShortTitle));
    		} else if(refObj instanceof WfProcessInstance) {
    			WfProcessInstance wfProcessInstance = (WfProcessInstance)refObj;
    			return wfProcessInstance.getName() != null
    				? this.toPlain(wfProcessInstance.getName())
    				: wfProcessInstance.getProcess() == null 
    					? this.toPlain("Untitled") 
    					: this.getTitle(wfProcessInstance.getProcess(), codeMapper, locale, asShortTitle) + 
    						this.toPlain(" ") + 
    						this.toPlain(refObj.refGetValue("startedOn"));
    		} else if(refObj instanceof org.opencrx.kernel.workflow1.jmi1.WfProcess) {
    			return this.toPlain(((org.opencrx.kernel.workflow1.jmi1.WfProcess)refObj).getName());
    		} else if(refObj instanceof org.opencrx.kernel.base.jmi1.AuditEntry) {
    			Object createdAt = refObj.refGetValue(SystemAttributes.CREATED_AT);
    			return (refObj.refGetValue(SystemAttributes.CREATED_AT) == null) ? this.toPlain("Untitled") : 
    				this.toPlain(dateFormat.format(createdAt)) + 
    				this.toPlain(" ") + 
    				this.toPlain(timeFormat.format(createdAt));
    		} else if(refObj instanceof org.opencrx.kernel.base.jmi1.Note) {
    			return this.toPlain(refObj.refGetValue("title"));
    		} else if(refObj instanceof org.opencrx.kernel.base.jmi1.Chart) {
    			return this.toPlain(refObj.refGetValue("description"));
    		} else if(refObj instanceof org.opencrx.kernel.model1.jmi1.Element) {
    			String title = this.toPlain(refObj.refGetValue("qualifiedName"));              
    			if(title.indexOf(":") > 0) {
    				int pos = title.lastIndexOf(":");
    				title = 
    					title.substring(pos + 1) + 
    					this.toPlain(" (") + 
    					title.substring(0, pos) + 
    					this.toPlain(")");
    			}
    			return title;
    		} else if(refObj instanceof DepotEntity) {
    			DepotEntity obj = (DepotEntity)refObj;
    			return obj.getDepotEntityNumber() == null ? this.toPlain(obj.getName()) : 
    				this.toPlain(obj.getDepotEntityNumber());
    		} else if(refObj instanceof DepotContract) {
    			DepotContract obj = (DepotContract)refObj;
    			return obj.getDepotHolderNumber() == null ? this.toPlain(obj.getName()) : 
    				this.toPlain(obj.getDepotHolderNumber());
    		} else if(refObj instanceof Depot) {
    			Depot obj = (Depot)refObj;
    			return obj.getDepotNumber() == null ? this.toPlain(obj.getName()) : 
    				this.toPlain(obj.getDepotNumber());          
    		} else if(refObj instanceof DepotPosition) {
    			DepotPosition obj = (DepotPosition)refObj;
    			String depotTitle = this.getTitle(obj.getDepot(), codeMapper, locale, asShortTitle);
    			return 
    				depotTitle + 
    				this.toPlain(" / ") + 
    				this.toPlain(obj.getName());                    
    		} else if(refObj instanceof DepotReport) {
    			DepotReport obj = (DepotReport)refObj;
    			String depotTitle = this.getTitle(obj.getDepot(), codeMapper, locale, asShortTitle);
    			return 
    				depotTitle + 
    				this.toPlain(" / ") + 
    				this.toPlain(obj.getName());                    
    		} else if(refObj instanceof DepotReportItemPosition) {
    			DepotReportItemPosition obj = (DepotReportItemPosition)refObj;
    			return this.toPlain(obj.getPositionName());
    		} else if(refObj instanceof SimpleEntry) {
    			SimpleEntry obj = (SimpleEntry)refObj;
    			return this.toPlain(obj.getEntryValue());
    		} else if(refObj instanceof Media) {
    			Media obj = (Media)refObj;
    			return this.toPlain(obj.getContentName());
    		} else if(refObj instanceof ContractRole) {
    			ContractRole contractRole = (ContractRole)refObj;
    			return 
	    			this.getTitle(contractRole.getContract(), codeMapper, locale, asShortTitle) + this.toPlain(" / ") + 
	    			this.getTitle(contractRole.getAccount(), codeMapper, locale, asShortTitle) + this.toPlain(" / ") + 
	    			this.getTitle(contractRole.getContractReferenceHolder(), codeMapper, locale, asShortTitle);
    		} else if(refObj instanceof InvolvedObject) {
    			InvolvedObject involved = (InvolvedObject)refObj;
    			RefObject_1_0 parentInvolved = (RefObject_1_0)pm.getObjectById(involved.refGetPath().getParent().getParent());
    			return
    				this.getTitle(parentInvolved, codeMapper, locale, asShortTitle) + this.toPlain(": ") +
    				this.getTitle(involved.getInvolved(), codeMapper, locale, asShortTitle);
    		} else if(refObj instanceof AccountAssignment) {
    			AccountAssignment obj = (AccountAssignment)refObj;
    			return this.getTitle(obj.getAccount(), codeMapper, locale, asShortTitle);
    		} else if(refObj instanceof AssignedAccountAssignment) {
    			AssignedAccountAssignment obj = (AssignedAccountAssignment)refObj;
    			return getTitle(obj.getAccountAssignment(), codeMapper, locale, asShortTitle);
    		} else if(refObj instanceof Uom) {
    			return this.toPlain(((Uom)refObj).getName());
    		} else if(refObj instanceof AbstractPriceLevel) {
    			return this.toPlain(((AbstractPriceLevel)refObj).getName());
    		} else if(refObj instanceof DocumentRevision) {
    			if(refObj instanceof MediaContent) {
    				return this.toPlain(((MediaContent)refObj).getContentName());
    			} else {
    				return this.toPlain(((DocumentRevision)refObj).getName());
    			}
    		} else if(refObj instanceof InventoryItem) {
				return this.toPlain(((InventoryItem)refObj).getName());   
    		} else if(refObj instanceof ActivityLinkFrom) {
    			ActivityLinkFrom linkFrom = (ActivityLinkFrom)refObj;
    			return 
    				this.toPlain(linkFrom.getName()) + this.toPlain(" (") + 
    				this.getTitle(linkFrom.getLinkFrom(), codeMapper, locale, asShortTitle) + 
    				this.toPlain(")");
    		} else if(refObj instanceof DocumentFolder) {
    			DocumentFolder folder = (DocumentFolder)refObj;
    			return
    				(folder.getParent() == null ? "" : this.getTitle(folder.getParent(), codeMapper, locale, asShortTitle)) +
    				this.toPlain("/") +
    				this.toPlain(folder.getName());
    		} else if(refObj instanceof AggregatedDepotReportItem) {
    			return this.toPlain(((AggregatedDepotReportItem)refObj).getPositionName());
    		} else if(refObj instanceof AbstractContract) {
    			return this.toPlain(((AbstractContract)refObj).getName());
    		} else if(refObj instanceof SingleBooking) {
    			return this.toPlain(((SingleBooking)refObj).getName());
    		} else if(refObj instanceof SimpleBooking) {
    			return this.toPlain(((SimpleBooking)refObj).getName());
    		} else if(refObj instanceof CompoundBooking) {
    			return this.toPlain(((CompoundBooking)refObj).getName());
    		} else {
    			return null;
    		}
    	} catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }

	/**
	 * Counter
	 *
	 */
	public static class Counter {

		public Counter(
			int initialValue
		) {
			this.counter = initialValue;
		}
		
		public void increment(
		) {
			this.counter++;
		}
		
		public int getValue(
		) {
			return this.counter;
		}
		
		private int counter;

	}

	/**
	 * Recursively enable / disable given object(s).
	 * 
	 * @param object
	 * @param disable
	 * @param reason
	 * @param counter
	 * @return
	 */
	public short enableDisableCrxObject(
		final CrxObject object,
		final boolean disable,
		final String reason,
		final Counter counter
	) {
		short status = 0;
		try {
			Utils.traverseObjectTree(
				object,
				null, // referenceFilter
				new Utils.TraverseObjectTreeCallback() {
					@Override
					public Object visit(
						RefObject_1_0 object,
						Object context
					) throws ServiceException {
						try {
							// enable / disable on all objects having attribute 'disabled', e.g. CrxObject
							Boolean isDisabled = (Boolean)object.refGetValue("disabled");
							if(disable) {
								if(!Boolean.TRUE.equals(isDisabled)) {								
									object.refSetValue("disabled", Boolean.TRUE);
									try {
										object.refSetValue("disabledReason", reason);
									} catch(Exception ignore) {}
									if(context instanceof Counter) {
										((Counter)context).increment();
									}
								}
							} else {
								if(Boolean.TRUE.equals(isDisabled)) {
									object.refSetValue("disabled",  Boolean.FALSE);
									try {
										object.refSetValue("disabledReason", reason);
									} catch(Exception ignore) {}
									if(context instanceof Counter) {
										((Counter)context).increment();
									}
								}
							}
						} catch(Exception ignore) {
							// don't care if object does not have attribute 'disabled'
						}
						return context;
					}
				},
				counter
			);
		} catch(Exception e) {
			status = -1;
		}
		return status;
	}

	/**
	 * Disable CrxObject.
	 * 
	 * @param crxObject
	 * @param mode
	 * @param reason
	 * @return
	 */
	public EnableDisableCrxObjectResult disableCrxObject(
		CrxObject object,
		short mode,
		String reason
	) throws ServiceException {
		Counter counter = new Counter(0);
		short status = 0;
		try {
			if(mode == 0) {
				if(!Boolean.TRUE.equals(object.isDisabled())) {
					object.setDisabled(true);
					object.setDisabledReason(reason);
					counter.increment();
				}
			} else {
				status = this.enableDisableCrxObject(
					object, 
					true, 
					reason,
					counter
				);
			}
		} catch(Exception e) {
			status = -1;
			new ServiceException(e).log();
		}
        return Structures.create(
        	EnableDisableCrxObjectResult.class, 
        	Datatypes.member(EnableDisableCrxObjectResult.Member.status, status),
        	Datatypes.member(EnableDisableCrxObjectResult.Member.count, counter.getValue())
        );
	}

    /**
     * Enable CrxObject.
     * 
     * @param crxObject
     * @param mode
     * @param reason
     * @return
     */
    public EnableDisableCrxObjectResult enableCrxObject(
		CrxObject object,
		short mode,
		String reason
    ) throws ServiceException {
		Counter counter = new Counter(0);
		short status = 0;
		try {
			if(mode == 0) {
				if(Boolean.TRUE.equals(object.isDisabled())) {
					object.setDisabled(false);
					counter.increment();
				}
			} else {
				status = this.enableDisableCrxObject(
					object, 
					false, 
					reason, 
					counter
				);
			}
		} catch(Exception e) {
			status = -1;
			new ServiceException(e).log();
		}
        return Structures.create(
        	EnableDisableCrxObjectResult.class, 
        	Datatypes.member(EnableDisableCrxObjectResult.Member.status, status),
        	Datatypes.member(EnableDisableCrxObjectResult.Member.count, counter.getValue())
        );
    }

    /**
     * Get list of types which are subject to be indexed.
     *
     * @return
     */
    public List<Path> getIndexableTypes(
    ) {
    	List<Path> indexableTypes = new ArrayList<Path>();
        Model_1_0 model = Utils.getModel();
        try {
	        for(ModelElement_1_0 element: model.getContent()) {
	        	if(
	        		element.isClassType() &&
	        		model.isSubtypeOf(element, "org:opencrx:kernel:base:Indexed") &&
	        		!model.isSubtypeOf(element, "org:openmdx:base:Segment")
	        	) {
		            Path type = model.getIdentityPattern(element);	    
		            if(type != null) {
		            	indexableTypes.add(type);
		            }
	        	}
	        }
        } catch(ServiceException e) {
        	e.log();
        }
        // Manually add some more
        indexableTypes.addAll(
            Arrays.asList(
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityTracker/:*/followUp/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityMilestone/:*/followUp/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityCategory/:*/followUp/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/account/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/lead/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/opportunity/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/quote/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/salesOrder/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/invoice/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/account/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/lead/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/opportunity/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/quote/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/salesOrder/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/invoice/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/account/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activity/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/lead/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/opportunity/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/quote/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/salesOrder/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/invoice/:*/media/:*")
            )
        );
        return indexableTypes;
    }

    /**
     * Create new result record.
     * 
     * @return
     * @throws ResourceException
     */
    protected ResultRecord newResult(
    ) throws ResourceException {
    	return Records.getRecordFactory().createIndexedRecord(ResultRecord.class);
    }
    
    /**
     * Create new query record.
     * 
     * @param resourceIdentifier
     * @return
     */
    protected QueryRecord newQuery(
        Path resourceIdentifier
    ){
        QueryRecord query = new org.openmdx.base.rest.spi.QueryRecord();
        query.setResourceIdentifier(resourceIdentifier);
        return query;
    }
   
    /**
     * Retrieve object with given identity.
     * 
     * @param resourceIdentifier
     * @param fetchGroupName
     * @return
     * @throws ResourceException
     */
    protected ObjectRecord retrieveObject(
    	RestInteractionCallback restInteractionCallback,
    	Path resourceIdentifier,
    	String fetchGroupName
    ) throws ResourceException {
    	ResultRecord result = this.newResult();
    	QueryRecord query = this.newQuery(resourceIdentifier);
    	query.setFetchGroupName(fetchGroupName);
    	restInteractionCallback.get(
    		query, 
    		result
    	);
    	return result.isEmpty() ? null : (ObjectRecord)result.get(0);
    }

    /**
     * Retrieve objects with given identity matching the given query.
     * 
     * @param resourceIdentifier
     * @param query
     * @param fetchGroupName
     * @return
     * @throws ResourceException
     */
    protected ResultRecord retrieveObjects(
    	RestInteractionCallback restInteractionCallback,
    	Path resourceIdentifier,
    	QueryRecord query,
    	String fetchGroupName
    ) throws ResourceException {
    	ResultRecord result = this.newResult();
    	query.setFetchGroupName(fetchGroupName);
    	restInteractionCallback.find(
    		query, 
    		result
    	);
    	return result;
    }

    /**
     * Extract keywords from object.
     * 
     * @param object
     * @return
     * @throws ServiceException
     */
    public List<String> getKeywords(
    	MappedRecord object,
        Integer keywordLengthMin,
        Integer keywordLengthMax,
        Set<String> indexedAttributes,
        RestInteractionCallback restInteractionCallback
    ) throws ServiceException, ResourceException {
    	Object_2Facade objectFacade = Facades.asObject(object);
    	Path objPath = Object_2Facade.getPath(object);
        List<String> keywords = new ArrayList<String>();
        for(String attribute: indexedAttributes) {
            if(objectFacade.getValue().keySet().contains(attribute)) {
                for(
                    Iterator<Object> j = objectFacade.attributeValuesAsList(attribute).iterator(); 
                    j.hasNext(); 
                ) {
                    Object value = j.next();
                    Reader text = null;
                    boolean isXml = false;
                    if(value instanceof String) {
                        text = new StringReader((String)value);
                    } else if(value instanceof InputStream || value instanceof byte[] || value instanceof BinaryLargeObject) {
                    	if(value instanceof byte[]) {
                    		value = new ByteArrayInputStream((byte[])value);
                    	} else if(value instanceof BinaryLargeObject) {
                    		try {
                    			value = ((BinaryLargeObject)value).getContent();
                    		} catch(Exception e) {}
                    	}
                        String contentName = (String)objectFacade.attributeValuesAsList(attribute + "Name").get(0);
                        String contentMimeType = (String)objectFacade.attributeValuesAsList(attribute + "MimeType").get(0);
                        if(contentName != null) { 
                            if(
                                "text/rtf".equals(contentMimeType) ||
                                contentName.endsWith(".rtf")
                            ) {
                                 try {
                                     text = RTFToText.toTextAsReader(
                                         (InputStream)value
                                     );
                                 } catch(Exception e) {
                                	 SysLog.warning("Cannot extract text from a RTF document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                 }
                            } else if(
                                "application/pdf".equals(contentMimeType) ||
                                contentName.endsWith(".pdf")
                            ) {
                                try {
                                    text = new PDFToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from PDF document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                               "application/vnd.ms-excel".equals(contentMimeType) ||
                               "application/ms-excel".equals(contentMimeType) ||
                                contentName.endsWith(".xls")
                            ) {
                                try {
                                    text = new ExcelToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from Excel document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                               "application/vnd.ms-word".equals(contentMimeType) ||
                               "application/ms-word".equals(contentMimeType) ||
                                contentName.endsWith(".doc")
                            ) {
                                try {
                                    text = new WordToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from Word document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                            	(contentMimeType != null && contentMimeType.startsWith("application/vnd.openxmlformats")) ||
                                contentName.endsWith(".docx") ||
                                contentName.endsWith(".dotx") ||
                                contentName.endsWith(".xlsx") ||
                                contentName.endsWith(".xltx")
                            ) {
                                try {
                                    text = new XmlDocToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from XML document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                                contentName.endsWith(".odt") ||
                                contentName.endsWith(".odp") ||
                                contentName.endsWith(".ods")
                            ) {
                                try {
                                    ZipInputStream document = new ZipInputStream((InputStream)value);
                                    text = new OpenOfficeToText().parse(
                                        document
                                    );
                                    isXml = true;
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from OpenOffice document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                                "text/plain".equals(contentMimeType) ||
                                contentName.endsWith(".txt")
                            ) {
                                text = new InputStreamReader((InputStream)value);                                
                            } else if(
                                "text/html".equals(contentMimeType) ||
                                "text/xml".equals(contentMimeType) ||
                                "application/xml".equals(contentMimeType) ||
                                contentName.endsWith(".xml") || 
                                contentName.endsWith(".html") || 
                                contentName.endsWith(".htm")
                            ) {
                                text = new InputStreamReader((InputStream)value);           
                                isXml = true;
                            }                           
                        }
                    }
                    if(text != null) {
                        try {
                            int ch = text.read();
                            while(ch != -1) {
                                // Skip tags if xml
                                if(isXml && (ch == '<')) {
                                    while(
                                        (ch != -1) &&
                                        (ch != '>')
                                    ) {
                                        ch = text.read();
                                    }
                                    if(ch != -1) {
                                        ch = text.read();
                                    }
                                }
                                StringBuilder keyword = new StringBuilder();
                                boolean isKeyword = false;
                                while(
                                    (ch != -1) && 
                                    (!isXml || (isXml && ch != '<')) &&
                                    Character.isLetterOrDigit((char)ch) || (ch == '-') || (ch == '_') || (ch == '@') || (ch == '.')                                   
                                ) {
                                    keyword.append((char)ch);
                                    ch = text.read();        
                                    isKeyword = true;
                                }
                                if(!isKeyword && (!isXml || (ch != '<'))) {
                                    ch = text.read();
                                } else if(
                                    (keyword.length() >= keywordLengthMin) &&
                                    (keyword.length() < keywordLengthMax)
                                ) {        
                                	String normalizedKeyword = keyword.toString().toLowerCase();
                                	while(normalizedKeyword.endsWith("-") || normalizedKeyword.endsWith("_") || normalizedKeyword.endsWith("@") || normalizedKeyword.endsWith(".")) {
                                		normalizedKeyword = normalizedKeyword.substring(0, normalizedKeyword.length() - 1);
                                	}
                                	if(!keywords.contains(normalizedKeyword)) {
                                		keywords.add(normalizedKeyword);
                                	}
                                }
                            }
                        } catch(Exception e) {}
                    }
                }
            }
        }
        if(Utils.isInstanceOf(object, "org:opencrx:kernel:account1:Account")) {
            // Account: include address keywords
        	QueryRecord query = this.newQuery(objPath.getDescendant("address"));
        	query.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));
        	query.getQueryFilter().getCondition().add(
        		new IsInCondition(
        	        Quantifier.FOR_ALL,        	        
        	        "disabled",
        	        true,
        	        Boolean.FALSE        			
        		)
        	);
        	ResultRecord addresses = this.retrieveObjects(
        		restInteractionCallback, 
        		objPath.getDescendant("address"), 
        		query, 
        		FetchGroup.ALL
        	);
        	for(Object address: addresses) {
        		if(address instanceof MappedRecord) {
    	            keywords.addAll(	            	
    	            	this.getKeywords(
    	            		(MappedRecord)address,
    	            		keywordLengthMin,
    	            		keywordLengthMax,
    	            		indexedAttributes,
    	            		restInteractionCallback
    	            	)
    	            );
        		}
        	}
        }
        return keywords;
    }

    /**
     * Return true if updating of existing index entries is allowed.
     * 
     * @return
     */
    public boolean allowUpdateExistingIndexEntries(
    ) {
    	return true;
    }
    
    /**
     * Create a new index entry or update existing.
     * 
     * @param restInteractionCallback
     * @param indexEntry
     * @throws ServiceException
     * @throws ResourceException
     */
    public void updateIndexEntry(
    	RestInteractionCallback restInteractionCallback,
    	ObjectRecord indexEntry
    ) throws ServiceException, ResourceException {
    	ResultRecord existingIndexEntries = this.newResult();
    	if(this.allowUpdateExistingIndexEntries()) {
	    	QueryRecord query = this.newQuery(indexEntry.getResourceIdentifier().getParent());
	    	query.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));
	    	query.getQueryFilter().getCondition().add(
	    		new IsInCondition(
	    	        Quantifier.THERE_EXISTS,        	        
	    	        "indexedObject",
	    	        true,
	    	        Facades.asObject(indexEntry).attributeValue("indexedObject")
	    		)
	    	);
	    	existingIndexEntries = this.retrieveObjects(
	    		restInteractionCallback, 
	    		indexEntry.getResourceIdentifier().getParent(), 
	    		query, 
	    		FetchGroup.ALL
	    	);
    	}
    	if(!existingIndexEntries.isEmpty()) {
    		ObjectRecord existingIndexEntry = (ObjectRecord)existingIndexEntries.get(0);
    		indexEntry.setResourceIdentifier(existingIndexEntry.getResourceIdentifier());
	    	restInteractionCallback.update(
		    	indexEntry, 
		    	this.newResult()
		    );
    	} else {
	    	restInteractionCallback.create(
		    	indexEntry, 
		    	this.newResult()
		    );
    	}
    }

    /**
     * Get hashable features for given hash name and object.
     * 
     * @param hashable
     * @param name
     * @return
     * @throws ServiceException
     */
    public List<String> getHashableFeatures(
    	Hashable hashable,
    	String name
    ) throws ServiceException {
    	if("Default".equals(name)) {
			Model_1_0 model = Model_1Factory.getModel();
	        ModelElement_1_0 classifier = model.getElement(hashable.refClass().refMofId());
	        Map<String,ModelElement_1_0> attributeDefs = classifier.getModel().getAttributeDefs(
	        	classifier,
	        	false, // sub-types
	            true // includeDerived
	        );
	        List<String> hashableFeatures = new ArrayList<String>(attributeDefs.keySet());
	        hashableFeatures.remove(SystemAttributes.OBJECT_IDENTITY);
	        hashableFeatures.remove(SystemAttributes.MODIFIED_AT);
	        hashableFeatures.remove(SystemAttributes.MODIFIED_BY);
	        hashableFeatures.remove("owner");
	        hashableFeatures.remove("owningGroup");
	        hashableFeatures.remove("owningUser");
	        return hashableFeatures;
    	} else {
    		return null;
    	}
    }

    /**
     * Get hash value.
     * 
     * @param hashable
     * @param name
     * @param deep
     * @return
     * @throws ServiceException
     */
    public String getHashValue(
    	Hashable hashable,
    	String name,
    	boolean deep
    ) throws ServiceException {
    	try {
	    	String hashValue = null;
	        List<String> features = this.getHashableFeatures(hashable, name);
	        if(features != null) {
		        Collections.sort(features);
		        MessageDigest md = MessageDigest.getInstance("MD5");
		        for(String feature : features) {
		        	md.update(
		        		(feature + 
		        		":" + 
		        		hashable.refGetValue(feature)).getBytes()
		        	);
		        }
		        hashValue = Base64.encode(md.digest());
	        }
	        return hashValue;
    	} catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }

    /**
     * Update hashValue. Override this method for custom-specific behaviour.
     *
     * @param hashEntry
     * @param hashValue
     * @throws ServiceException
     */
    public void updateHashValue(
    	Hashable hashable,
    	String name,
    	HashEntry hashEntry,
    	String hashValue
    ) throws ServiceException {
        if(!Utils.areEqual(hashValue, hashEntry.getHashValue())) {
        	hashEntry.setHashValue(hashValue);
        	hashEntry.setHashValueModifiedAt(new Date());
        }
    }
    
    /**
     * Update named hash for given object.
     * 
     * @param hashable
     * @param name
     * @return
     * @throws ServiceException
     */
    public String updateHash(
    	Hashable hashable,
    	String name
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(hashable);
    	String hashValue = null;
		try {
			hashValue = this.getHashValue(
				hashable,
				name,
				true // deep
			);
    		// Update hash entry
    		if(hashValue != null) {
	            HashEntryQuery hashEntryQuery = (HashEntryQuery)pm.newQuery(HashEntry.class);
	            hashEntryQuery.name().equalTo(name);
	            hashEntryQuery.orderByCreatedAt().ascending();
	            HashEntry hashEntry = null;
	            List<HashEntry> hashEntries = hashable.getHashEntry(hashEntryQuery);
	            if(!hashEntries.isEmpty()) {
	            	hashEntry = hashEntries.iterator().next();
	            } else {
	            	hashEntry = pm.newInstance(HashEntry.class);
	            	hashEntry.setName(name);
	            	hashable.addHashEntry(
	            		this.getUidAsString(),
	            		hashEntry
	            	);
	            }
	            this.updateHashValue(
	            	hashable,
	            	name,
	            	hashEntry,
	            	hashValue
	            );
    		}
		} catch(Exception e) {
			throw new ServiceException(e);
		}
    	return hashValue;
    }

	//-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
	public static final String PRIVATE_SUFFIX = "~Private";

    public static final short IMPORT_EXPORT_OK = 0;
    public static final short IMPORT_EXPORT_FORMAT_NOT_SUPPORTED = 1;
    public static final short IMPORT_EXPORT_ITEM_NOT_VALID = 2;
    public static final short IMPORT_EXPORT_MISSING_DATA = 3;
    
    public static final String MIME_TYPE_ZIP = "application/zip";

	public static final String COMMENT_SEPARATOR_BOT = "//"; // at beginning of text
	public static final String COMMENT_SEPARATOR_EOT = " //"; // at end of text

	public static final List<String> ASSIGN_TO_ME_ATTRIBUTES = Arrays.asList("assignedTo", "salesRep", "ratedBy");

}
