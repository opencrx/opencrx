/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SegmentSetupController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.wizard;

import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.account1.jmi1.AddressFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Buildings;
import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.backend.Depots;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.backend.SecureObject;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.jmi1.ExportProfile;
import org.opencrx.kernel.home1.jmi1.QuickAccess;
import org.opencrx.kernel.product1.jmi1.SalesTaxType;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Quantifier;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.CssClass;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.component.View;

/**
 * SegmentSetupController
 *
 */
public class SegmentSetupController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public SegmentSetupController(
	) {
		super();
	}
	

	/**
	 * Find account filter.
	 * 
	 * @param accountFilterName
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.account1.jmi1.AccountFilterGlobal findAccountFilter(
		String accountFilterName,
		org.opencrx.kernel.account1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery query =
		    (org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountFilterGlobal.class);
		query.name().equalTo(accountFilterName);
		Collection<AccountFilterGlobal> accountFilters = segment.getAccountFilter(query);
		if(!accountFilters.isEmpty()) {
			return (org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)accountFilters.iterator().next();
		}
		return null;
	}

	/**
	 * Find address filter.
	 * 
	 * @param accountFilterName
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.account1.jmi1.AddressFilterGlobal findAddressFilter(
		String accountFilterName,
		org.opencrx.kernel.account1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);		
		org.opencrx.kernel.account1.cci2.AddressFilterGlobalQuery query =
		    (org.opencrx.kernel.account1.cci2.AddressFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AddressFilterGlobal.class);
		query.name().equalTo(accountFilterName);
		Collection<AddressFilterGlobal> addressFilters = segment.getAddressFilter(query);
		if(!addressFilters.isEmpty()) {
			return (org.opencrx.kernel.account1.jmi1.AddressFilterGlobal)addressFilters.iterator().next();
		}
		return null;
	}

	/**
	 * Find contract filter.
	 * 
	 * @param contractFilterName
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal findContractFilter(
		String contractFilterName,
		org.opencrx.kernel.contract1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);		
		org.opencrx.kernel.contract1.cci2.ContractFilterGlobalQuery query =
		    (org.opencrx.kernel.contract1.cci2.ContractFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal.class);
		query.name().equalTo(contractFilterName);
		Collection<ContractFilterGlobal> contractFilters = segment.getContractFilter(query);
		if(!contractFilters.isEmpty()) {
			return (org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal)contractFilters.iterator().next();
		}
		return null;
	}

	/**
	 * Find activity filter.
	 * 
	 * @param activityFilterName
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal findActivityFilter(
		String activityFilterName,
		org.opencrx.kernel.activity1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);		
		org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery query =
		    (org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal.class);
		query.name().equalTo(activityFilterName);
		Collection<ActivityFilterGlobal> activityFilters = segment.getActivityFilter(query);
		if(!activityFilters.isEmpty()) {
			return (org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)activityFilters.iterator().next();
		}
		return null;
	}

	/**
	 * Find export profile.
	 * 
	 * @param exportProfileName
	 * @param userHome
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.home1.jmi1.ExportProfile findExportProfile(
		String exportProfileName,
		org.opencrx.kernel.home1.jmi1.UserHome userHome
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(userHome);		
		org.opencrx.kernel.home1.cci2.ExportProfileQuery query =
		    (org.opencrx.kernel.home1.cci2.ExportProfileQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.ExportProfile.class);
		query.name().equalTo(exportProfileName);
		Collection<ExportProfile> exportProfiles = userHome.getExportProfile(query);
		if(!exportProfiles.isEmpty()) {
			return (org.opencrx.kernel.home1.jmi1.ExportProfile)exportProfiles.iterator().next();
		}
		return null;
	}

	/**
	 * Find document.
	 * 
	 * @param documentName
	 * @param segment
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.document1.jmi1.Document findDocument(
		String documentName,
		org.opencrx.kernel.document1.jmi1.Segment segment
	) throws ServiceException {
		return Documents.getInstance().findDocument(
			documentName,
			segment
		);
	}

	/**
	 * Find favorite.
	 * 
	 * @param favoriteName
	 * @param userHome
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.home1.jmi1.QuickAccess findFavorite(
	   String favoriteName,
	   org.opencrx.kernel.home1.jmi1.UserHome userHome
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(userHome);		
		org.opencrx.kernel.home1.cci2.QuickAccessQuery query = (org.opencrx.kernel.home1.cci2.QuickAccessQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.QuickAccess.class);
		query.thereExistsName().equalTo(favoriteName);
		Collection<QuickAccess> favorites = userHome.getQuickAccess(query);
		if(!favorites.isEmpty()) {
			return (org.opencrx.kernel.home1.jmi1.QuickAccess)favorites.iterator().next();
		}
		return null;
	}

	/**
	 * Find document folder.
	 * 
	 * @param documentFolderName
	 * @param segment
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.document1.jmi1.DocumentFolder findDocumentFolder(
		String documentFolderName,
		org.opencrx.kernel.document1.jmi1.Segment segment
	) throws ServiceException {
		return Documents.getInstance().findDocumentFolder(
			documentFolderName,
			segment
		);
	}

	/**
	 * Init account filter.
	 * 
	 * @param filterName
	 * @param filterProperties
	 * @param pm
	 * @param segment
	 * @param allUsers
	 * @return
	 */
	public org.opencrx.kernel.account1.jmi1.AccountFilterGlobal initAccountFilter(
		String filterName,
		org.opencrx.kernel.account1.jmi1.AccountFilterProperty[] filterProperties,
		org.opencrx.kernel.account1.jmi1.Segment segment,
		List<PrincipalGroup> allUsers
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.account1.jmi1.AccountFilterGlobal accountFilter = findAccountFilter(
			filterName,
			segment
		);
		if(accountFilter != null) return accountFilter;
		try {
			pm.currentTransaction().begin();
			accountFilter = pm.newInstance(org.opencrx.kernel.account1.jmi1.AccountFilterGlobal.class);
			accountFilter.setName(filterName);
			accountFilter.getOwningGroup().addAll(allUsers);
			segment.addAccountFilter(
				false,
				Accounts.getInstance().getUidAsString(),
				accountFilter
			);
			for(int i = 0; i < filterProperties.length; i++) {
				filterProperties[i].getOwningGroup().addAll(allUsers);
				accountFilter.addAccountFilterProperty(
					false,
					Accounts.getInstance().getUidAsString(),
					filterProperties[i]
				);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return accountFilter;
	}

	/**
	 * Init address filter.
	 * 
	 * @param filterName
	 * @param filterProperties
	 * @param pm
	 * @param segment
	 * @param allUsers
	 * @return
	 */
	public org.opencrx.kernel.account1.jmi1.AddressFilterGlobal initAddressFilter(
		String filterName,
		org.opencrx.kernel.account1.jmi1.AddressFilterProperty[] filterProperties,
		org.opencrx.kernel.account1.jmi1.Segment segment,
		List<PrincipalGroup> allUsers
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.account1.jmi1.AddressFilterGlobal addressFilter = findAddressFilter(
			filterName,
			segment
		);
		if(addressFilter != null) return addressFilter;
		try {
			pm.currentTransaction().begin();
			addressFilter = pm.newInstance(org.opencrx.kernel.account1.jmi1.AddressFilterGlobal.class);
			addressFilter.setName(filterName);
			addressFilter.getOwningGroup().addAll(allUsers);
			segment.addAddressFilter(
				false,
				Accounts.getInstance().getUidAsString(),
				addressFilter
			);
			for(int i = 0; i < filterProperties.length; i++) {
				filterProperties[i].getOwningGroup().addAll(allUsers);
				addressFilter.addAddressFilterProperty(
					false,
					Accounts.getInstance().getUidAsString(),
					filterProperties[i]
				);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return addressFilter;
	}

	/**
	 * Init contract filter.
	 * 
	 * @param filterName
	 * @param filterProperties
	 * @param pm
	 * @param segment
	 * @param allUsers
	 * @return
	 */
	public org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal initContractFilter(
		String filterName,
		org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[] filterProperties,
		org.opencrx.kernel.contract1.jmi1.Segment segment,
		List<PrincipalGroup> allUsers
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal contractFilter = findContractFilter(
			filterName,
			segment
		);
		if(contractFilter != null) return contractFilter;
		try {
			pm.currentTransaction().begin();
			contractFilter = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal.class);
			contractFilter.setName(filterName);
			contractFilter.getOwningGroup().addAll(allUsers);
			segment.addContractFilter(
				false,
				Contracts.getInstance().getUidAsString(),
				contractFilter
			);
			for(int i = 0; i < filterProperties.length; i++) {
				filterProperties[i].getOwningGroup().addAll(allUsers);
				contractFilter.addFilterProperty(
					false,
					Contracts.getInstance().getUidAsString(),
					filterProperties[i]
				);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return contractFilter;
	}

	/**
	 * Init activity filter.
	 * 
	 * @param filterName
	 * @param filterProperties
	 * @param pm
	 * @param segment
	 * @param allUsers
	 * @return
	 */
	public org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal initActivityFilter(
		String filterName,
		org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[] filterProperties,
		org.opencrx.kernel.activity1.jmi1.Segment segment,
		List<PrincipalGroup> allUsers
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal activityFilter = findActivityFilter(
			filterName,
			segment
		);
		if(activityFilter != null) return activityFilter;
		try {
			pm.currentTransaction().begin();
			activityFilter = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal.class);
			activityFilter.setName(filterName);
			activityFilter.getOwningGroup().addAll(allUsers);
			segment.addActivityFilter(
				false,
				Activities.getInstance().getUidAsString(),
				activityFilter
			);
			for(int i = 0; i < filterProperties.length; i++) {
				filterProperties[i].getOwningGroup().addAll(allUsers);
				activityFilter.addFilterProperty(
					false,
					Activities.getInstance().getUidAsString(),
					filterProperties[i]
				);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return activityFilter;
	}

	/**
	 * Init document filter.
	 * 
	 * @param documentFolderName
	 * @param segment
	 * @param allUsers
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.document1.jmi1.DocumentFolder initDocumentFolder(
		String documentFolderName,
		org.opencrx.kernel.document1.jmi1.Segment segment,
		List<org.opencrx.security.realm1.jmi1.PrincipalGroup> allUsers
	) throws ServiceException {
		return Documents.getInstance().initDocumentFolder(
			documentFolderName,
			segment,
			allUsers
		);
	}

	/**
	 * Init document.
	 * 
	 * @param documentName
	 * @param revisionURL
	 * @param revisionMimeType
	 * @param revisionName
	 * @param documentFolder
	 * @param segment
	 * @param allUsers
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.document1.jmi1.Document initDocument(
		String documentName,
		URL revisionURL,
		String revisionMimeType,
		String revisionName,
		org.opencrx.kernel.document1.jmi1.DocumentFolder documentFolder,
		org.opencrx.kernel.document1.jmi1.Segment segment,
		List<org.opencrx.security.realm1.jmi1.PrincipalGroup> allUsers
	) throws ServiceException {
		return Documents.getInstance().initDocument(
			documentName,
			documentName,
			revisionURL,
			revisionMimeType,
			revisionName,
			documentFolder,
			segment,
			allUsers
		);
	}

	/**
	 * Init export profile.
	 * 
	 * @param exportProfileName
	 * @param forClass
	 * @param mimeType
	 * @param exportParams
	 * @param template
	 * @param pm
	 * @param userHome
	 * @param allUsers
	 * @return
	 */
	public org.opencrx.kernel.home1.jmi1.ExportProfile initExportProfile(
		String exportProfileName,
		String[] forClass,
		String mimeType,
		String exportParams,
		org.opencrx.kernel.document1.jmi1.Document template,
		org.opencrx.kernel.home1.jmi1.UserHome userHome,
		List<PrincipalGroup> allUsers
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(userHome);
		org.opencrx.kernel.home1.jmi1.ExportProfile exportProfile = findExportProfile(
			exportProfileName,
			userHome
		);
		try {
			pm.currentTransaction().begin();
			if(exportProfile == null)  {
				exportProfile = pm.newInstance(org.opencrx.kernel.home1.jmi1.ExportProfile.class);
				exportProfile.setName(exportProfileName);
				exportProfile.getForClass().addAll(
					Arrays.asList(forClass)
				);
				exportProfile.getOwningGroup().addAll(allUsers);
				userHome.addExportProfile(
					Activities.getInstance().getUidAsString(),
					exportProfile
				);
			}
			if(exportProfile.getMimeType() == null || exportProfile.getMimeType().isEmpty()) {
				exportProfile.setMimeType(mimeType);
			}
			if(exportProfile.getExportParams() == null || exportProfile.getExportParams().isEmpty()) {
				exportProfile.setExportParams(exportParams);
			}
			if(exportProfile.getTemplate() == null) {
				exportProfile.setTemplate(template);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return exportProfile;
	}

	/**
	 * Init favorite.
	 * 
	 * @param favoriteName
	 * @param reference
	 * @param iconKey
	 * @param action
	 * @param pm
	 * @param userHome
	 * @return
	 */
	public org.opencrx.kernel.home1.jmi1.QuickAccess initFavorite(
		String favoriteName,
		org.openmdx.base.jmi1.ContextCapable reference,
		String iconKey,
		String action,
		org.opencrx.kernel.home1.jmi1.UserHome userHome
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(userHome);
		org.opencrx.kernel.home1.jmi1.QuickAccess favorite = findFavorite(
		    favoriteName,
			userHome
		);
		try {
			pm.currentTransaction().begin();
			if(favorite == null) {
				favorite = pm.newInstance(org.opencrx.kernel.home1.jmi1.QuickAccess.class);
				favorite.setName(favoriteName);
				favorite.getOwningGroup().addAll(
					userHome.getOwningGroup()
				);
				favorite.setActionType((short)1); // Javascript
				userHome.addQuickAccess(
					Activities.getInstance().getUidAsString(),
					favorite
				);
			}
			if(favorite.getReference() == null) {
				favorite.setReference(reference);
			}
			if(favorite.getDescription() == null || favorite.getDescription().isEmpty()) {
				favorite.setDescription(favoriteName);
			}
			if(favorite.getIconKey() == null || favorite.getIconKey().isEmpty()) {
				favorite.setIconKey(iconKey);
			}
			if(favorite.getActionName() == null || favorite.getActionName().isEmpty()) {
				favorite.setActionName(action);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return favorite;
	}

	/**
	 * Init sales tax type.
	 * 
	 * @param name
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.product1.jmi1.SalesTaxType findSalesTaxType(
		String name,
		org.opencrx.kernel.product1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.product1.cci2.SalesTaxTypeQuery query =
		    (org.opencrx.kernel.product1.cci2.SalesTaxTypeQuery)pm.newQuery(org.opencrx.kernel.product1.jmi1.SalesTaxType.class);
		query.name().equalTo(name);
		Collection<SalesTaxType> salesTaxTypes = segment.getSalesTaxType(query);
		if(!salesTaxTypes.isEmpty()) {
			return (org.opencrx.kernel.product1.jmi1.SalesTaxType)salesTaxTypes.iterator().next();
		}
		return null;
	}

	/**
	 * Init sales tax type.
	 * 
	 * @param name
	 * @param rate
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.product1.jmi1.SalesTaxType initSalesTaxType(
		String name,
		java.math.BigDecimal rate,
		org.opencrx.kernel.product1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);		
		org.opencrx.kernel.product1.jmi1.SalesTaxType salesTaxType = findSalesTaxType(
		    name,
			segment
		);
		if(salesTaxType != null) return salesTaxType;
		try {
			pm.currentTransaction().begin();
			salesTaxType = pm.newInstance(org.opencrx.kernel.product1.jmi1.SalesTaxType.class);
			salesTaxType.setName(name);
			salesTaxType.setRate(rate);
			salesTaxType.getOwningGroup().addAll(
				segment.getOwningGroup()
			);
			segment.addSalesTaxType(
				Contracts.getInstance().getUidAsString(),
				salesTaxType
			);
			pm.currentTransaction().commit();
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return salesTaxType;
	}
	
	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(
	) {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);		
	}
	
	public void renderSetupReport(
		Writer out
	) throws ServiceException, IOException {
		out.append("<div class=\"row\">");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Activity and Incident Management</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Activity Processes</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityProcess(Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityProcess(Activities.ACTIVITY_PROCESS_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_PROCESS_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityProcess(Activities.ACTIVITY_PROCESS_NAME_BULK_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_PROCESS_NAME_BULK_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Calendars</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findCalendar(Activities.CALENDAR_NAME_DEFAULT_BUSINESS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.CALENDAR_NAME_DEFAULT_BUSINESS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Activity Types</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_BUGS_AND_FEATURES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_BUGS_AND_FEATURES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_BULK_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_BULK_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Activity Trackers</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_BUGS_AND_FEATURES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_BUGS_AND_FEATURES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_POLLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_POLLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_MEETING_ROOMS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_MEETING_ROOMS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_PUBLIC, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_PUBLIC + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Activity Creators</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td  class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_BUGS_AND_FEATURES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_BUGS_AND_FEATURES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_POLLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_POLLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_MEETING_ROOMS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_MEETING_ROOMS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Workflows and Topics</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Topics</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ACCOUNT_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ACCOUNT_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ACTIVITY_FOLLOWUP_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ACTIVITY_FOLLOWUP_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ACTIVITY_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ACTIVITY_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_EMAIL, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_EMAIL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_TWITTER, this.getWorkflowSegment()) == null ? MISSING : OK)  + "\">" + Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_TWITTER + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_JABBER, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_JABBER + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_BOOKING_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_BOOKING_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_COMPETITOR_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_COMPETITOR_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_COMPOUND_BOOKING_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_COMPOUND_BOOKING_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_INVOICE_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_INVOICE_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_LEAD_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_LEAD_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_OPPORTUNITY_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_OPPORTUNITY_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ORGANIZATION_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ORGANIZATION_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_PRODUCT_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_PRODUCT_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_QUOTE_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_QUOTE_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_TIMER_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_TIMER_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_SALES_ORDER_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_SALES_ORDER_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Workflows</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_EXPORT_MAIL, this.getWorkflowSegment()) == null && Workflows.getInstance().findWfProcess("org.opencrx.mail.workflow.ExportMailWorkflow", this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_EXPORT_MAIL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_PRINT_CONSOLE, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_PRINT_CONSOLE + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_SEND_ALERT, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_SEND_ALERT + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_SEND_MAIL, this.getWorkflowSegment()) == null && Workflows.getInstance().findWfProcess("org.opencrx.mail.workflow.SendMailWorkflow", this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_SEND_MAIL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_SEND_MAIL_NOTIFICATION, this.getWorkflowSegment()) == null && Workflows.getInstance().findWfProcess("org.opencrx.mail.workflow.SendMailNotificationWorkflow", this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_SEND_MAIL_NOTIFICATION + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Products</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Pricing Rules</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Products.getInstance().findPricingRule(Products.PRICING_RULE_NAME_LOWEST_PRICE, this.getProductSegment()) == null ? MISSING : OK) + "\">" + Products.PRICING_RULE_NAME_LOWEST_PRICE + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Sales Tax Types</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findSalesTaxType(SegmentSetupController.SALES_TAX_TYPE_NAME_8_5, this.getProductSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.SALES_TAX_TYPE_NAME_8_5 + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Contracts</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Calculation Rules</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findCalculationRule(Contracts.CALCULATION_RULE_NAME_DEFAULT, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Contracts.CALCULATION_RULE_NAME_DEFAULT + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Documents</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Message of the day</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.MESSAGE_OF_THE_DAY_DOCUMENT_NAME, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.MESSAGE_OF_THE_DAY_DOCUMENT_NAME + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Reports</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Account Filters</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findAccountFilter(SegmentSetupController.ACCOUNT_FILTER_NAME_ALL, this.getAccountSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.ACCOUNT_FILTER_NAME_ALL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findAccountFilter(SegmentSetupController.ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD, this.getAccountSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findAddressFilter(SegmentSetupController.ADDRESS_FILTER_NAME_ALL, this.getAccountSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.ADDRESS_FILTER_NAME_ALL + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Contract Filters</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findContractFilter(SegmentSetupController.CONTRACT_FILTER_NAME_LEAD_FORECAST, this.getContractSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.CONTRACT_FILTER_NAME_LEAD_FORECAST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findContractFilter(SegmentSetupController.CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST, this.getContractSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findContractFilter(SegmentSetupController.CONTRACT_FILTER_NAME_QUOTE_FORECAST, this.getContractSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.CONTRACT_FILTER_NAME_QUOTE_FORECAST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findContractFilter(SegmentSetupController.CONTRACT_FILTER_NAME_WON_LEADS, this.getContractSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.CONTRACT_FILTER_NAME_WON_LEADS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findContractFilter(SegmentSetupController.CONTRACT_FILTER_NAME_WON_OPPORTUNITIES, this.getContractSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.CONTRACT_FILTER_NAME_WON_OPPORTUNITIES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findContractFilter(SegmentSetupController.CONTRACT_FILTER_NAME_WON_QUOTES, this.getContractSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.CONTRACT_FILTER_NAME_WON_QUOTES + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Activity Filters</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findActivityFilter(SegmentSetupController.ACTIVITY_FILTER_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.ACTIVITY_FILTER_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findActivityFilter(SegmentSetupController.ACTIVITY_FILTER_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.ACTIVITY_FILTER_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findActivityFilter(SegmentSetupController.ACTIVITY_FILTER_NAME_NEW_ACTIVITIES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.ACTIVITY_FILTER_NAME_NEW_ACTIVITIES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findActivityFilter(SegmentSetupController.ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Export Profiles</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findExportProfile(SegmentSetupController.EXPORT_PROFILE_NAME_CONTRACT_LIST, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.EXPORT_PROFILE_NAME_CONTRACT_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findExportProfile(SegmentSetupController.EXPORT_PROFILE_NAME_CONTRACT_WITH_POSITION_LIST, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.EXPORT_PROFILE_NAME_CONTRACT_WITH_POSITION_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findExportProfile(SegmentSetupController.EXPORT_PROFILE_NAME_ACTIVITY_LIST, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.EXPORT_PROFILE_NAME_ACTIVITY_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findExportProfile(SegmentSetupController.EXPORT_PROFILE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.EXPORT_PROFILE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findExportProfile(SegmentSetupController.EXPORT_PROFILE_NAME_ACCOUNT_MEMBER_LIST, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.EXPORT_PROFILE_NAME_ACCOUNT_MEMBER_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findExportProfile(SegmentSetupController.EXPORT_PROFILE_NAME_ACCOUNT_LIST, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.EXPORT_PROFILE_NAME_ACCOUNT_LIST + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Mail Merge Templates</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.MAILMERGE_TEMPLATE_NAME_LETTER, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.MAILMERGE_TEMPLATE_NAME_LETTER + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.MAILMERGE_TEMPLATE_NAME_LABEL, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.MAILMERGE_TEMPLATE_NAME_LABEL + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Report Templates</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.REPORT_TEMPLATE_NAME_CONTRACT_LIST, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.REPORT_TEMPLATE_NAME_CONTRACT_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.REPORT_TEMPLATE_NAME_CONTRACT_WITH_POSITION_LIST, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.REPORT_TEMPLATE_NAME_CONTRACT_WITH_POSITION_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.REPORT_TEMPLATE_NAME_ACTIVITY_LIST, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.REPORT_TEMPLATE_NAME_ACTIVITY_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.REPORT_TEMPLATE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.REPORT_TEMPLATE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.REPORT_TEMPLATE_NAME_ACCOUNT_MEMBER_LIST, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.REPORT_TEMPLATE_NAME_ACCOUNT_MEMBER_LIST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findDocument(SegmentSetupController.REPORT_TEMPLATE_NAME_ACCOUNT_LIST, this.getDocumentSegment()) == null ? MISSING : OK) + "\">" + SegmentSetupController.REPORT_TEMPLATE_NAME_ACCOUNT_LIST + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Menues</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Favorites</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findFavorite(SegmentSetupController.FAVORITE_NAME_CREATE_ACTIVITY, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.FAVORITE_NAME_CREATE_ACTIVITY + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findFavorite(SegmentSetupController.FAVORITE_NAME_CREATE_CONTACT, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.FAVORITE_NAME_CREATE_CONTACT + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findFavorite(SegmentSetupController.FAVORITE_NAME_CREATE_CONTRACT, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.FAVORITE_NAME_CREATE_CONTRACT + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findFavorite(SegmentSetupController.FAVORITE_NAME_CREATE_LEAD, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.FAVORITE_NAME_CREATE_LEAD + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (this.findFavorite(SegmentSetupController.FAVORITE_NAME_SCHEDULE_EVENT, this.getUserHome()) == null ? MISSING : OK) + "\">" + SegmentSetupController.FAVORITE_NAME_SCHEDULE_EVENT + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("</div>");
	}

	/**
	 * Refresh action.
	 * 
	 * @throws ServiceException
	 */
	public void doRefresh(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.currentUserIsAdmin = app.getCurrentUserRole().equals(
			org.opencrx.kernel.generic.SecurityKeys.ADMIN_PRINCIPAL + org.opencrx.kernel.generic.SecurityKeys.ID_SEPARATOR + this.getSegmentName() + "@" + this.getSegmentName()
		);
		this.accountSegment = Accounts.getInstance().getAccountSegment(pm, this.getProviderName(), this.getSegmentName());
		this.activitySegment = Activities.getInstance().getActivitySegment(pm, this.getProviderName(), this.getSegmentName());
		this.productSegment = Products.getInstance().getProductSegment(pm, this.getProviderName(), this.getSegmentName());
		this.contractSegment = Contracts.getInstance().getContractSegment(pm, this.getProviderName(), this.getSegmentName());
		this.documentSegment = Documents.getInstance().getDocumentSegment(pm, this.getProviderName(), this.getSegmentName());
		this.workflowSegment = Workflows.getInstance().getWorkflowSegment(pm, this.getProviderName(), this.getSegmentName());
		this.depotSegment = Depots.getInstance().getDepotSegment(pm, this.getProviderName(), this.getSegmentName());
		this.buildingSegment = Buildings.getInstance().getBuildingSegment(pm, this.getProviderName(), this.getSegmentName());
		this.userHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
			app.getUserHomeIdentityAsPath()
		);
	}

	/**
	 * Setup action.
	 * 
	 * @throws ServiceException
	 */
	public void doSetup(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh();
		try {
			pm.currentTransaction().begin();
			activitySegment.setAccessLevelBrowse((short)3);
			pm.currentTransaction().commit();
			org.opencrx.security.realm1.jmi1.PrincipalGroup publicPrincipalGroup =
				SecureObject.getInstance().initPrincipalGroup(
					"Public",
					pm,
					this.getProviderName(),
					this.getSegmentName()
				);
			org.opencrx.security.realm1.jmi1.PrincipalGroup usersPrincipalGroup =
				SecureObject.getInstance().initPrincipalGroup(
					"Users",
					pm,
					this.getProviderName(),
					this.getSegmentName()
				);
			org.opencrx.security.realm1.jmi1.PrincipalGroup administratorsPrincipalGroup =
				SecureObject.getInstance().initPrincipalGroup(
					"Administrators",
					pm,
					this.getProviderName(),
					this.getSegmentName()
				);
			List<org.opencrx.security.realm1.jmi1.PrincipalGroup> allUsers = new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
			allUsers.add(usersPrincipalGroup);
			allUsers.add(administratorsPrincipalGroup);

			Workflows.getInstance().initWorkflows(
				pm,
				this.getProviderName(),
				this.getSegmentName()
			);
			org.opencrx.kernel.activity1.jmi1.ActivityProcess bulkEmailProcess =
				Activities.getInstance().initBulkEmailProcess(
					pm,
					this.getProviderName(),
					this.getSegmentName(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			org.opencrx.kernel.activity1.jmi1.ActivityProcess emailProcess =
				Activities.getInstance().initEmailProcess(
					pm,
					this.getProviderName(),
					this.getSegmentName(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			org.opencrx.kernel.activity1.jmi1.ActivityProcessState emailProcessStateNew = null;
			org.opencrx.kernel.activity1.jmi1.ActivityProcessState emailProcessStateOpen = null;
			for(Iterator<ActivityProcessState> i = emailProcess.<ActivityProcessState>getState().iterator(); i.hasNext(); ) {
				org.opencrx.kernel.activity1.jmi1.ActivityProcessState state = i.next();
				if("New".equals(state.getName())) {
					emailProcessStateNew = state;
				}
				if("Open".equals(state.getName())) {
					emailProcessStateOpen = state;
				}
			}
			org.opencrx.kernel.activity1.jmi1.ActivityProcess bugAndFeatureTrackingProcess =
				Activities.getInstance().initBugAndFeatureTrackingProcess(
					pm,
					this.getProviderName(),
					this.getSegmentName(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			org.opencrx.kernel.activity1.jmi1.ActivityProcessState bugAndFeatureTrackingProcessStateNew = null;
			org.opencrx.kernel.activity1.jmi1.ActivityProcessState bugAndFeatureTrackingProcessStateInProgress  = null;
			for(Iterator<ActivityProcessState> i = bugAndFeatureTrackingProcess.<ActivityProcessState>getState().iterator(); i.hasNext(); ) {
				org.opencrx.kernel.activity1.jmi1.ActivityProcessState state = i.next();
				if("New".equals(state.getName())) {
					bugAndFeatureTrackingProcessStateNew = state;
				}
				if("In Progress".equals(state.getName())) {
					bugAndFeatureTrackingProcessStateInProgress = state;
				}
			}
			Activities.getInstance().initCalendar(
				Activities.CALENDAR_NAME_DEFAULT_BUSINESS,
				pm,
				this.getProviderName(),
				this.getSegmentName(),
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
			// Activity Types
			org.opencrx.kernel.activity1.jmi1.ActivityType bugsAndFeaturesType =
				Activities.getInstance().initActivityType(
					bugAndFeatureTrackingProcess,
					Activities.ACTIVITY_TYPE_NAME_BUGS_AND_FEATURES,
					Activities.ActivityClass.INCIDENT.getValue(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			@SuppressWarnings("unused")
            org.opencrx.kernel.activity1.jmi1.ActivityType bulkEmailsType =
				Activities.getInstance().initActivityType(
					bulkEmailProcess,
					Activities.ACTIVITY_TYPE_NAME_BULK_EMAILS,
					Activities.ActivityClass.EMAIL.getValue(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			org.opencrx.kernel.activity1.jmi1.ActivityType emailsType =
				Activities.getInstance().initActivityType(
					emailProcess,
					Activities.ACTIVITY_TYPE_NAME_EMAILS,
					Activities.ActivityClass.EMAIL.getValue(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			org.opencrx.kernel.activity1.jmi1.ActivityType tasksType =
				Activities.getInstance().initActivityType(
					bugAndFeatureTrackingProcess,
					Activities.ACTIVITY_TYPE_NAME_TASKS,
					Activities.ActivityClass.TASK.getValue(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			org.opencrx.kernel.activity1.jmi1.ActivityType meetingsType =
				Activities.getInstance().initActivityType(
					bugAndFeatureTrackingProcess,
					Activities.ACTIVITY_TYPE_NAME_MEETINGS,
					Activities.ActivityClass.MEETING.getValue(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			org.opencrx.kernel.activity1.jmi1.ActivityType phoneCallsType =
				Activities.getInstance().initActivityType(
					bugAndFeatureTrackingProcess,
					Activities.ACTIVITY_TYPE_NAME_PHONE_CALLS,
					Activities.ActivityClass.PHONE_CALL.getValue(),
					allUsers,
					SecurityKeys.ACCESS_LEVEL_PRIVATE
				);
			// Activity Trackers
			org.opencrx.kernel.activity1.jmi1.ActivityTracker bugsAndFeaturesTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_BUGS_AND_FEATURES,
					allUsers,
					this.activitySegment
				);
			org.opencrx.kernel.activity1.jmi1.ActivityTracker emailsTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_EMAILS,
					allUsers,
					this.activitySegment
				);
			org.opencrx.kernel.activity1.jmi1.ActivityTracker tasksTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_TASKS,
					allUsers,
					this.activitySegment
				);
			org.opencrx.kernel.activity1.jmi1.ActivityTracker pollsTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_POLLS,
					allUsers,
					this.activitySegment
				);
			org.opencrx.kernel.activity1.jmi1.ActivityTracker meetingRoomsTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_MEETING_ROOMS,
					allUsers,
					this.activitySegment
				);
			org.opencrx.kernel.activity1.jmi1.ActivityTracker meetingsTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_MEETINGS,
					allUsers,
					this.activitySegment
				);
			org.opencrx.kernel.activity1.jmi1.ActivityTracker phoneCallsTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_PHONE_CALLS,
					allUsers,
					this.activitySegment
				);
			org.opencrx.kernel.activity1.jmi1.ActivityTracker publicTracker =
				Activities.getInstance().initActivityTracker(
					Activities.ACTIVITY_TRACKER_NAME_PUBLIC,
					Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup}),
					this.activitySegment
				);
			// Activity Creators
			try {
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_BUGS_AND_FEATURES,					
					bugsAndFeaturesType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{bugsAndFeaturesTracker}),
					allUsers
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_EMAILS,
					emailsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{emailsTracker}),
					allUsers
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_TASKS,
					tasksType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{tasksTracker}),
					allUsers
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_POLLS,
					emailsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{pollsTracker}),
					allUsers
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_MEETING_ROOMS,
					emailsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{meetingRoomsTracker}),
					allUsers
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_MEETINGS,
					meetingsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{meetingsTracker}),
					allUsers
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_PHONE_CALLS,
					phoneCallsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{phoneCallsTracker}),
					allUsers
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_PUBLIC_EMAILS,
					emailsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
					Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_PUBLIC_TASKS,
					tasksType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
					Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_PUBLIC_MEETINGS,
					meetingsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
					Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
				);
				Activities.getInstance().initActivityCreator(
					Activities.ACTIVITY_CREATOR_NAME_PUBLIC_PHONE_CALLS,
					phoneCallsType,
					Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
					Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
				);
			} catch (Exception e) {
				new ServiceException(e).log();
				try {
					pm.currentTransaction().rollback();
				} catch (Exception re) {}
			}
			
			// PricingRule
			Products.getInstance().initPricingRule(
				Products.PRICING_RULE_NAME_LOWEST_PRICE,
				Products.PRICING_RULE_DESCRIPTION_LOWEST_PRICE,
				Products.PRICING_RULE_GET_PRICE_LEVEL_SCRIPT_LOWEST_PRICE,
				pm,
				this.getProviderName(),
				this.getSegmentName()
			);
			// SalesTaxType
			initSalesTaxType(
			    SALES_TAX_TYPE_NAME_8_5,
			    new java.math.BigDecimal(8.5),
			    productSegment
			);
			// CalculationRule
			Contracts.getInstance().initCalculationRule(
				Contracts.CALCULATION_RULE_NAME_DEFAULT,
				null,
				Contracts.DEFAULT_GET_POSITION_AMOUNTS_SCRIPT,
				Contracts.DEFAULT_GET_CONTRACT_AMOUNTS_SCRIPT,
				pm,
				this.getProviderName(),
				this.getSegmentName()
			);

			// AccountFilter

			// ACCOUNT_FILTER_NAME_ALL
			initAccountFilter(
				ACCOUNT_FILTER_NAME_ALL,
				new org.opencrx.kernel.account1.jmi1.AccountFilterProperty[]{},
				accountSegment,
				allUsers
			);

			// ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD
			org.opencrx.kernel.account1.jmi1.AccountQueryFilterProperty accountQueryFilterProperty = pm.newInstance(org.opencrx.kernel.account1.jmi1.AccountQueryFilterProperty .class);
			accountQueryFilterProperty.setName("external_link is null or vcard is null");
			accountQueryFilterProperty.setActive(new Boolean (true));
			accountQueryFilterProperty.setClause("object_id IN (\n" +
              "  select oocke1_account.object_id from oocke1_account, oocke1_account_\n" +
              "  where oocke1_account.object_id = oocke1_account_.object_id\n" +
              "  and oocke1_account_.idx=0\n" +
              "  and ((oocke1_account_.external_link is null) or (oocke1_account.vcard is null))\n" +
              "  )"
			);
			@SuppressWarnings("unused")
            org.opencrx.kernel.account1.jmi1.AccountFilterGlobal accountFilterBrokenVcard = initAccountFilter(
			ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD,
			  new org.opencrx.kernel.account1.jmi1.AccountFilterProperty[]{
			      accountQueryFilterProperty
			  },
			  accountSegment,
			  allUsers
			);
			// ADDRESS_FILTER_NAME_ALL
			initAddressFilter(
				ADDRESS_FILTER_NAME_ALL,
				new org.opencrx.kernel.account1.jmi1.AddressFilterProperty[]{},
				accountSegment,
				allUsers
			);

			// ContractFilter

			// CONTRACT_FILTER_NAME_LEAD_FORECAST
			org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty.class);
			contractTypeFilterProperty.setName("Lead");
			contractTypeFilterProperty.setActive(new Boolean (true));
			contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Lead");
			org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty contractQueryFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty .class);
			contractQueryFilterProperty.setName("Estimated close date >= Today");
			contractQueryFilterProperty.setActive(new Boolean (true));
			contractQueryFilterProperty.setClause("(v.estimated_close_date >= now())");
			initContractFilter(
				CONTRACT_FILTER_NAME_LEAD_FORECAST,
				new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
					contractTypeFilterProperty,
					contractQueryFilterProperty
				},
				contractSegment,
				allUsers
			);

			// CONTRACT_FILTER_NAME_WON_LEADS
			contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
			contractTypeFilterProperty.setName("Lead");
			contractTypeFilterProperty.setActive(new Boolean (true));
			contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Lead");
			org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty contractStateFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty.class);
			contractStateFilterProperty.setName("Won");
			contractStateFilterProperty.setActive(new Boolean (true));
			contractStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractStateFilterProperty.getContractState().add(new Short((short)1110));
			initContractFilter(
				CONTRACT_FILTER_NAME_WON_LEADS,
				new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
					contractTypeFilterProperty,
					contractStateFilterProperty
				},
				contractSegment,
				allUsers
			);

			// CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST
			contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
			contractTypeFilterProperty.setName("Opportunity");
			contractTypeFilterProperty.setActive(new Boolean (true));
			contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Opportunity");
			contractQueryFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty .class);
			contractQueryFilterProperty.setName("Estimated close date >= Today");
			contractQueryFilterProperty.setActive(new Boolean (true));
			contractQueryFilterProperty.setClause("(v.estimated_close_date >= now())");
			initContractFilter(
				CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST,
				new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
					contractTypeFilterProperty,
					contractQueryFilterProperty
				},
				contractSegment,
				allUsers
			);

			// CONTRACT_FILTER_NAME_WON_OPPORTUNITIES
			contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
			contractTypeFilterProperty.setName("Opportunity");
			contractTypeFilterProperty.setActive(new Boolean (true));
			contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Opportunity");
			contractStateFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty.class);
			contractStateFilterProperty.setName("Won");
			contractStateFilterProperty.setActive(new Boolean (true));
			contractStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractStateFilterProperty.getContractState().add(new Short((short)1210));
			initContractFilter(
				CONTRACT_FILTER_NAME_WON_OPPORTUNITIES,
				new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
					contractTypeFilterProperty,
					contractStateFilterProperty
				},
				contractSegment,
				allUsers
			);

			// CONTRACT_FILTER_NAME_QUOTE_FORECAST
			contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
			contractTypeFilterProperty.setName("Quote");
			contractTypeFilterProperty.setActive(new Boolean (true));
			contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Quote");
			contractQueryFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty .class);
			contractQueryFilterProperty.setName("Estimated close date >= Today");
			contractQueryFilterProperty.setActive(new Boolean (true));
			contractQueryFilterProperty.setClause("(v.estimated_close_date >= now())");
			initContractFilter(
				CONTRACT_FILTER_NAME_QUOTE_FORECAST,
				new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
					contractTypeFilterProperty,
					contractQueryFilterProperty
				},
				contractSegment,
				allUsers
			);

			// CONTRACT_FILTER_NAME_WON_QUOTES
			contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
			contractTypeFilterProperty.setName("Quote");
			contractTypeFilterProperty.setActive(new Boolean (true));
			contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Quote");
			contractStateFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty.class);
			contractStateFilterProperty.setName("Won");
			contractStateFilterProperty.setActive(new Boolean (true));
			contractStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			contractStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			contractStateFilterProperty.getContractState().add(new Short((short)1310));
			initContractFilter(
				CONTRACT_FILTER_NAME_WON_QUOTES,
				new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
					contractTypeFilterProperty,
					contractStateFilterProperty
				},
				contractSegment,
				allUsers
			);

			// ActivityFilter

			// ACTIVITY_FILTER_NAME_PHONE_CALLS
			org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
			activityTypeFilterProperty.setName("Phone Calls");
			activityTypeFilterProperty.setActive(new Boolean (true));
			activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			activityTypeFilterProperty.getActivityType().add(phoneCallsType);
			initActivityFilter(
				ACTIVITY_FILTER_NAME_PHONE_CALLS,
				new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
					activityTypeFilterProperty
				},
				activitySegment,
				allUsers
			);

			// ACTIVITY_FILTER_NAME_MEETINGS
			activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
			activityTypeFilterProperty.setName("Meetings");
			activityTypeFilterProperty.setActive(new Boolean (true));
			activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			activityTypeFilterProperty.getActivityType().add(meetingsType);
			initActivityFilter(
				ACTIVITY_FILTER_NAME_MEETINGS,
				new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
					activityTypeFilterProperty
				},
				activitySegment,
				allUsers
			);

			// ACTIVITY_FILTER_NAME_NEW_ACTIVITIES
			activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
			activityTypeFilterProperty.setName("All Types");
			activityTypeFilterProperty.setActive(new Boolean (true));
			activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			activityTypeFilterProperty.getActivityType().add(bugsAndFeaturesType);
			activityTypeFilterProperty.getActivityType().add(meetingsType);
			activityTypeFilterProperty.getActivityType().add(emailsType);
			activityTypeFilterProperty.getActivityType().add(phoneCallsType);
			org.opencrx.kernel.activity1.jmi1.ActivityProcessStateFilterProperty activityProcessStateFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityProcessStateFilterProperty.class);
			activityProcessStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			activityProcessStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			activityProcessStateFilterProperty.getProcessState().add(bugAndFeatureTrackingProcessStateNew);
			activityProcessStateFilterProperty.getProcessState().add(emailProcessStateNew);
			initActivityFilter(
				ACTIVITY_FILTER_NAME_NEW_ACTIVITIES,
				new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
					activityTypeFilterProperty,
					activityProcessStateFilterProperty
				},
				activitySegment,
				allUsers
			);

			// ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES
			activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
			activityTypeFilterProperty.setName("All Types");
			activityTypeFilterProperty.setActive(new Boolean (true));
			activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			activityTypeFilterProperty.getActivityType().add(bugsAndFeaturesType);
			activityTypeFilterProperty.getActivityType().add(meetingsType);
			activityTypeFilterProperty.getActivityType().add(emailsType);
			activityTypeFilterProperty.getActivityType().add(phoneCallsType);
			activityProcessStateFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityProcessStateFilterProperty.class);
			activityProcessStateFilterProperty.setName("Open");
			activityProcessStateFilterProperty.setActive(new Boolean (true));
			activityProcessStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
			activityProcessStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
			activityProcessStateFilterProperty.getProcessState().add(bugAndFeatureTrackingProcessStateInProgress);
			activityProcessStateFilterProperty.getProcessState().add(emailProcessStateOpen);
			initActivityFilter(
				ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES,
				new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
					activityTypeFilterProperty,
					activityProcessStateFilterProperty
				},
				activitySegment,
				allUsers
			);

			// MessageOfTheDay
			org.opencrx.kernel.document1.jmi1.DocumentFolder messageOfTheDayFolder = initDocumentFolder(
				MESSAGE_OF_THE_DAY_FOLDER_NAME,
				documentSegment,
				allUsers
			);
			@SuppressWarnings("unused")
            org.opencrx.kernel.document1.jmi1.Document messageOfTheDay = initDocument(
				MESSAGE_OF_THE_DAY_DOCUMENT_NAME,
				this.getRequest().getServletContext().getResource("/documents/" + MESSAGE_OF_THE_DAY_DOCUMENT_NAME),
				"text/html",
				MESSAGE_OF_THE_DAY_DOCUMENT_NAME,
				messageOfTheDayFolder,
				documentSegment,
				allUsers
			);

			// Mail Merge Templates
			org.opencrx.kernel.document1.jmi1.DocumentFolder templateFolder = initDocumentFolder(
				MAILMERGE_TEMPLATE_FOLDER_NAME,
				documentSegment,
				allUsers
			);
			@SuppressWarnings("unused")
            org.opencrx.kernel.document1.jmi1.Document templateMailMergeLetter = initDocument(
				MAILMERGE_TEMPLATE_NAME_LETTER,
				this.getRequest().getServletContext().getResource("/documents/Template_MailMergeLetter.rtf"),
				"text/rtf",
				MAILMERGE_TEMPLATE_NAME_LETTER,
				templateFolder,
				documentSegment,
				allUsers
			);
			@SuppressWarnings("unused")
            org.opencrx.kernel.document1.jmi1.Document templateMailMergeEtiquette = initDocument(
				MAILMERGE_TEMPLATE_NAME_LABEL,
				this.getRequest().getServletContext().getResource("/documents/Template_MailMergeLabel.rtf"),
				"text/rtf",
				MAILMERGE_TEMPLATE_NAME_LABEL,
				templateFolder,
				documentSegment,
				allUsers
			);
			// Report Templates
			templateFolder = initDocumentFolder(
				REPORT_TEMPLATE_FOLDER_NAME,
				documentSegment,
				allUsers
			);
			org.opencrx.kernel.document1.jmi1.Document templateContractList = initDocument(
				REPORT_TEMPLATE_NAME_CONTRACT_LIST,
				this.getRequest().getServletContext().getResource("/documents/Template_ContractList.xls"),
				"application/x-excel",
				REPORT_TEMPLATE_NAME_CONTRACT_LIST,
				templateFolder,
				documentSegment,
				allUsers
			);
			org.opencrx.kernel.document1.jmi1.Document templateContractWithPositionList = initDocument(
				REPORT_TEMPLATE_NAME_CONTRACT_WITH_POSITION_LIST,
				this.getRequest().getServletContext().getResource("/documents/Template_ContractWithPositionList.xls"),
				"application/x-excel",
				REPORT_TEMPLATE_NAME_CONTRACT_WITH_POSITION_LIST,
				templateFolder,
				documentSegment,
				allUsers
			);
			org.opencrx.kernel.document1.jmi1.Document templateActivityList = initDocument(
				REPORT_TEMPLATE_NAME_ACTIVITY_LIST,
				this.getRequest().getServletContext().getResource("/documents/Template_ActivityList.xls"),
				"application/x-excel",
				REPORT_TEMPLATE_NAME_ACTIVITY_LIST,
				templateFolder,
				documentSegment,
				allUsers
			);
			org.opencrx.kernel.document1.jmi1.Document templateActivityWithFollowUpList = initDocument(
				REPORT_TEMPLATE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST,
				this.getRequest().getServletContext().getResource("/documents/Template_ActivityListWithFollowups.xls"),
				"application/x-excel",
				REPORT_TEMPLATE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST,
				templateFolder,
				documentSegment,
				allUsers
			);
			org.opencrx.kernel.document1.jmi1.Document templateAccountMemberList = initDocument(
				REPORT_TEMPLATE_NAME_ACCOUNT_MEMBER_LIST,
				this.getRequest().getServletContext().getResource("/documents/Template_MemberList.xls"),
				"application/x-excel",
				REPORT_TEMPLATE_NAME_ACCOUNT_MEMBER_LIST,
				templateFolder,
				documentSegment,
				allUsers
			);
			org.opencrx.kernel.document1.jmi1.Document templateAccountList = initDocument(
				REPORT_TEMPLATE_NAME_ACCOUNT_LIST,
				this.getRequest().getServletContext().getResource("/documents/Template_FilteredAccountList.xls"),
				"application/x-excel",
				REPORT_TEMPLATE_NAME_ACCOUNT_LIST,
				templateFolder,
				documentSegment,
				allUsers
			);
			// ExportProfile
			initExportProfile(
				EXPORT_PROFILE_NAME_CONTRACT_LIST,
				new String[]{
					"org:opencrx:kernel:contract1:ContractFilterGlobal"
				},
				"application/x-excel",
					"xri:@openmdx:org.opencrx.kernel.code1/provider/" + this.getProviderName() + "/segment/Root/valueContainer/currency\n" +
					"$\n" +
					"filteredContract,customer,salesRep,entry\n" +
					"!\n" +
					"ContractFilter[XRI;IDX;name;object_class],\n" +
					"FilteredContract[XRI;IDX;name;contractNumber;customer;salesRep;contractState;priority;contractCurrency;totalBaseAmount;totalDiscountAmount;totalAmount;totalTaxAmount;totalAmountIncludingTax;totalSalesCommission],\n" +
					"Account[XRI;IDX;fullName;aliasName;lastName;firstName;vcard;object_class],\n" +
					"Entry[XRI;IDX;ID;shortText;longText]",
				templateContractList,
				userHome,
				allUsers
			);
			initExportProfile(
				EXPORT_PROFILE_NAME_CONTRACT_WITH_POSITION_LIST,
				new String[]{
					"org:opencrx:kernel:contract1:ContractFilterGlobal"
				},
				"application/x-excel",
					"xri:@openmdx:org.opencrx.kernel.code1/provider/" + this.getProviderName() + "/segment/Root/valueContainer/currency\n" +
					"$\n" +
					"filteredContract,customer,salesRep,position,product,address,salesTaxType,entry\n" +
					"!\n" +
					"ContractFilter[XRI;IDX;name;object_class],\n" +
					"FilteredContract[XRI;IDX;name;contractNumber;customer;salesRep;contractState;priority;contractCurrency;totalBaseAmount;totalDiscountAmount;totalAmount;totalTaxAmount;totalAmountIncludingTax;totalSalesCommission],\n" +
					"Position[XRI;IDX;name;positionNumber;contractPositionState;quantity;pricePerUnit;pricingState;uom;priceUom;salesTaxType;discount;discountIsPercentage;salesCommission;salesCommissionIsPercentage;baseAmount;discountAmount;amount;taxAmount],\n" +
					"Product[XRI;IDX;name;productNumber],\n" +
					"Account[XRI;IDX;fullName;aliasName;lastName;firstName;vcard;object_class],\n" +
					"Address[XRI;IDX;isMain;usage;phoneNumberFull;postalCity;postalCode;postalCountry;postalState;postalAddressLine;postalStreet;emailAddress;webUrl;object_class]\n" +
					"SalesTaxType[XRI;IDX;name;rate],\n" +
					"Entry[XRI;IDX;ID;shortText;longText]",
				templateContractWithPositionList,
				userHome,
				allUsers
			);
			initExportProfile(
				EXPORT_PROFILE_NAME_ACTIVITY_LIST,
				new String[]{
					"org:opencrx:kernel:activity1:AbstractFilterActivity",
					"org:opencrx:kernel:activity1:ActivityGroup",
				},
				"application/x-excel",
					"filteredActivity,assignedTo,address,processState,lastTransition\n" +
					"!\n" +
					"ActivityFilter[XRI;IDX;name;object_class],\n" +
					"ActivityTracker[XRI;IDX;name;object_class],\n" +
					"ActivityMilestone[XRI;IDX;name;object_class],\n" +
					"ActivityCategory[XRI;IDX;name;object_class],\n" +
					"FilteredActivity[XRI;IDX;activityNumber;name;scheduledStart;scheduledEnd;dueBy;priority;processState;assignedTo;lastTransition;percentComplete;object_class],\n" +
					"Account[XRI;IDX;fullName;aliasName;lastName;firstName;vcard;object_class]",
				templateActivityList,
				userHome,
				allUsers
			);
			initExportProfile(
				EXPORT_PROFILE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST,
				new String[]{
					"org:opencrx:kernel:activity1:AbstractFilterActivity",
					"org:opencrx:kernel:activity1:ActivityGroup"
				},
				"application/x-excel",
					"filteredActivity,followUp,assignedTo,address,processState,lastTransition\n" +
					"!\n" +
					"ActivityFilter[XRI;IDX;name;object_class],\n" +
					"ActivityTracker[XRI;IDX;name;object_class],\n" +
					"ActivityMilestone[XRI;IDX;name;object_class],\n" +
					"ActivityCategory[XRI;IDX;name;object_class],\n" +
					"FilteredActivity[XRI;IDX;activityNumber;name;scheduledStart;scheduledEnd;dueBy;priority;processState;assignedTo;lastTransition;percentComplete;object_class],\n" +
					"FollowUp[XRI;IDX;title;text;object_class],\n" +
					"Account[XRI;IDX;fullName;aliasName;lastName;firstName;vcard;object_class]",
				templateActivityWithFollowUpList,
				userHome,
				allUsers
			);
			initExportProfile(
				EXPORT_PROFILE_NAME_ACCOUNT_MEMBER_LIST,
				new String[]{
					"org:opencrx:kernel:account1:Contact",
					"org:opencrx:kernel:account1:AbstractGroup"
				},
				"application/x-excel",
					"member,account,address[2]\n" +
					"!\n" +
					"Member[XRI;IDX;account],\n" +
					"Account[XRI;IDX;fullName;aliasName;lastName;firstName;vcard;object_class],\n" +
					"Address[XRI;IDX;isMain;usage;phoneNumberFull;postalCity;postalCode;postalCountry;postalState;postalAddressLine;postalStreet;emailAddress;webUrl;object_class]",
				templateAccountMemberList,
				userHome,
				allUsers
			);
			initExportProfile(
				EXPORT_PROFILE_NAME_ACCOUNT_LIST,
				new String[]{
					"org:opencrx:kernel:account1:AbstractFilterAccount"
				},
				"application/x-excel",
					"filteredAccount,address\n" +
					"!\n" +
					"FilteredAccount[XRI;IDX;fullName;aliasName;lastName;firstName;vcard;object_class],\n" +
					"Address[XRI;IDX;isMain;usage;phoneNumberFull;postalCity;postalCode;postalCountry;postalState;postalAddressLine;postalStreet;emailAddress;webUrl;object_class]",
				templateAccountList,
				userHome,
				allUsers
			);

			// Favorites
			initFavorite(
			    FAVORITE_NAME_CREATE_ACTIVITY,
			    activitySegment,
			    "ActivityManagement.gif",
			    "$('UserDialogWait').className='loading udwait';jQuery.ajax({type: 'get', url: './wizards/en_US/CreateActivityWizard.jsp?" + Action.PARAMETER_REQUEST_ID + "=" + View.REQUEST_ID_TEMPLATE + "&" + Action.PARAMETER_OBJECTXRI + "=xri:@openmdx:org.opencrx.kernel.activity1/provider/" + this.getProviderName() + "/segment/" + this.getSegmentName() + "', dataType: 'html', success: function(data){$('UserDialog').innerHTML=data;evalScripts(data);}});",
				userHome
			);
			initFavorite(
			    FAVORITE_NAME_CREATE_CONTACT,
			    accountSegment,
			    "Account.gif",
			    "$('UserDialogWait').className='loading udwait';jQuery.ajax({type: 'get', url: './wizards/en_US/CreateContactWizard.jsp?" + Action.PARAMETER_REQUEST_ID + "=" + View.REQUEST_ID_TEMPLATE + "&" + Action.PARAMETER_OBJECTXRI + "=xri:@openmdx:org.opencrx.kernel.account1/provider/" + this.getProviderName() + "/segment/" + this.getSegmentName() + "', dataType: 'html', success: function(data){$('UserDialog').innerHTML=data;evalScripts(data);}});",
				userHome
			);
			initFavorite(
			    FAVORITE_NAME_CREATE_CONTRACT,
			    contractSegment,
			    "SalesOrder.gif",
			    "$('UserDialogWait').className='loading udwait';jQuery.ajax({type: 'get', url: './wizards/en_US/CreateContractWizard.jsp?" + Action.PARAMETER_REQUEST_ID + "=" + View.REQUEST_ID_TEMPLATE + "&" + Action.PARAMETER_OBJECTXRI + "=xri:@openmdx:org.opencrx.kernel.contract1/provider/" + this.getProviderName() + "/segment/" + this.getSegmentName() + "', dataType: 'html', success: function(data){$('UserDialog').innerHTML=data;evalScripts(data);}});",
				userHome
			);
			initFavorite(
			    FAVORITE_NAME_CREATE_LEAD,
			    contractSegment,
			    "Lead.gif",
			    "$('UserDialogWait').className='loading udwait';jQuery.ajax({type: 'get', url: './wizards/en_US/CreateLeadWizard.jsp?" + Action.PARAMETER_REQUEST_ID + "=" + View.REQUEST_ID_TEMPLATE + "&" + Action.PARAMETER_OBJECTXRI + "=xri:@openmdx:org.opencrx.kernel.contract1/provider/" + this.getProviderName() + "/segment/" + this.getSegmentName() + "', dataType: 'html', success: function(data){$('UserDialog').innerHTML=data;evalScripts(data);}});",
				userHome
			);
			initFavorite(
			    FAVORITE_NAME_SCHEDULE_EVENT,
			    activitySegment,
			    "Meeting.gif",
			    "$('UserDialogWait').className='loading udwait';window.location.href='./wizards/en_US/ScheduleEventWizard.jsp?" + Action.PARAMETER_REQUEST_ID + "=" + View.REQUEST_ID_TEMPLATE + "&" + Action.PARAMETER_OBJECTXRI + "=" + URLEncoder.encode("xri://@openmdx*org.opencrx.kernel.activity1/provider/" + this.getProviderName() + "/segment/" + this.getSegmentName()) + "';",
				userHome
			);
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
			new ServiceException(e).log();
		}
	}

	/**
	 * @return the currentUserIsAdmin
	 */
	public boolean isCurrentUserIsAdmin() {
		return currentUserIsAdmin;
	}


	/**
	 * @return the accountSegment
	 */
	public org.opencrx.kernel.account1.jmi1.Segment getAccountSegment() {
		return accountSegment;
	}


	/**
	 * @return the activitySegment
	 */
	public org.opencrx.kernel.activity1.jmi1.Segment getActivitySegment() {
		return activitySegment;
	}


	/**
	 * @return the productSegment
	 */
	public org.opencrx.kernel.product1.jmi1.Segment getProductSegment() {
		return productSegment;
	}


	/**
	 * @return the contractSegment
	 */
	public org.opencrx.kernel.contract1.jmi1.Segment getContractSegment() {
		return contractSegment;
	}


	/**
	 * @return the documentSegment
	 */
	public org.opencrx.kernel.document1.jmi1.Segment getDocumentSegment() {
		return documentSegment;
	}

	/**
	 * @return the workflowSegment
	 */
	public org.opencrx.kernel.workflow1.jmi1.Segment getWorkflowSegment() {
		return workflowSegment;
	}	

	/**
	 * @return the userHome
	 */
	public org.opencrx.kernel.home1.jmi1.UserHome getUserHome() {
		return userHome;
	}

	/**
	 * @return the depotSegment
	 */
	public org.opencrx.kernel.depot1.jmi1.Segment getDepotSegment() {
		return depotSegment;
	}
	
	/**
	 * @return the buildingSegment
	 */
	public org.opencrx.kernel.building1.jmi1.Segment getBuildingSegment() {
		return buildingSegment;
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String ACCOUNT_FILTER_NAME_ALL = "All Accounts";
	public static final String ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD = "Accounts with missing or broken vCard";
	public static final String ADDRESS_FILTER_NAME_ALL = "All Addresses";

	public static final String CONTRACT_FILTER_NAME_LEAD_FORECAST = "Lead Forecast";
	public static final String CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST = "Opportunity Forecast";
	public static final String CONTRACT_FILTER_NAME_QUOTE_FORECAST = "Quote Forecast";
	public static final String CONTRACT_FILTER_NAME_WON_LEADS = "Won Leads";
	public static final String CONTRACT_FILTER_NAME_WON_OPPORTUNITIES = "Won Opportunities";
	public static final String CONTRACT_FILTER_NAME_WON_QUOTES = "Won Quotes";

	public static final String ACTIVITY_FILTER_NAME_PHONE_CALLS = "Phone Calls";
	public static final String ACTIVITY_FILTER_NAME_NEW_ACTIVITIES = "New Activities";
	public static final String ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES = "Open Activities";
	public static final String ACTIVITY_FILTER_NAME_MEETINGS = "Meetings";

	public static final String EXPORT_PROFILE_NAME_CONTRACT_LIST = "Contract List (Excel)";
	public static final String EXPORT_PROFILE_NAME_CONTRACT_WITH_POSITION_LIST = "Contract List with Positions (Excel)";
	public static final String EXPORT_PROFILE_NAME_ACTIVITY_LIST = "Activity List (Excel)";
	public static final String EXPORT_PROFILE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST = "Activity List with Follow-Ups (Excel)";
	public static final String EXPORT_PROFILE_NAME_ACCOUNT_MEMBER_LIST = "Account Member List (Excel)";
	public static final String EXPORT_PROFILE_NAME_ACCOUNT_LIST = "Account List (Excel)";

	public static final String MAILMERGE_TEMPLATE_FOLDER_NAME = "Mail Merge Templates";
	public static final String MAILMERGE_TEMPLATE_NAME_LETTER = "Letter Template";
	public static final String MAILMERGE_TEMPLATE_NAME_LABEL = "Label Template";
	public static final String MESSAGE_OF_THE_DAY_FOLDER_NAME = "Message of the day";
	public static final String MESSAGE_OF_THE_DAY_DOCUMENT_NAME = "Message of the day.html";

	public static final String SALES_TAX_TYPE_NAME_8_5 = "Sales Tax 8.5%";

	public static final String REPORT_TEMPLATE_FOLDER_NAME = "Report Templates";
	public static final String REPORT_TEMPLATE_NAME_CONTRACT_LIST = "Contract Report Template";
	public static final String REPORT_TEMPLATE_NAME_CONTRACT_WITH_POSITION_LIST = "Contract with Positions Report Template";
	public static final String REPORT_TEMPLATE_NAME_ACTIVITY_LIST = "Activity Report Template";
	public static final String REPORT_TEMPLATE_NAME_ACTIVITY_WITH_FOLLOWUP_LIST = "Activity With Follow-Ups Report Template";
	public static final String REPORT_TEMPLATE_NAME_ACCOUNT_MEMBER_LIST = "Account Members Report Template";
	public static final String REPORT_TEMPLATE_NAME_ACCOUNT_LIST = "Account Report Template";

	public static final String FAVORITE_NAME_CREATE_ACTIVITY = "Create Activity";
	public static final String FAVORITE_NAME_CREATE_CONTACT = "Create Contact";
	public static final String FAVORITE_NAME_CREATE_CONTRACT = "Create Contract";
	public static final String FAVORITE_NAME_CREATE_LEAD = "Create Lead";
	public static final String FAVORITE_NAME_SCHEDULE_EVENT = "Schedule Event";

	public static final String OK = "alert-success";
	public static final String MISSING = "alert-danger";

	private boolean currentUserIsAdmin;
	private org.opencrx.kernel.account1.jmi1.Segment accountSegment;
	private org.opencrx.kernel.activity1.jmi1.Segment activitySegment;
	private org.opencrx.kernel.product1.jmi1.Segment productSegment;
	private org.opencrx.kernel.contract1.jmi1.Segment contractSegment;
	private org.opencrx.kernel.document1.jmi1.Segment documentSegment;	
	private org.opencrx.kernel.depot1.jmi1.Segment depotSegment;	
	private org.opencrx.kernel.building1.jmi1.Segment buildingSegment;
	private org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment;
	private org.opencrx.kernel.home1.jmi1.UserHome userHome;
}
