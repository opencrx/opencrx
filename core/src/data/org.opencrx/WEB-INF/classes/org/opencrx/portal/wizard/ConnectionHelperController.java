/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ConnectionHelperController
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.portal.wizard;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Map;
import java.util.TreeMap;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.home1.cci2.CalendarProfileQuery;
import org.opencrx.kernel.home1.cci2.CardProfileQuery;
import org.opencrx.kernel.home1.cci2.DocumentProfileQuery;
import org.opencrx.kernel.home1.jmi1.CalendarProfile;
import org.opencrx.kernel.home1.jmi1.CardProfile;
import org.opencrx.kernel.home1.jmi1.DocumentProfile;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JspWizardController;

/**
 * ConnectionHelperController
 *
 */
public class ConnectionHelperController extends JspWizardController {

    /**
     * SelectorType
     *
     */
    public enum SelectorType {
    	CALENDARPROFILE,
    	DOCUMENTPROFILE,
    	CARDPROFILE
    }

	/**
	 * Constructor.
	 * 
	 */
	public ConnectionHelperController(
	) {
		super();
	}
	
	public void doReload(
		@JspWizardController.RequestParameter(name = "isInitialized") Boolean isInitialized,
		@JspWizardController.RequestParameter(name = "mustReload") Boolean mustReload,
		@JspWizardController.RequestParameter(name = "anchorObjectXri") String anchorObjectXri,
		@JspWizardController.RequestParameter(name = "selectorType") String selectorType,
		@JspWizardController.RequestParameter(name = "optionMax") Integer optionMax,
		@JspWizardController.RequestParameter(name = "optionUser") String optionUser,
		@JspWizardController.RequestParameter(name = "optionIsDisabled") Boolean optionIsDisabled,
		@JspWizardController.RequestParameter(name = "optionSummaryPrefix") String optionSummaryPrefix,
		@JspWizardController.RequestParameter(name = "optionCategories") String optionCategories,
		@JspWizardController.RequestParameter(name = "optionYear") Integer optionYear,
		@JspWizardController.RequestParameter(name = "optionTimelineHeight") Integer optionTimelineHeight,
		@JspWizardController.RequestParameter(name = "optionAlarm") Boolean optionAlarm
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		PersistenceManager pm = this.getPm();
		RefObject_1_0 obj = this.getObject();
		this.isInitialized = isInitialized;
		this.anchorObjectXri = anchorObjectXri;
	    this.selectorType = selectorType != null ? SelectorType.valueOf(selectorType) : null;
		this.optionMax = optionMax == null ? 500 : optionMax;
		this.optionUser = optionUser == null ? app.getLoginPrincipal() : optionUser;	
		this.optionIsDisabled = optionIsDisabled == null ? false : optionIsDisabled;
		this.optionSummaryPrefix = optionSummaryPrefix == null ? "Birthdays" : optionSummaryPrefix;
		this.optionCategories = optionCategories == null ? "Birthday" : optionCategories;
		this.optionYear = optionYear == null ? 2012 : optionYear;
		this.optionTimelineHeight = optionTimelineHeight == null ? 500 : optionTimelineHeight;
		this.optionAlarm = optionAlarm == null ? false : optionAlarm;
		if(!Boolean.TRUE.equals(this.isInitialized)) {
			this.selectorType = SelectorType.CALENDARPROFILE;
		}
	    {
	    	NumberFormat formatter = new DecimalFormat("00000");	    	
		    this.urlBase = (this.getRequest().getRequestURL().toString()).substring(0, (this.getRequest().getRequestURL().toString()).indexOf(this.getRequest().getServletPath().toString()));
		    this.anchorObjectLabel = "Anchor object";
		    this.anchorObjects = new TreeMap<String,String>();
	    	if(this.selectorType == SelectorType.CALENDARPROFILE) {
		    	this.anchorObjectLabel = app.getLabel(CALENDARPROFILE_CLASS);
		        if(obj instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
		        	org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)obj;
		        	CalendarProfileQuery profileQuery = (CalendarProfileQuery)pm.newQuery(CalendarProfile.class);
		        	profileQuery.orderByName().ascending();
			        int index = 0;
			        for(CalendarProfile syncProfile: userHome.<CalendarProfile>getSyncProfile(profileQuery)) {
			            String display = (syncProfile.getName() != null ? syncProfile.getName() : "?");
			            String sortKey = formatter.format(index++) + ":" + display.toUpperCase();
			            this.anchorObjects.put(
			                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
			                syncProfile.refMofId()
			            );
			        }
		        }
		    } else if(this.selectorType == SelectorType.CARDPROFILE) {
		    	this.anchorObjectLabel = app.getLabel(CARDPROFILE_CLASS);
		        if(obj instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
		        	org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)obj;
		        	CardProfileQuery profileQuery = (CardProfileQuery)pm.newQuery(CardProfile.class);
		        	profileQuery.orderByName().ascending();
			        int index = 0;
			        for(CardProfile syncProfile: userHome.<CardProfile>getSyncProfile(profileQuery)) {
		                String display = (syncProfile.getName() != null ? syncProfile.getName() : "?");
		                String sortKey = formatter.format(index++) + ":" + display.toUpperCase();
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    syncProfile.refMofId()
		                );
			        }
		        }
		    } else if(this.selectorType == SelectorType.DOCUMENTPROFILE) {
		    	this.anchorObjectLabel = app.getLabel(DOCUMENTPROFILE_CLASS);
		        if(obj instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
		        	org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)obj;		        
		        	DocumentProfileQuery profileQuery = (DocumentProfileQuery)pm.newQuery(DocumentProfile.class);
		        	profileQuery.orderByName().ascending();
			        int index = 0;
			        for(DocumentProfile syncProfile: userHome.<DocumentProfile>getSyncProfile(profileQuery)) {
		                String display = (syncProfile.getName() != null ? syncProfile.getName() : "?");
		                String sortKey = formatter.format(index++) + ":" + display.toUpperCase();
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    syncProfile.refMofId()
		                );
			        }
		        }
		    }
	    }
		if(!Boolean.TRUE.equals(this.isInitialized)) {
			if(!this.anchorObjects.isEmpty()) {
				this.anchorObjectXri = this.anchorObjects.firstEntry().getValue();
			}
		}
	}

	/**
	 * @return the selectorType
	 */
	public SelectorType getSelectorType(
	) {
		return this.selectorType;
	}
	/**
	 * @return the optionMax
	 */
	public Integer getOptionMax(
	) {
		return this.optionMax;
	}

	/**
	 * @return the optionUser
	 */
	public String getOptionUser(
	) {
		return this.optionUser;
	}

	/**
	 * @return the optionIsDisabled
	 */
	public Boolean getOptionIsDisabled(
	) {
		return this.optionIsDisabled;
	}

	/**
	 * @return the optionSummaryPrefix
	 */
	public String getOptionSummaryPrefix(
	) {
		return this.optionSummaryPrefix;
	}

	/**
	 * @return the optionCategories
	 */
	public String getOptionCategories(
	) {
		return this.optionCategories;
	}

	/**
	 * @return the optionYear
	 */
	public Integer getOptionYear(
	) {
		return this.optionYear;
	}

	/**
	 * @return the optionTimelineHeight
	 */
	public Integer getOptionTimelineHeight(
	) {
		return this.optionTimelineHeight;
	}

	/**
	 * @return the optionAlarm
	 */
	public Boolean getOptionAlarm(
	) {
		return this.optionAlarm;
	}

	/**
	 * @return the anchorObjects
	 */
	public Map<String, String> getAnchorObjects(
	) {
		return this.anchorObjects;
	}
	
	/**
	 * @return the anchorObjectXri
	 */
	public String getAnchorObjectXri(
	) {
		return this.anchorObjectXri;
	}
	
	/**
	 * @return the anchorObjectLabel
	 */
	public String getAnchorObjectLabel(
	) {
		return this.anchorObjectLabel;
	}
	
	/**
	 * @return the urlBase
	 */
	public String getUrlBase(
	) {
		return this.urlBase;
	}

    /**
	 * @return the isInitialized
	 */
	public Boolean getIsInitialized(
	) {
		return this.isInitialized;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
    public static final String ACTIVITYTRACKER_CLASS = "org:opencrx:kernel:activity1:ActivityTracker";
    public static final String ACTIVITYCATEGORY_CLASS = "org:opencrx:kernel:activity1:ActivityCategory";
    public static final String ACTIVITYMILESTONE_CLASS = "org:opencrx:kernel:activity1:ActivityMilestone";
    public static final String ACTIVITYFILTERGROUP_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGroup";
    public static final String ACTIVITYFILTERGLOBAL_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGlobal";
    public static final String RESOURCE_CLASS = "org:opencrx:kernel:activity1:Resource";
    public static final String RESOURCEASSIGNMENT_CLASS = "org:opencrx:kernel:activity1:ResourceAssignment";
    public static final String USERHOME_CLASS = "org:opencrx:kernel:home1:UserHome";
    public static final String CALENDARPROFILE_CLASS = "org:opencrx:kernel:home1:CalendarProfile";
    public static final String ACCOUNTFILTERGLOBAL_CLASS = "org:opencrx:kernel:account1:AccountFilterGlobal";
    public static final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
    public static final String DOCUMENTPROFILE_CLASS = "org:opencrx:kernel:home1:DocumentProfile";
    public static final String CARDPROFILE_CLASS = "org:opencrx:kernel:home1:CardProfile";
    public static final String ABSTRACTPRICELEVEL_CLASS = "org:opencrx:kernel:product1:AbstractPriceLevel";

    public static final Integer MAX_ENTRY_SELECT = 200;

    public static final String HTML_COMMENT_BEGIN = "<!-- ";
    public static final String HTML_COMMENT_END = " -->";
    public static final String PROTOCOL_SPECIFIER_HTTP = "http:";
    public static final String PROTOCOL_SPECIFIER_HTTPS = "https:";
    public static final String UNKNOWN = "_?_";
    
    private Boolean isInitialized;
	private SelectorType selectorType;
	private String anchorObjectXri;
	private String anchorObjectLabel;
	private String urlBase;
	private Integer optionMax;
	private String optionUser;	
	private Boolean optionIsDisabled;
	private String optionSummaryPrefix;
	private String optionCategories;
	private Integer optionYear;
	private Integer optionTimelineHeight;
	private Boolean optionAlarm;
	private TreeMap<String,String> anchorObjects;
	
}
