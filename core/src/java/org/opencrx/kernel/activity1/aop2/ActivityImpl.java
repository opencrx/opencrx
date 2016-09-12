/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ActivityImpl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2009, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.activity1.aop2;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.jdo.JDOUserException;
import javax.jdo.listener.DeleteCallback;
import javax.jdo.listener.StoreCallback;

import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityWorkRecord;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class ActivityImpl
	<S extends org.opencrx.kernel.activity1.jmi1.Activity,N extends org.opencrx.kernel.activity1.cci2.Activity,C extends ActivityImpl.DerivedAttributes>
	extends AbstractObject<S,N,C>
	implements StoreCallback, DeleteCallback {

    //-----------------------------------------------------------------------
	public static class DerivedAttributes {
		
		public DerivedAttributes(
			Object[] mainEffortEstimate
		) {
			this.mainEstimateEffortHours = (Integer)mainEffortEstimate[0];
			this.mainEstimateEffortMinutes = (Integer)mainEffortEstimate[1];
			this.mainEstimateEffortHhMm = (String)mainEffortEstimate[2];
		}
		
		public Integer mainEstimateEffortHours;
		public Integer mainEstimateEffortMinutes;
		public String mainEstimateEffortHhMm;
		
	}
	
    //-----------------------------------------------------------------------
    public ActivityImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void voteForActivity(
        org.opencrx.kernel.activity1.jmi1.ActivityVoteForActivityParams params
    ) {
        try {
            Activities.getInstance().voteForActivity(
                this.sameObject(),
                params.getName(),
                params.getDescription()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }
    
    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void assignTo(
        org.opencrx.kernel.activity1.jmi1.ActivityAssignToParams params
    ) {
        try {
            Activities.getInstance().assignTo(
                this.sameObject(), 
                params.getResource()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }
        
    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void reapplyActivityCreator(
        org.opencrx.kernel.activity1.jmi1.ReapplyActivityCreatorParams params
    ) {
        try {
        	ActivityCreator activityCreator = params.getActivityCreator() == null ? 
                this.sameObject().getLastAppliedCreator() : 
                	params.getActivityCreator();
            if(activityCreator != null) {
            	List<ActivityGroup> activityGroups = activityCreator.getActivityGroup();
	            Activities.getInstance().reapplyActivityCreator(
	                this.sameObject(),
	                activityCreator,
	                activityGroups
	            );
            }
            return super.newVoid();
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult doFollowUp(
        org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams params
    ) {
        try {
            ActivityFollowUp followUp = Activities.getInstance().doFollowUp(
                this.sameObject(), 
                params.getFollowUpTitle(),
                params.getFollowUpText(),
                params.getTransition(),
                params.getAssignTo(),
                params.getParentProcessInstance()
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult.Member.followUp, followUp)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                   
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult addWorkRecord(
        org.opencrx.kernel.activity1.jmi1.ActivityAddWorkRecordParams params
    ) {
        try {
        	Uom uomHour = null;
        	try {
        		uomHour = (Uom)this.sameManager().getObjectById(
        			new Path("xri:@openmdx:org.opencrx.kernel.uom1/provider/" + this.sameObject().refGetPath().get(2) + "/segment/Root/uom/hour")
        		);
        	}
        	catch(Exception e) {}    
        	List<PrincipalGroup> owningGroups = params.getOwningGroup();        	
            ActivityWorkRecord workRecord = Activities.getInstance().addWorkAndExpenseRecord(
                this.sameObject(),
                params.getResource(),
                params.getName(),
                params.getDescription(),
                params.getStartAt(),
                params.getEndAt(),
                new BigDecimal(params.getDurationHours() == null ? 0 : params.getDurationHours()).add(
                	new BigDecimal(params.getDurationMinutes() == null ? 0.0 : params.getDurationMinutes().doubleValue() / 60.0)
                ),
                uomHour,
                params.getRecordType(),
                (short)0,
                params.getDepotSelector(),
                params.getRate(),
                params.getRateCurrency(),
                params.isBillable(),
                Boolean.FALSE, // isReimbursable
                owningGroups
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.Member.workRecord, workRecord)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                                    
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult addExpenseRecord(
        org.opencrx.kernel.activity1.jmi1.ActivityAddExpenseRecordParams params
    ) {
        try {
        	List<PrincipalGroup> owningGroups = params.getOwningGroup();        	
            ActivityWorkRecord workRecord = Activities.getInstance().addWorkAndExpenseRecord(
                this.sameObject(),
                params.getResource(),
                params.getName(),
                params.getDescription(),
                params.getStartAt(),
                params.getEndAt(),
                params.getQuantity(),
                params.getQuantityUom(),
                params.getRecordType(),
                params.getPaymentType(),
                params.getDepotSelector(),
                params.getRate(),
                params.getRateCurrency(),
                params.isBillable(),
                params.isReimbursable(),
                owningGroups
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.Member.workRecord, workRecord)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                                    
    }
    
    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void updateIcal(
    ) {
        try {
            Activities.getInstance().updateIcal(
                this.sameObject()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                                            
    }

    //-------------------------------------------------------------------------
    public int getTotalVotes(
    ) {
    	return this.sameObject().getVote().size();
    }
        
    // ----------------------------------------------------------------------------
    public java.lang.Boolean isAllDayEvent(
    ) {
    	DateTimeFormat utcf = DateTimeFormat.BASIC_UTC_FORMAT;
    	String scheduledStart = this.sameObject().getScheduledStart() == null ?
    		"" : 
    			utcf.format(this.sameObject().getScheduledStart());
    	String scheduledEnd = this.sameObject().getScheduledEnd() == null ?
    		"" : 
    			utcf.format(this.sameObject().getScheduledEnd());
    	return scheduledStart.endsWith("T000000.000Z") && scheduledEnd.endsWith("T000000.000Z"); 
    }
    
    //-----------------------------------------------------------------------
    public org.openmdx.base.cci2.Void markAsAllDayEvent(
    	org.opencrx.kernel.activity1.jmi1.MarkAsAllDayEventParams params    	
    ) {
        try {
            Activities.getInstance().markAsAllDayEvent(
                this.sameObject(),
                params.getTimezone()
            );
            return this.newVoid();
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                                    
    }
        
    // ----------------------------------------------------------------------------
    public java.lang.String getMainEstimateEffortHhMm(
    ) {
    	return super.thisContext().mainEstimateEffortHhMm;
    }

    // ----------------------------------------------------------------------------
    public java.lang.Integer getMainEstimateEffortHours(
    ) {
    	return super.thisContext().mainEstimateEffortHours;
    }

    // ----------------------------------------------------------------------------
    public java.lang.Integer getMainEstimateEffortMinutes(
    ) {
    	return super.thisContext().mainEstimateEffortMinutes;
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult calcTotalQuantity(
        org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityParams params    	
    ) {
        try {
            List<BigDecimal> totalQuantities = new ArrayList<BigDecimal>();
            List<Uom> quantityUoms = new ArrayList<Uom>();  	        	
            Activities.getInstance().calcTotalQuantity(
                this.sameObject(),
                params.getRecordType(),
                params.getStartAt(),
                params.getEndAt(),
                totalQuantities,
                quantityUoms
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult.Member.quantityUom, quantityUoms),
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult.Member.totalQuantity, totalQuantities)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.LinkToAndFollowUpResult linkToAndFollowUp(
        org.opencrx.kernel.activity1.jmi1.LinkToAndFollowUpParams params
    ) {
        try {
            ActivityFollowUp followUp = Activities.getInstance().linkToAndFollowUp(
                this.sameObject(),
                params.getTransition(),
                params.getActivity(),
                params.getParentProcessInstance()
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.LinkToAndFollowUpResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.LinkToAndFollowUpResult.Member.followUp, followUp)
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }

	/* (non-Javadoc)
	 * @see org.openmdx.base.aop2.AbstractObject#jdoPreStore()
	 */
	@Override
    public void jdoPreStore(
    ) {
    	try {
    		Activities.getInstance().preStore(
    			this.sameObject() 
    		);
    		super.jdoPreStore();
    	} catch(ServiceException e) {
    		throw new JDOUserException(
    			"jdoPreStore failed",
    			e,
    			this.sameObject()
    		);
    	}
    }

    /* (non-Javadoc)
     * @see org.openmdx.base.aop2.AbstractObject#jdoPreDelete()
     */
    @Override
    public void jdoPreDelete(
    ) {
    	try {
    		Activities.getInstance().preDelete(
    			this.sameObject(), 
    			true
    		);
    		super.jdoPreDelete();
    	} catch(ServiceException e) {
    		throw new JDOUserException(
    			"jdoPreDelete failed",
    			e,
    			this.sameObject()
    		);
    	}
    }

	//-----------------------------------------------------------------------
	@SuppressWarnings("unchecked")
    @Override
    protected C newContext(
    ) {
		try {
			return (C)new DerivedAttributes(
		    	Activities.getInstance().calcMainEffortEstimate(
		    		this.sameObject()
		    	)
		    );
		}
		catch(ServiceException e) {
			throw new JDOUserException(
				"newContext failed",
				e,
				this.sameObject()
			);
		}
    }
        
}
