/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Buildings
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.building1.jmi1.AbstractBuildingUnit;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;

/**
 * Buildings
 *
 */
public class Buildings extends AbstractImpl {

	/**
	 *  Register Buildings backend.
	 *  
	 */
	public static void register(
	) {
		registerImpl(new Buildings());
	}
	
	/**
	 * Get current Buildings backend.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Buildings getInstance(
	) throws ServiceException {
		return getInstance(Buildings.class);
	}

	/**
	 * Constructor.
	 * 
	 */
	protected Buildings(
	) {
		
	}
	
    /**
     * @return Returns the buildings segment.
     */
    public org.opencrx.kernel.building1.jmi1.Segment getBuildingSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.building1.jmi1.Segment) pm.getObjectById(
            new Path("xri://@openmdx*org.opencrx.kernel.building1").getDescendant("provider", providerName, "segment", segmentName)
        );
    }	
    
    /**
     * jdoPrestore() callback. Override for custom-specific behavior.
     * 
     * @param buildingUnit
     * @throws ServiceException
     */
    protected void updateAbstractBuildingUnit(
    	AbstractBuildingUnit buildingUnit
    ) throws ServiceException {
    }
    
    /**
     * jdoPreDelete() callback. Override for custom-specific behavior.
     * 
     * @param buildingUnit
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeAbstractBuildingUnit(
    	AbstractBuildingUnit buildingUnit,
    	boolean preDelete    	
    ) throws ServiceException {
    	
    }

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.backend.AbstractImpl#preDelete(org.opencrx.kernel.generic.jmi1.CrxObject, boolean)
	 */
	@Override
	public void preDelete(
		RefObject_1_0 object, 
		boolean preDelete
	) throws ServiceException {
		super.preDelete(object, preDelete);
		if(object instanceof AbstractBuildingUnit) {
			this.removeAbstractBuildingUnit((AbstractBuildingUnit)object, preDelete);
		}
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.backend.AbstractImpl#preStore(org.opencrx.kernel.generic.jmi1.CrxObject)
	 */
	@Override
	public void preStore(
		RefObject_1_0 object
	) throws ServiceException {
		super.preStore(object);
		if(object instanceof AbstractBuildingUnit) {
			this.updateAbstractBuildingUnit((AbstractBuildingUnit)object);
		}
	}
	
}
