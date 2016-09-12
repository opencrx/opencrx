/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: GridExportIncludingCompositesAsXmlAction
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.action;

import java.util.List;
import java.util.Map;

import org.opencrx.kernel.backend.Exporter;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.AggregationKind;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.component.UiGrid;

public class ExportIncludingCompositesAsXmlAction extends ExportObjectsAction {

	@Override
    protected GridExporter getGridExporter(
    	UiGrid grid, 
    	List<Path> selectedObjectIdentities,
    	int maxItems
    ) throws ServiceException {
		return new ModelBasedGridExporter(
			grid,
			selectedObjectIdentities,
			Exporter.MIME_TYPE_XML,
			this.getReferenceFilter(grid),
			maxItems
		);
    }

    protected String getReferenceFilter(
    	UiGrid grid
    ) throws ServiceException {
		Model_1_0 model = Model_1Factory.getModel();
	    String referenceFilter = grid.getReferenceName();
	    ModelElement_1_0 referencedType = null;
	    try {
	    	referencedType = model.getElement(model.getElement(grid.getQualifiedReferenceName()).getType());
	    } catch(Exception e) {}
	    if(referencedType != null) {
		    Map<String,ModelElement_1_0> featureDefs = model.getStructuralFeatureDefs(
		    	referencedType, 
		    	false, // includeSubtypes
		    	false, // includeDerived 
		    	false // attributesOnly
		    );
		    for(Map.Entry<String,ModelElement_1_0> eFeature: featureDefs.entrySet()) {
		    	ModelElement_1_0 featureDef = eFeature.getValue();
		    	if(model.isReferenceType(featureDef)) {
					ModelElement_1_0 referencedEnd = model.getElement(featureDef.getReferencedEnd());
					boolean referenceIsComposite = model.isReferenceType(featureDef) && AggregationKind.COMPOSITE.equals(referencedEnd.getAggregation());
			    	if(
			    		referenceIsComposite &&
			    		// ignore the following references
			    		!"audit".equals(eFeature.getKey()) && 
			    		!"indexEntry".equals(eFeature.getKey()) &&			    		
			    		!"media".equals(eFeature.getKey()) &&			    		
			    		!"propertySetEntry".equals(eFeature.getKey()) &&			    		
			    		!"accountMembership".equals(eFeature.getKey())			    		
			    	) {
			    		referenceFilter += "," + featureDef.getQualifiedName();
			    	}
		    	}
		    }
	    }
	    return referenceFilter;
    }

	public static final int EVENT_ID = 103;
		
}
