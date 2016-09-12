/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ImportPropertiesFromXlsController
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFDateUtil;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.base.cci2.PropertyQuery;
import org.opencrx.kernel.base.jmi1.BooleanProperty;
import org.opencrx.kernel.base.jmi1.DateProperty;
import org.opencrx.kernel.base.jmi1.DateTimeProperty;
import org.opencrx.kernel.base.jmi1.DecimalProperty;
import org.opencrx.kernel.base.jmi1.IntegerProperty;
import org.opencrx.kernel.base.jmi1.Property;
import org.opencrx.kernel.base.jmi1.ReferenceProperty;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.base.jmi1.UriProperty;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.generic.cci2.PropertySetQuery;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.PropertySet;
import org.opencrx.kernel.product1.cci2.ProductConfigurationTypeQuery;
import org.opencrx.kernel.product1.cci2.ProductConfigurationTypeSetQuery;
import org.opencrx.kernel.product1.jmi1.ProductConfigurationType;
import org.opencrx.kernel.product1.jmi1.ProductConfigurationTypeSet;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.action.SelectObjectAction;

/**
 * ImportPropertiesFromXlsController
 *
 */
public class ImportPropertiesFromXlsController extends AbstractWizardController {

	/**
	 * ImportTarget
	 *
	 */
	public enum ImportTarget {
		NA,
		ProductSegment,
		ProductConfigurationTypeSet,
		ProductConfigurationType,
		PropertySet,
		CrxObject		
	}

	/**
	 * Constructor.
	 * 
	 * @param requiresAdminRole
	 */
	public ImportPropertiesFromXlsController(
		boolean requiresAdminRole
	) {
		super();
		this.requiresAdminRole = requiresAdminRole;
	}

    /**
     * Get UID as string.
     * 
     * @return
     * @throws ServiceException
     */
    public String getUidAsString(
    ) throws ServiceException {
    	return org.opencrx.kernel.backend.Base.getInstance().getUidAsString();
    }

    /**
     * Get href of select object action.
     * 
     * @param object
     * @return
     */
    public String getSelectObjectHref(
        RefObject_1_0 object
    ) {
        if (object != null) {
            Action parentAction = new Action(
                SelectObjectAction.EVENT_ID,
                new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, object.refMofId())
                },
                "",
                true // enabled
            );
            return parentAction.getEncodedHRef();
        } else {
        	return "";
        }
    }

    /**
     * Return true if property type is supported.
     * 
     * @param propertyType
     * @param propertyValue
     * @return
     */
    public boolean isSupportedDtypeValue(
    	String propertyType,
    	HSSFCell propertyValue
    ) {
    	return (
    		propertyType != null &&
    		(
    			propertyValue == null ||
    			(propertyType.equals(PROPERTY_DTYPE_STRING) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_STRING) ||
    			(propertyType.equals(PROPERTY_DTYPE_DECIMAL) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_NUMERIC && !HSSFDateUtil.isCellDateFormatted(propertyValue)) ||
    			(propertyType.equals(PROPERTY_DTYPE_INTEGER) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_NUMERIC && !HSSFDateUtil.isCellDateFormatted(propertyValue)) ||
    			(propertyType.equals(PROPERTY_DTYPE_BOOLEAN) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_BOOLEAN) ||
    			(propertyType.equals(PROPERTY_DTYPE_DATETIME) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_NUMERIC && HSSFDateUtil.isCellDateFormatted(propertyValue)) ||
    			(propertyType.equals(PROPERTY_DTYPE_DATE) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_NUMERIC && HSSFDateUtil.isCellDateFormatted(propertyValue)) ||
    			(propertyType.equals(PROPERTY_DTYPE_REFERENCE) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_STRING) ||
    			(propertyType.equals(PROPERTY_DTYPE_URI) && propertyValue.getCellType() == HSSFCell.CELL_TYPE_STRING)
    			)
    		);
    }

    /**
     * Update product configuration type.
     * 
     * @param productConfigurationType
     * @param valueMap
     * @return
     */
    public boolean updateProductConfigurationType(
        ProductConfigurationType productConfigurationType,
        Map<String,Cell> valueMap
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(productConfigurationType);
        Cell cell = null;
        boolean updated = false;
        try {
            // validFrom
            pm.currentTransaction().begin();
            if (valueMap.get(ATTR_PRODUCTCONFIGURATIONTYPE_VALIDFROM) != null) {
                cell = valueMap.get(ATTR_PRODUCTCONFIGURATIONTYPE_VALIDFROM);
                if (cell.getCellType() == HSSFCell.CELL_TYPE_NUMERIC && HSSFDateUtil.isCellDateFormatted(cell)) {
                    productConfigurationType.setValidFrom(HSSFDateUtil.getJavaDate(cell.getNumericCellValue()));
                    updated = true;
                }
            }
            pm.currentTransaction().commit();
        } catch (Exception e) {
            new ServiceException(e).log();
            try {
                pm.currentTransaction().rollback();
            } catch(Exception e1) {}
        }
        try {
            // validTo
            pm.currentTransaction().begin();
            if (valueMap.get(ATTR_PRODUCTCONFIGURATIONTYPE_VALIDTO) != null) {
                cell = valueMap.get(ATTR_PRODUCTCONFIGURATIONTYPE_VALIDTO);
                if (cell.getCellType() == HSSFCell.CELL_TYPE_NUMERIC && HSSFDateUtil.isCellDateFormatted(cell)) {
                    productConfigurationType.setValidTo(HSSFDateUtil.getJavaDate(cell.getNumericCellValue()));
                    updated = true;
                }
            }
            pm.currentTransaction().commit();
        } catch (Exception e) {
            new ServiceException(e).log();
            try {
                pm.currentTransaction().rollback();
            } catch(Exception e1) {}
        }
        try {
            // isDefault
            pm.currentTransaction().begin();
            if (valueMap.get(ATTR_PRODUCTCONFIGURATIONTYPE_ISDEFAULT) != null) {
                cell = valueMap.get(ATTR_PRODUCTCONFIGURATIONTYPE_ISDEFAULT);
                if (cell.getCellType() == HSSFCell.CELL_TYPE_BOOLEAN) {
                    productConfigurationType.setDefault(new Boolean(cell.getBooleanCellValue()));
                    updated = true;
                }
            }
            pm.currentTransaction().commit();
        } catch (Exception e) {
            new ServiceException(e).log();
            try {
                pm.currentTransaction().rollback();
            } catch(Exception e1) {}
        }

        return updated;
    }

    /**
     * Create or update property.
     * 
     * @param prodConfTypeSet
     * @param productConfigurationTypeSetName
     * @param productConfigurationTypeSetDescription
     * @param prodConfType
     * @param productConfigurationTypeName
     * @param productConfigurationTypeDescription
     * @param crxObject
     * @param propSet
     * @param propertySetName
     * @param propertySetDescription
     * @param propertyType
     * @param propertyName
     * @param propertyDescription
     * @param propertyValue
     * @param productSegment
     * @return
     */
    public Property createOrUpdatePropertyOfPropertySet(
        ProductConfigurationTypeSet prodConfTypeSet,
        String productConfigurationTypeSetName,
        String productConfigurationTypeSetDescription,
        ProductConfigurationType prodConfType,
        String productConfigurationTypeName,
        String productConfigurationTypeDescription,
        CrxObject crxObject,
        PropertySet propSet,
        String propertySetName,
        String propertySetDescription,
        String propertyType,
        String propertyName,
        String propertyDescription,
        HSSFCell propertyValue,
        org.opencrx.kernel.product1.jmi1.Segment productSegment,
        ApplicationContext app
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(productSegment);
        ProductConfigurationTypeSet productConfigurationTypeSet = prodConfTypeSet;
        ProductConfigurationType productConfigurationType = prodConfType;
        PropertySet propertySet = propSet;
        Property property = null;
        if (
            prodConfTypeSet != null || productConfigurationTypeSetName != null ||
            prodConfType != null || productConfigurationTypeName != null
        ) {
            if (
                productConfigurationTypeSet == null &&
                productConfigurationTypeSetName != null && !productConfigurationTypeSetName.isEmpty()
            ) {
                // try to locate productConfigurationTypeSet with respective name (or create new productConfigurationTypeSet)
                ProductConfigurationTypeSetQuery productConfigurationTypeSetQuery = (ProductConfigurationTypeSetQuery)pm.newQuery(ProductConfigurationTypeSet.class);
                productConfigurationTypeSetQuery.name().equalTo(productConfigurationTypeSetName);
                try {
                    pm.currentTransaction().begin();
                    Iterator<ProductConfigurationTypeSet> pcts = productSegment.getConfigurationTypeSet(productConfigurationTypeSetQuery).iterator();
                    if (pcts.hasNext()) {
                        productConfigurationTypeSet = pcts.next();
                    } else {
                        // create new ProductConfigurationTypeSet
                        productConfigurationTypeSet = pm.newInstance(ProductConfigurationTypeSet.class);
                        productConfigurationTypeSet.setName(productConfigurationTypeSetName);
                        productSegment.addConfigurationTypeSet(
                            getUidAsString(),
                            productConfigurationTypeSet
                        );
                    }
                    productConfigurationTypeSet.setDescription(productConfigurationTypeSetDescription);
                    pm.currentTransaction().commit();
                    //System.out.println("productConfigurationTypeSet found/committed name=" + productConfigurationTypeSet.getName());
                } catch (Exception e) {
                    new ServiceException(e).log();
                    try {
                        pm.currentTransaction().rollback();
                    } catch(Exception e1) {}
                }
            }
            if (
                productConfigurationTypeSet != null &&
                productConfigurationType == null &&
                productConfigurationTypeName != null && !productConfigurationTypeName.isEmpty()
            ) {
                // try to locate productConfigurationType with respective name (or create new productConfigurationType)
                ProductConfigurationTypeQuery productConfigurationTypeFilter = (ProductConfigurationTypeQuery)pm.newQuery(ProductConfigurationType.class);
                productConfigurationTypeFilter.name().equalTo(productConfigurationTypeName);
                try {
                    pm.currentTransaction().begin();
                    Iterator<ProductConfigurationType> pct = productConfigurationTypeSet.getConfigurationType(productConfigurationTypeFilter).iterator();
                    if (pct.hasNext()) {
                        productConfigurationType = (ProductConfigurationType)pct.next();
                    } else {
                        // create new ProductConfigurationType
                        productConfigurationType = pm.newInstance(ProductConfigurationType.class);
                        productConfigurationType.setName(productConfigurationTypeName);
                        productConfigurationTypeSet.addConfigurationType(
                            getUidAsString(),
                            productConfigurationType
                        );
                    }
                    productConfigurationType.setDescription(productConfigurationTypeDescription);
                    pm.currentTransaction().commit();
                    //System.out.println("productConfigurationType found/committed name=" + productConfigurationTypeSet.getName());
                } catch (Exception e) {
                    new ServiceException(e).log();
                    try {
                        pm.currentTransaction().rollback();
                    } catch(Exception e1) {}
                }
            }
        } else if (crxObject != null) {
            // try to locate PropertySet with same parent and name (or create new PropertySet)
            PropertySetQuery propertySetFilter = (PropertySetQuery)pm.newQuery(PropertySet.class);
            propertySetFilter.name().equalTo(propertySetName);
            try {
                pm.currentTransaction().begin();
                Iterator<PropertySet> ps = crxObject.getPropertySet(propertySetFilter).iterator();
                if (ps.hasNext()) {
                    propertySet = (PropertySet)ps.next();
                } else {
                    // create new PropertySet
                    propertySet = pm.newInstance(PropertySet.class);
                    propertySet.setName(propertySetName);
                    crxObject.addPropertySet(
                        getUidAsString(),
                        propertySet
                    );
                }
                propertySet.setDescription(propertySetDescription);
                pm.currentTransaction().commit();
            } catch (Exception e) {
                new ServiceException(e).log();
                try {
                    pm.currentTransaction().rollback();
                } catch(Exception e1) {}
            }
        }
        if (
            (propertySet != null || productConfigurationType != null) &&
            propertyType != null && !propertyType.isEmpty() &&
            propertyName != null && !propertyName.isEmpty()
        ) {
            // try to locate property with same parent and name (or create new property)
            PropertyQuery propertyQuery = (PropertyQuery)pm.newQuery(Property.class);
            propertyQuery.name().equalTo(propertyName);
            Iterator<Property> p = null;
            if (productConfigurationType != null) {
                p = productConfigurationType.getProperty(propertyQuery).iterator();
            } else {
                p = propertySet.getProperty(propertyQuery).iterator();
            }
            try {
                while(p.hasNext() && property == null) {
                    property = p.next();
                    if (!(
                        (property instanceof StringProperty) && (propertyType.equals(PROPERTY_DTYPE_STRING)) ||
                        (property instanceof DecimalProperty) && (propertyType.equals(PROPERTY_DTYPE_DECIMAL)) ||
                        (property instanceof IntegerProperty) && (propertyType.equals(PROPERTY_DTYPE_INTEGER)) ||
                        (property instanceof BooleanProperty) && (propertyType.equals(PROPERTY_DTYPE_BOOLEAN)) ||
                        (property instanceof DateTimeProperty) && (propertyType.equals(PROPERTY_DTYPE_DATETIME)) ||
                        (property instanceof DateProperty) && (propertyType.equals(PROPERTY_DTYPE_DATE)) ||
                        (property instanceof ReferenceProperty) && (propertyType.equals(PROPERTY_DTYPE_REFERENCE)) ||
                        (property instanceof UriProperty) && (propertyType.equals(PROPERTY_DTYPE_URI))
                    )) {
                        property = null;
                    }
                }
                pm.currentTransaction().begin();
                if (propertyType.equals(PROPERTY_DTYPE_STRING)) {
                    if (property == null) {
                        // create new StringProperty
                        property = pm.newInstance(StringProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        ((StringProperty)property).setStringValue(propertyValue != null ? propertyValue.getStringCellValue().trim() : null);
                    }
                } else if (propertyType.equals(PROPERTY_DTYPE_DECIMAL)) {
                    if (property == null) {
                        // create new DecimalProperty
                        property = pm.newInstance(DecimalProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        ((DecimalProperty)property).setDecimalValue(propertyValue != null ? new BigDecimal(propertyValue.getNumericCellValue()) : null);
                    }
                } else if (propertyType.equals(PROPERTY_DTYPE_INTEGER)) {
                    if (property == null) {
                        // create new IntegerProperty
                        property = pm.newInstance(IntegerProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        ((IntegerProperty)property).setIntegerValue(propertyValue != null ? (new BigDecimal(propertyValue.getNumericCellValue())).intValue() : null);
                    }
                } else if (propertyType.equals(PROPERTY_DTYPE_BOOLEAN)) {
                    if (property == null) {
                        // create new BooleanProperty
                        property = pm.newInstance(BooleanProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        ((BooleanProperty)property).setBooleanValue(propertyValue != null ? propertyValue.getBooleanCellValue() : null);
                    }
                } else if (propertyType.equals(PROPERTY_DTYPE_DATETIME)) {
                    if (property == null) {
                        // create new DateTimeProperty
                        property = pm.newInstance(DateTimeProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        ((DateTimeProperty)property).setDateTimeValue(propertyValue != null ? HSSFDateUtil.getJavaDate(propertyValue.getNumericCellValue()) : null);
                    }
                } else if (propertyType.equals(PROPERTY_DTYPE_DATE)) {
                    if (property == null) {
                        // create new DateTimeProperty
                        property = pm.newInstance(DateProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        if (propertyValue != null) {
                            TimeZone timezone = TimeZone.getTimeZone(app.getCurrentTimeZone());
                            SimpleDateFormat dateonlyf = new SimpleDateFormat("yyyyMMdd", app.getCurrentLocale()); dateonlyf.setTimeZone(timezone);
                            String date =
    				        	dateonlyf.format(HSSFDateUtil.getJavaDate(propertyValue.getNumericCellValue())).substring(0, 8);
                            XMLGregorianCalendar cal = org.w3c.spi2.Datatypes.create(
                                XMLGregorianCalendar.class,
                                date
                            );
                            ((DateProperty)property).setDateValue(cal);
                        } else {
                            ((DateProperty)property).setDateValue(null);
                        }
                    }
                } else if (propertyType.equals(PROPERTY_DTYPE_REFERENCE)) {
                    if (property == null) {
                        // create new ReferenceProperty
                        property = pm.newInstance(ReferenceProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        BasicObject basicObject = null;
                        if (propertyValue != null) {
                            try {
                               String xri = propertyValue.getStringCellValue().trim();
                               basicObject = (BasicObject)pm.getObjectById(new Path(xri));
                            } catch (Exception e) {}
                        }
                        ((ReferenceProperty)property).setReferenceValue(basicObject);
                    }
                } else if (propertyType.equals(PROPERTY_DTYPE_URI)) {
                    if (property == null) {
                        // create new UriProperty
                        property = pm.newInstance(UriProperty.class);
                        property.setName(propertyName);
                        if (productConfigurationType != null) {
                            productConfigurationType.addProperty(
                                getUidAsString(),
                                property
                            );
                        } else {
                            propertySet.addProperty(
                                getUidAsString(),
                                property
                            );
                        }
                    }
                    if (property != null) {
                        property.setDescription(propertyDescription);
                        ((UriProperty)property).setUriValue(propertyValue != null ? propertyValue.getStringCellValue().trim() : null);
                    }
                }
                pm.currentTransaction().commit();
            } catch (Exception e) {
                new ServiceException(e).log();
                try {
                    pm.currentTransaction().rollback();
                } catch(Exception e1) {}
            }
        }
        return property;
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

	/**
	 * Refresh action.
	 * 
	 */
	public void doRefresh(
	) {
		ApplicationContext app = this.getApp();
		if(this.requiresAdminRole) {
			String currentUserRole = app.getCurrentUserRole();
			String adminRole = SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + this.getSegmentName() + "@" + this.getSegmentName();
			this.hasPermission = currentUserRole.equals(adminRole);
		} else {
		   this.hasPermission = true;
		}
		this.importReport = "";			
	}

	/**
	 * OK action.
	 * 
	 * @throws ServiceException
	 */
	public void doOK(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.doRefresh();
		if(!this.hasPermission) {
			this.errorMessage = "no permission to run this wizard";
		} else {
			String location = app.getTempFileName(UPLOAD_FILE_FIELD_NAME, "");
			try {
				if(
					new File(location + ".INFO").exists() &&
					new File(location).exists() &&
					(new File(location).length() > 0)
				) {
					String contentMimeType = null;
					String contentName = null;
					try {
						// mimeType and name
						BufferedReader r = new BufferedReader(
							new FileReader(location + ".INFO")
						);
						contentMimeType = r.readLine();
						contentName = r.readLine();
						r.close();
						new File(location + ".INFO").delete();
					} catch(Exception ignore) {}
					if(
						(contentName != null) &&
						!contentName.isEmpty() &&
						(contentMimeType != null) &&
						!contentMimeType.isEmpty()
					) {
						// the calling object determines which columns are optional/required in the spreadsheet
						ProductConfigurationTypeSet productConfigurationTypeSet = null;
						ProductConfigurationType productConfigurationType = null;
						PropertySet propertySet = null;
						CrxObject crxObject = null;
						String callerName = null;
						String callerParentName = null;
						ImportTarget importTarget = ImportTarget.NA;
						// case 1:
						// required: Property_name
						//           ProductConfigurationTypeSet_name
						//           ProductConfigurationType_name
						if (this.getObject() instanceof org.opencrx.kernel.product1.jmi1.Segment) {
							importTarget = ImportTarget.ProductSegment;
						} 
						// case 2:
						// required: Property_name
						//           ProductConfigurationType_name
						// optional: ProductConfigurationTypeSet_name (if provided, then only Properties of matching ProductConfigurationTypeSets are considered)
						else if (this.getObject() instanceof ProductConfigurationTypeSet) {
							importTarget = ImportTarget.ProductConfigurationTypeSet;
							productConfigurationTypeSet = (ProductConfigurationTypeSet)this.getObject();
							callerName = ((ProductConfigurationTypeSet)this.getObject()).getName();
						} 
						// case 3:
						// required: Property_name
						// optional: ProductConfigurationTypeSet_name (if provided, then only Properties of matching ProductConfigurationTypeSets are considered)
						//           ProductConfigurationType_name (if provided, then only Properties of matching ProductConfigurationTypes are considered)
						else if (this.getObject() instanceof ProductConfigurationType) {
							importTarget = ImportTarget.ProductConfigurationType;
							productConfigurationType = (ProductConfigurationType)this.getObject();
							callerName = ((ProductConfigurationType)this.getObject()).getName();
							RefObject_1_0 parentObj = (RefObject_1_0)pm.getObjectById(this.getObject().refGetPath().getParent().getParent());
							if (parentObj instanceof ProductConfigurationTypeSet) {
								callerParentName = ((ProductConfigurationTypeSet)parentObj).getName();
							}
						} 
						// case 4:
						// required: Property_name
						// optional: PropertySet_name (if provided, then only Properties of matching PropertySets are considered)
						else if(this.getObject() instanceof PropertySet) {
							importTarget = ImportTarget.PropertySet;
							propertySet = (PropertySet)this.getObject();
							callerName = propertySet.getName();
						} 
						// case 5:
						// required: PropertySet_name
						//           Property_name
						else if (this.getObject() instanceof CrxObject) {
							importTarget = ImportTarget.CrxObject;
							crxObject = (CrxObject)this.getObject();
						}
						// Get product segment
						org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(pm, this.getProviderName(), this.getSegmentName());
						int idxProperty_dtype = -1;
						int idxProperty_name = -1;
						int idxProperty_description = -1;
						int idxProperty_value = -1;
						int idxPropertySet_name = -1;
						int idxPropertySet_description = -1;
						int idxProductConfigurationTypeSet_name = -1;
						int idxProductConfigurationTypeSet_description = -1;
						int idxProductConfigurationType_name = -1;
						int idxProductConfigurationType_description = -1;

						// verify whether File exists
						// Read workbook
						Workbook wb = null;
						try {
							wb = WorkbookFactory.create(new FileInputStream(location));
						} catch (Exception e) {
							this.errorMessage = e.getMessage();
						}
						if(wb != null) {						
							//for (int i = 0; i < workbook.getNumberOfSheets(); i++) {
								// read first sheet only!!!
							for (int i = 0; i < 1; i++) {
								Sheet sheet = wb.getSheetAt(i);
								int linesRead = 0;
								int propertiesUpdated = 0;
								this.importReport += "";
								Iterator<Row> rows = sheet.rowIterator();
								int nRow = 0;
								int maxCell = 0;
								HSSFRow row = null;
								Map<String,String> attributeMap = new TreeMap<String,String>();
								if (rows.hasNext()) {
									nRow += 1;
									// read first row with attribute names
									this.importReport += "<tr class='gridTableHeaderFull'>";
									this.importReport += "<td>#</td>";
									row = (HSSFRow) rows.next();
									Iterator<Cell> cells = row.cellIterator();
									int nCell = 0;
									while (cells.hasNext()) {
										HSSFCell cell = (HSSFCell)cells.next();
										nCell = cell.getColumnIndex();
										if (nCell > maxCell) {
											maxCell = nCell;
										}
										try {
											if (
												(cell.getCellType() == HSSFCell.CELL_TYPE_STRING) &&
												(cell.getStringCellValue() != null)
											) {
												boolean isSearchAttribute = false;
												String cellValue = (cell.getStringCellValue().trim());
												attributeMap.put(DECIMAL_FORMAT_0000.format(nCell), cellValue);
												// get idx of select attributes
												if (ATTR_PROPERTY_DTYPE.compareToIgnoreCase(cellValue) == 0) {
													idxProperty_dtype = nCell;
												} else if (ATTR_PROPERTY_NAME.compareToIgnoreCase(cellValue) == 0) {
													idxProperty_name = nCell;
													isSearchAttribute = true;
												} else if (ATTR_PROPERTY_DESCRIPTION.compareToIgnoreCase(cellValue) == 0) {
													idxProperty_description = nCell;
												} else if (ATTR_PROPERTY_VALUE.compareToIgnoreCase(cellValue) == 0) {
													idxProperty_value = nCell;
												} else if (ATTR_PROPERTYSET_NAME.compareToIgnoreCase(cellValue) == 0) {
													idxPropertySet_name = nCell;
													isSearchAttribute = true;
												} else if (ATTR_PROPERTYSET_DESCRIPTION.compareToIgnoreCase(cellValue) == 0) {
													idxPropertySet_description = nCell;
												} else if (ATTR_PRODUCTCONFIGURATIONTYPESET_NAME.compareToIgnoreCase(cellValue) == 0) {
													idxProductConfigurationTypeSet_name = nCell;
													isSearchAttribute = true;
												} else if (ATTR_PRODUCTCONFIGURATIONTYPESET_DESCRIPTION.compareToIgnoreCase(cellValue) == 0) {
													idxProductConfigurationTypeSet_description = nCell;
												} else if (ATTR_PRODUCTCONFIGURATIONTYPE_NAME.compareToIgnoreCase(cellValue) == 0) {
													idxProductConfigurationType_name = nCell;
													isSearchAttribute = true;
												} else if (ATTR_PRODUCTCONFIGURATIONTYPE_DESCRIPTION.compareToIgnoreCase(cellValue) == 0) {
													idxProductConfigurationType_description = nCell;
												}
												this.importReport += "<td " + (isSearchAttribute ? "class='searchAttr' title='attribute used for matching'" : "") + ">" + cellValue + "</td>";
											} else {
												this.importReport += "<td class='err'>c" + DECIMAL_FORMAT_0000.format(nCell) + "[not a string cell]<br>" + cell.getCellFormula() + "</td>";
											}
										} catch (Exception ec) {
											this.importReport += "<td class='err'>c" + DECIMAL_FORMAT_0000.format(nCell) + " [UNKNOWN ERROR]<br>" + ec.getMessage() + "</td>";
										}
									}
									this.importReport += "</tr>";
								}
								while (rows.hasNext()) {
									nRow += 1;
									linesRead += 1;
									row = (HSSFRow)rows.next();
									String propertyType = null;
									String propertyName = null;
									String propertyDescription = null;
									HSSFCell propertyValue = null;
									String propertySetName = null;
									String propertySetDescription = null;
									String productConfigurationTypeSetName = null;
									String productConfigurationTypeSetDescription = null;
									String productConfigurationTypeName = null;
									String productConfigurationTypeDescription = null;
									String cellId = null;
									Map<String,Cell> valueMap = new TreeMap<String,Cell>(String.CASE_INSENSITIVE_ORDER);
									String appendErrorRow = null;
									this.importReport += "<tr class='gridTableRowFull'>";
									this.importReport += "<td id='r" + nRow + "'><b>" + DECIMAL_FORMAT_0000.format(nRow) + "</b></td>";
									String jsBuffer = "";
									try {
										Iterator<Cell> cells = row.cellIterator();
										int nCell = 0;
										int currentCell = 0;
										appendErrorRow = null;
										while (cells.hasNext()) {
											//HSSFCell cell = (HSSFCell)row.getCell((short)0);
											HSSFCell cell = (HSSFCell)cells.next();
											nCell = cell.getColumnIndex();
											if (nCell > currentCell) {
												this.importReport += "<td colspan=\"" + (nCell-currentCell) + "\" class=\"empty\">&nbsp;</td>";
											}
											currentCell = nCell+1;
											try {
												cellId =  "id='r" + nRow + (attributeMap.get(DECIMAL_FORMAT_0000.format(nCell))).toString().toUpperCase() + "'";
												if (cell.getCellType() == HSSFCell.CELL_TYPE_STRING) {
													String cellValue = cell.getStringCellValue().trim();
													valueMap.put(
														(attributeMap.get(DECIMAL_FORMAT_0000.format(nCell))).toString(), 
														cell
													);
													if (nCell == idxProperty_dtype) {
														propertyType = cellValue;
													} else if (nCell == idxProperty_name) {
														propertyName = cellValue;
													} else if (nCell == idxProperty_description) {
														propertyDescription = cellValue;
													} else if (nCell == idxProperty_value) {
														propertyValue = cell;
													} else if (nCell == idxPropertySet_name) {
														propertySetName = cellValue;
													} else if (nCell == idxPropertySet_description) {
														propertySetDescription = cellValue;
													} else if (nCell == idxProductConfigurationTypeSet_name) {
														productConfigurationTypeSetName = cellValue;
													} else if (nCell == idxProductConfigurationTypeSet_description) {
														productConfigurationTypeSetDescription = cellValue;
													} else if (nCell == idxProductConfigurationType_name) {
														productConfigurationTypeName = cellValue;
													} else if (nCell == idxProductConfigurationType_description) {
														productConfigurationTypeDescription = cellValue;
													}
													this.importReport += "<td " + cellId + ">" + (cellValue != null ? (cellValue.replace("\r\n", EOL_HTML)).replace("\n", EOL_HTML) : "") + "</td>";
												} else if (cell.getCellType() == HSSFCell.CELL_TYPE_NUMERIC) {
													if (nCell == idxProperty_value) {
														propertyValue = cell;
													}
													BigDecimal cellValue = new BigDecimal(cell.getNumericCellValue());
													valueMap.put(
														(attributeMap.get(DECIMAL_FORMAT_0000.format(nCell))).toString(), 
														cell
													);
													this.importReport += "<td " + cellId + ">" + cellValue + "</td>";
												} else if (cell.getCellType() == HSSFCell.CELL_TYPE_BOOLEAN) {
													if (nCell == idxProperty_value) {
														propertyValue = cell;
													}
													boolean cellValue = cell.getBooleanCellValue();
													valueMap.put(
														(attributeMap.get(DECIMAL_FORMAT_0000.format(nCell))).toString(), 
														cell
													);
													this.importReport += "<td " + cellId + ">" + (cellValue ? "TRUE" : "FALSE") + "</td>";
												} else if (cell.getCellType() == HSSFCell.CELL_TYPE_BLANK) {
													valueMap.put(
														(attributeMap.get(DECIMAL_FORMAT_0000.format(nCell))).toString(), 
														cell
													);
													this.importReport += "<td " + cellId + " class=\"empty\">&nbsp;</td>";
												} else {
													this.importReport += "<td class=\"err\">r" + DECIMAL_FORMAT_0000.format(nRow) + "-c" + DECIMAL_FORMAT_0000.format(nCell) + "[cell-type (" + cell.getCellType() + ") not supported]<br>" + cell.getCellFormula() + "</td>";
												}
											} catch (Exception ec) {
												this.importReport += "<td class=\"err\">r" + DECIMAL_FORMAT_0000.format(nRow) + "-c" + DECIMAL_FORMAT_0000.format(nCell) + " [UNKNOWN ERROR]<br>" + cell.getCellFormula() + "</td>";
											}
										}
										if (nCell < maxCell) {
											this.importReport += "<td colspan=\"" + (maxCell-nCell) + "\" class=\"empty\"></td>";
										}
									} catch (Exception e) {
										this.importReport += "<td class='err' colspan=\"" + (maxCell+2) + "\">ERROR in Attribute Row!</td>";
									}
									// process row
									Property property = null;
									if (this.isSupportedDtypeValue(propertyType, propertyValue)) {
										/* case 1 */                                  
										if (
											importTarget == ImportTarget.ProductSegment &&
											propertyName != null && !propertyName.isEmpty() &&
											productConfigurationTypeSetName != null && !productConfigurationTypeSetName.isEmpty() &&
											productConfigurationTypeName != null && !productConfigurationTypeName.isEmpty()
										) {
											jsBuffer += "$('r" + nRow + "').title += 'Property Of ProductConfigurationTypeSet (called from Product Segment)';";
											if (propertySetName == null || propertySetName.isEmpty()) {
												property = this.createOrUpdatePropertyOfPropertySet(
													productConfigurationTypeSet,
													productConfigurationTypeSetName,
													productConfigurationTypeSetDescription,
													productConfigurationType,
													productConfigurationTypeName,
													productConfigurationTypeDescription,
													null,
													null,
													null,
													null,
													propertyType,
													propertyName,
													propertyDescription,
													propertyValue,
													productSegment,
													app
												);
												if (property != null) {
													this.updateProductConfigurationType(
														(org.opencrx.kernel.product1.jmi1.ProductConfigurationType)pm.getObjectById(new Path(property.refMofId()).getParent().getParent()),
														valueMap
													);
												}
											} else {
												jsBuffer += "$('r" + nRow + "').title += ' - verify data row';";
											}
											/* case 2 */                                  
											} else if (
												importTarget == ImportTarget.ProductConfigurationTypeSet &&
												propertyName != null && !propertyName.isEmpty() &&
												productConfigurationTypeName != null && !productConfigurationTypeName.isEmpty()
											) {
												jsBuffer += "$('r" + nRow + "').title += 'Property Of ProductConfigurationTypeSet (called from ProductConfigurationTypeSet)';";
												if (
													((productConfigurationTypeSetName == null || productConfigurationTypeSetName.isEmpty()) ||
													(callerName != null && productConfigurationTypeSetName != null && callerName.equals(productConfigurationTypeSetName))) &&
													(propertySetName == null || propertySetName.isEmpty())
												) {
													property = this.createOrUpdatePropertyOfPropertySet(
														productConfigurationTypeSet,
														productConfigurationTypeSetName,
														productConfigurationTypeSetDescription,
														productConfigurationType,
														productConfigurationTypeName,
														productConfigurationTypeDescription,
														null,
														null,
														null,
														null,
														propertyType,
														propertyName,
														propertyDescription,
														propertyValue,
														productSegment,
														app
													);
													if (property != null) {
														this.updateProductConfigurationType(
															(ProductConfigurationType)pm.getObjectById(new Path(property.refMofId()).getParent().getParent()),
															valueMap
														);
													}
												} else {
													jsBuffer += "$('r" + nRow + "').title += ' - verify data row';";
												}
												/* case 3 */                                  
												} else if (
													importTarget == ImportTarget.ProductConfigurationType &&
													propertyName != null && !propertyName.isEmpty()
												) {
													jsBuffer += "$('r" + nRow + "').title += 'Property Of ProductConfigurationTypeSet (called from ProductConfigurationType)';";
													if (
														((productConfigurationTypeSetName == null || productConfigurationTypeSetName.isEmpty()) ||
														(callerParentName != null && productConfigurationTypeSetName != null && callerParentName.equals(productConfigurationTypeSetName))) &&
														((productConfigurationTypeName == null || productConfigurationTypeName.isEmpty()) ||
														(callerName != null && productConfigurationTypeName != null && callerName.equals(productConfigurationTypeName))) &&
														(propertySetName == null || propertySetName.isEmpty())
													) {
														property = this.createOrUpdatePropertyOfPropertySet(
															productConfigurationTypeSet,
															productConfigurationTypeSetName,
															productConfigurationTypeSetDescription,
															productConfigurationType,
															productConfigurationTypeName,
															productConfigurationTypeDescription,
															null,
															null,
															null,
															null,
															propertyType,
															propertyName,
															propertyDescription,
															propertyValue,
															productSegment,
															app
														);
														if (property != null) {
															this.updateProductConfigurationType(
																(ProductConfigurationType)pm.getObjectById(property.refGetPath().getParent().getParent()),
																valueMap
															);
														}
													} else {
														jsBuffer += "$('r" + nRow + "').title += ' - verify data row';";
													}
													/* case 4 */                                  
													} else if (
														importTarget == ImportTarget.PropertySet &&
														propertyName != null && !propertyName.isEmpty()
													) {
														jsBuffer += "$('r" + nRow + "').title += 'Property Of PropertySet (called from PropertySet)';";
														if (
															((propertySetName == null || propertySetName.isEmpty()) ||
															(callerName != null && propertySetName != null && callerName.equals(propertySetName))) &&
															(productConfigurationTypeSetName == null || productConfigurationTypeSetName.isEmpty()) &&
															(productConfigurationTypeName == null || productConfigurationTypeName.isEmpty())
														) {
															property = this.createOrUpdatePropertyOfPropertySet(
																null,
																null,
																null,
																null,
																null,
																null,
																null,
																propertySet,
																propertySetName,
																propertySetDescription,
																propertyType,
																propertyName,
																propertyDescription,
																propertyValue,
																productSegment,
																app
															);
														} else {
															jsBuffer += "$('r" + nRow + "').title += ' - verify data row';";
														}
														/* case 5 */                                  
														} else if (
															importTarget == ImportTarget.CrxObject &&
															propertyName != null && !propertyName.isEmpty() &&
															propertySetName != null && !propertySetName.isEmpty()
														) {
															jsBuffer += "$('r" + nRow + "').title += 'Property Of PropertySet (called from CrxObject)';";
															if (
																(productConfigurationTypeSetName == null || productConfigurationTypeSetName.isEmpty()) &&
																(productConfigurationTypeName == null || productConfigurationTypeName.isEmpty())
															) {
																//createOrUpdatePropertyOfPropertySet
																property = this.createOrUpdatePropertyOfPropertySet(
																	null,
																	null,
																	null,
																	null,
																	null,
																	null,
																	crxObject,
																	null,
																	propertySetName,
																	propertySetDescription,
																	propertyType,
																	propertyName,
																	propertyDescription,
																	propertyValue,
																	productSegment,
																	app
																);
															} else {
																jsBuffer += "$('r" + nRow + "').title += ' - verify data row';";
															}
														} else {
															// incomplete and/or inconsistent row --> disregard this row
															jsBuffer += "$('r" + nRow + "').title += 'incomplete and/or inconsistent row';";

														}
									} else {
										appendErrorRow = "<tr class='gridTableRowFull'><td class='err' colspan='" + (maxCell+2) + "'>CELL VALUE TYPE NOT SUPPORTED</td></tr>";
									}
									this.importReport += "</tr>";
									if (property != null) {
										propertiesUpdated++;
										cellId =  "r" + nRow + ATTR_PROPERTY_NAME.toUpperCase();
										jsBuffer += "try{$('r" + nRow + "').className += ' ok';$('" + cellId + "').className=' ok';$('" + cellId + "').innerHTML = '<a href=\"" + this.getSelectObjectHref(property) + "\" target=\"_blank\"><b>' + " + "$('" + cellId + "').innerHTML +" + "'</b></a>'}catch(e){};";
										cellId =  "r" + nRow + ATTR_PROPERTY_VALUE.toUpperCase();
										if (propertyValue != null) {
											jsBuffer += "try{$('" + cellId + "').className='ok';}catch(e){};";
										}

									} else {
										appendErrorRow = "<tr class='gridTableRowFull'><td class='err' colspan='" + (maxCell+2) + "'>VERIFY Property DTYPE/NAME/VALUE</td></tr>";
										jsBuffer += "$('r" + nRow + "').className += 'nok';";
									}
									if (appendErrorRow != null) {
										this.importReport += appendErrorRow;
									}
									valueMap = null;
									this.importReport += "<tr style=\"display:none;\">";
									this.importReport += "  <td colspan=\"" + (maxCell+2) + "\">";
									this.importReport += jsBuffer.length() > 0 ? "<script language='javascript' type='text/javascript'>" + jsBuffer + "</script>" : "";
									this.importReport += "  </td>";
									this.importReport += "</tr>";
								} /* while */
								// Spacer
								this.importReport += "<tr class='gridTableRowFull' style=\"background-color:white;\">";
								this.importReport += "  <td colspan='" + (maxCell+2) + "'>&nbsp;</td>";								
								this.importReport += "</tr>";
								// Summary
								this.importReport += "<tr class='sheetInfo gridTableRowFull'>";
								this.importReport += "  <td colspan=\"" + (maxCell+2) + "\">";
								this.importReport += "  Sheet: <b>" + wb.getSheetName(i) + "</b> |";
								this.importReport += "  data lines <b>read: " + linesRead + "</b><br>";
								this.importReport += "</td>";
								this.importReport += "</tr>";
								this.importReport += "<tr class='sheetInfo gridTableRowFull'>";
								this.importReport += "  <td>&nbsp;</td>";
								this.importReport += "  <td colspan=\"" + (maxCell+1) + "\">Created / Updated</td>";
								this.importReport += "</tr>";
								this.importReport += "<tr class='sheetInfo gridTableRowFull'>";
								this.importReport += "  <td>Properties</td>";
								this.importReport += "  <td colspan=\"" + (maxCell+1) + "\">" + propertiesUpdated + "</td>";
								this.importReport += "</tr>";
								if (linesRead != propertiesUpdated) {
									this.importReport += "<tr class='sheetInfo gridTableRowFull'>";
									this.importReport += "  <td class=\"err\" colspan=\"" + (maxCell+2) + "\">WARNING: some data lines were not processed due to data errors (e.g. multiple matches, missing name, etc.)</td>";
									this.importReport += "</tr>";
								}
							}
						}
					}
				} else {
					this.errorMessage = "No Excel workbook selected";					
				}
			} finally {
				new File(location).delete();
			}
		}
	}

    /**
	 * @return the importReport
	 */
	public String getImportReport() {
		return importReport;
	}

	/**
	 * @return the errorMessage
	 */
	public String getErrorMessage() {
		return errorMessage;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
    public static final String EOL_HTML = "<br>";

    public static final NumberFormat DECIMAL_FORMAT_0000 = new DecimalFormat("0000");

    public static final String UPLOAD_FILE_FIELD_NAME = "uploadFile";

    public static final String PROPERTY_DTYPE_STRING = "StringProperty";
    public static final String PROPERTY_DTYPE_DECIMAL = "DecimalProperty";
    public static final String PROPERTY_DTYPE_INTEGER = "IntegerProperty";
    public static final String PROPERTY_DTYPE_BOOLEAN = "BooleanProperty";
    public static final String PROPERTY_DTYPE_DATE = "DateProperty";
    public static final String PROPERTY_DTYPE_DATETIME = "DateTimeProperty";
    public static final String PROPERTY_DTYPE_URI = "UriProperty";
    public static final String PROPERTY_DTYPE_REFERENCE = "ReferenceProperty";

    public static final String ATTR_PROPERTY_DTYPE = "Property_dtype";
    public static final String ATTR_PROPERTY_NAME = "Property_name";
    public static final String ATTR_PROPERTY_DESCRIPTION = "Property_description";
    public static final String ATTR_PROPERTY_VALUE = "Property_value";

    public static final String ATTR_PROPERTYSET_NAME = "PropertySet_name";
    public static final String ATTR_PROPERTYSET_DESCRIPTION = "PropertySet_description";
    public static final String ATTR_PROPERTYSET_VALUE = "PropertySet_value";

    public static final String ATTR_PRODUCTCONFIGURATIONTYPE_NAME = "ProductConfigurationType_name";
    public static final String ATTR_PRODUCTCONFIGURATIONTYPE_DESCRIPTION = "ProductConfigurationType_description";
    public static final String ATTR_PRODUCTCONFIGURATIONTYPE_VALIDFROM = "ProductConfigurationType_validFrom";
    public static final String ATTR_PRODUCTCONFIGURATIONTYPE_VALIDTO = "ProductConfigurationType_validTo";
    public static final String ATTR_PRODUCTCONFIGURATIONTYPE_ISDEFAULT = "ProductConfigurationType_isDefault";

    public static final String ATTR_PRODUCTCONFIGURATIONTYPESET_NAME = "ProductConfigurationTypeSet_name";
    public static final String ATTR_PRODUCTCONFIGURATIONTYPESET_DESCRIPTION = "ProductConfigurationTypeSet_description";

    private final boolean requiresAdminRole;
    private boolean hasPermission;
	private String importReport;
    private String errorMessage;
    
}
