/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateProductWizardController
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

import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.jdo.PersistenceManager;
import jakarta.servlet.http.HttpServletRequest;

import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.product1.jmi1.PriceLevel;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.opencrx.kernel.product1.jmi1.SalesTaxType;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JspWizardController;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;

/**
 * CreateProductWizardController
 *
 */
public class CreateProductWizardController extends JspWizardController {

	/**
	 * Constructoir.
	 * 
	 */
	public CreateProductWizardController(
	) {
		super();
	}

    /**
     * Refresh action.
     * 
     * @param productBasePriceCount
     * @param formFields
     * @throws ServiceException
     */
    public void doRefresh(
    	@JspWizardController.RequestParameter(name = "ProductBasePriceCount") Integer productBasePriceCount,
    	@JspWizardController.FormParameter(forms = {"CreateProductForm", "ProductBasePriceForm"}) Map<String,Object> formFields	
    ) throws ServiceException {
    	ApplicationContext app = this.getApp();
    	this.formFields = formFields;
    	this.productBasePriceCount = productBasePriceCount == null ? 0 : productBasePriceCount;
    	// Get product base price values
    	for(int i = 0; i < 100; i++) {
    		HttpServletRequest request = this.getRequest();
    	    if(request.getParameter("productBasePrice.priceLevel." + i) != null) {
    	        List<Short> usage = new ArrayList<Short>();
    	        StringTokenizer tokenizer = new StringTokenizer(request.getParameter("productBasePrice.usage." + i), ",[] ", false);
    	        while(tokenizer.hasMoreTokens()) {
    	            usage.add(Short.valueOf(tokenizer.nextToken()));
    	        }
    		    this.formFields.put(
    		        "productBasePrice.usage." + i,
    		        usage
    		    );
    		    this.formFields.put(
    		        "productBasePrice.priceLevel." + i,
    		        new Path(request.getParameter("productBasePrice.priceLevel." + i))
    		    );
    		    this.formFields.put(
    		        "productBasePrice.price." + i,
    		        app.parseNumber(request.getParameter("productBasePrice.price." + i))
    		    );
    		    this.formFields.put(
    		        "productBasePrice.priceCurrency." + i,
    		        Short.valueOf(request.getParameter("productBasePrice.priceCurrency." + i))
    		    );
    		    this.formFields.put(
    		        "productBasePrice.quantityFrom." + i,
    		        app.parseNumber(request.getParameter("productBasePrice.quantityFrom." + i))
    		    );
    		    this.formFields.put(
    		        "productBasePrice.quantityTo." + i,
    		        app.parseNumber(request.getParameter("productBasePrice.quantityTo." + i))
    		    );
    		    this.formFields.put(
    		        "productBasePrice.discountIsPercentage." + i,
    		        "on".equals(request.getParameter("productBasePrice.discountIsPercentage." + i))
    		    );
    		    this.formFields.put(
    		        "productBasePrice.discount." + i,
    		        app.parseNumber(request.getParameter("productBasePrice.discount." + i))
    		    );
    	    }
    	}    	
    }

    /**
     * OK action.
     * 
     * @param isInitialized
     * @param productBasePriceCount
     * @param formFields
     * @throws ServiceException
     */
    public void doOK(
    	@JspWizardController.RequestParameter(name = "ProductBasePriceCount") Integer productBasePriceCount,
    	@JspWizardController.FormParameter(forms = {"CreateProductForm", "ProductBasePriceForm"}) Map<String,Object> formFields	
    ) throws ServiceException {
    	PersistenceManager pm = this.getPm();
    	ApplicationContext app = this.getApp();
    	this.doRefresh(
    		productBasePriceCount, 
    		formFields
    	);
    	org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(pm, this.getProviderName(), this.getSegmentName());
	    String productName = (String)this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:name");
	    String productNumber = (String)this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:productNumber");
	    SalesTaxType salesTaxType = this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:salesTaxType") != null ?
	    	(SalesTaxType)pm.getObjectById(
	    		this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:salesTaxType")
	    	) : null;
	    org.opencrx.kernel.uom1.jmi1.Uom defaultUom = this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:defaultUom") != null ?
	    	(org.opencrx.kernel.uom1.jmi1.Uom)pm.getObjectById(
	    		this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:defaultUom")
	    	) : null;
	    if(
	        (productName != null) &&
	        (productNumber != null) &&
	        (salesTaxType != null) &&
	        (defaultUom != null)
	    ) {
	        Product product = pm.newInstance(Product.class);
	        product.setName(productName);
	        product.setProductState((Short)this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:productState"));
	        product.setProductNumber(productNumber);
	        product.setDefaultUom(defaultUom);
	        product.getPriceUom().add(defaultUom);
	        product.setActiveOn((Date)this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:activeOn"));
	        product.setExpiresOn((Date)this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:expiresOn"));
	        product.setSalesTaxType(salesTaxType);
	        product.setGrossWeightKilogram((java.math.BigDecimal)this.formFields.get("org:opencrx:kernel:product1:Product:grossWeightKilogram"));
	        product.setNetWeightKilogram((java.math.BigDecimal)this.formFields.get("org:opencrx:kernel:product1:Product:netWeightKilogram"));
	        product.setProductDimension((String)this.formFields.get("org:opencrx:kernel:product1:Product:productDimension"));
	        product.setDescription((String)this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:description"));
	        product.setDetailedDescription((String)this.formFields.get("org:opencrx:kernel:product1:AbstractProduct:detailedDescription"));
	        pm.currentTransaction().begin();
	        productSegment.addProduct(
	            Base.getInstance().getUidAsString(),
	            product
	        );
	        pm.currentTransaction().commit();
			// Create product base prices
			pm.currentTransaction().begin();
			for(int i = 0; i < 100; i++) {
			    if(this.formFields.get("productBasePrice.priceLevel." + i) != null) {
			        ProductBasePrice basePrice = pm.newInstance(ProductBasePrice.class);
				    basePrice.getPriceLevel().add(
				    	(org.opencrx.kernel.product1.jmi1.PriceLevel)pm.getObjectById(
				    		this.formFields.get("productBasePrice.priceLevel." + i)
				    	)
				    );
				    basePrice.setUom(defaultUom);
				    @SuppressWarnings("unchecked")
                    List<Short> usage = (List<Short>)this.formFields.get("productBasePrice.usage." + i);
				    basePrice.getUsage().addAll(usage);
				    basePrice.setPrice((java.math.BigDecimal)this.formFields.get("productBasePrice.price." + i));
				    basePrice.setPriceCurrency((Short)this.formFields.get("productBasePrice.priceCurrency." + i));
				    basePrice.setQuantityFrom((java.math.BigDecimal)this.formFields.get("productBasePrice.quantityFrom." + i));
				    basePrice.setQuantityTo((java.math.BigDecimal)this.formFields.get("productBasePrice.quantityTo." + i));
				    basePrice.setDiscountIsPercentage((Boolean)this.formFields.get("productBasePrice.discountIsPercentage." + i));
				    basePrice.setDiscount((java.math.BigDecimal)this.formFields.get("productBasePrice.discount." + i));
				    product.addBasePrice(
				        Base.getInstance().getUidAsString(),
				        basePrice
				    );
			    }
			}
			pm.currentTransaction().commit();
			this.setExitAction(
				new ObjectReference(product, app).getSelectObjectAction()
			);
	    }    	
    }
    
    /**
     * AddProductBasePrice action.
     * 
     * @param isInitialized
     * @param productBasePriceCount
     * @param formFields
     * @throws ServiceException
     */
    public void doAddProductBasePrice(
    	@JspWizardController.RequestParameter(name = "ProductBasePriceCount") Integer productBasePriceCount,
    	@JspWizardController.FormParameter(forms = {"CreateProductForm", "ProductBasePriceForm"}) Map<String,Object> formFields	
    ) throws ServiceException {
    	PersistenceManager pm = this.getPm();
    	this.doRefresh(
    		productBasePriceCount, 
    		formFields
    	);
	    PriceLevel priceLevel = this.formFields.get("org:opencrx:kernel:product1:AbstractPriceLevel:basedOn") != null ?
	    	(PriceLevel)pm.getObjectById(
	    		this.formFields.get("org:opencrx:kernel:product1:AbstractPriceLevel:basedOn")
	    	) : null;
	    java.math.BigDecimal price = (java.math.BigDecimal)this.formFields.get("org:opencrx:kernel:product1:AbstractProductPrice:price");
	    Short priceCurrency = priceLevel == null ? null : priceLevel.getPriceCurrency();
	    @SuppressWarnings("unchecked")
        List<Object> usage = (List<Object>)this.formFields.get("org:opencrx:kernel:product1:AbstractProductPrice:usage");
	    if(
	        (priceLevel != null) &&
	        (price != null) &&
	        (priceCurrency != null) &&
	        (usage != null)
	    ) {
	        this.formFields.put(
	            "productBasePrice.usage." + productBasePriceCount,
	            usage
	        );
	        this.formFields.put(
	            "productBasePrice.priceLevel." + productBasePriceCount,
	            priceLevel.refGetPath()
	        );
	        this.formFields.put(
	            "productBasePrice.price." + productBasePriceCount,
	            price
	        );
	        this.formFields.put(
	            "productBasePrice.priceCurrency." + productBasePriceCount,
	            priceCurrency
	        );
	        this.formFields.put(
	            "productBasePrice.quantityFrom." + productBasePriceCount,
	            formFields.get("org:opencrx:kernel:product1:AbstractProductPrice:quantityFrom")
	        );
	        this.formFields.put(
	            "productBasePrice.quantityTo." + productBasePriceCount,
	            formFields.get("org:opencrx:kernel:product1:AbstractProductPrice:quantityTo")
	        );
	        this.formFields.put(
	            "productBasePrice.discountIsPercentage." + productBasePriceCount,
	            formFields.get("org:opencrx:kernel:product1:AbstractProductPrice:discountIsPercentage")
	        );
	        this.formFields.put(
	            "productBasePrice.discount." + productBasePriceCount,
	            formFields.get("org:opencrx:kernel:product1:AbstractProductPrice:discount")
	        );
	        this.productBasePriceCount++;
	    }
    }

    /**
     * DeleteProductBasePrice action.
     * 
     * @param isInitialized
     * @param productBasePriceCount
     * @param deleteProductBasePriceIndex
     * @param formFields
     * @throws ServiceException
     */
    public void doDeleteProductBasePrice(
    	@JspWizardController.RequestParameter(name = "ProductBasePriceCount") Integer productBasePriceCount,
    	@JspWizardController.RequestParameter(name = "DeleteProductBasePriceIndex") Integer deleteProductBasePriceIndex,
    	@JspWizardController.FormParameter(forms = {"CreateProductForm", "ProductBasePriceForm"}) Map<String,Object> formFields	
    ) throws ServiceException {
    	this.doRefresh(
    		productBasePriceCount, 
    		formFields
    	);
    	if(deleteProductBasePriceIndex != null) {
		    this.formFields.remove("productBasePrice.priceLevel." + deleteProductBasePriceIndex);
		    this.formFields.remove("productBasePrice.usage." + deleteProductBasePriceIndex);
		    this.formFields.remove("productBasePrice.price." + deleteProductBasePriceIndex);
		    this.formFields.remove("productBasePrice.priceCurrency." + deleteProductBasePriceIndex);
		    this.formFields.remove("productBasePrice.quantityFrom." + deleteProductBasePriceIndex);
		    this.formFields.remove("productBasePrice.quantityTo." + deleteProductBasePriceIndex);
		    this.formFields.remove("productBasePrice.discountIsPercentage." + deleteProductBasePriceIndex);
		    this.formFields.remove("productBasePrice.discount." + deleteProductBasePriceIndex);
    	}
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
	 * @return the form values
	 */
	public Map<String,Object> getFormFields(
	) {
		return this.formFields;
	}

	/**
	 * @return the productBasePriceCount
	 */
	public int getProductBasePriceCount(
	) {
		return this.productBasePriceCount;
	}

	/**
	 * Get viewPort.
	 * 
	 * @param out
	 * @return
	 */
	public ViewPort getViewPort(
		Writer out
	) {
		if(this.viewPort == null) {
			TransientObjectView view = new TransientObjectView(
				this.getFormFields(),
				this.getApp(),
				this.getObject(),
				this.getPm()
			);
			this.viewPort = ViewPortFactory.openPage(
				view,
				this.getRequest(),
				out
			);
		}
		return this.viewPort;
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.JspWizardController#close()
	 */
    @Override
    public void close(
    ) throws ServiceException {
	    super.close();
		if(this.viewPort != null) {
			this.viewPort.close(false);		
		}	    
    }
    
    //-------------------------------------------------------------------
	// Members
	//-------------------------------------------------------------------	
	private Map<String,Object> formFields;
	private int productBasePriceCount = 0;
	private ViewPort viewPort;
}
