/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Selenium2Example
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2014, CRIXP Corp., Switzerland
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
package test.org.opencrx.kernel.portal;

import java.util.Date;

import org.openmdx.base.exception.ServiceException;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.support.ui.ExpectedCondition;
import org.openqa.selenium.support.ui.WebDriverWait;

/**
 * BasicLoadTester
 *
 */
public class BasicLoadTester  {

	/**
	 * LoadTesterThread
	 *
	 */
	public static class LoadTesterThread extends Thread {

		/**
		 * Wait until element with given name is found on page.
		 * 
		 * @param driver
		 * @param name
		 */
		void waitForPageByName(
			WebDriver driver,
			final String name
		) {
			try {
				Thread.sleep(500);
			} catch(Exception ignore) {}
	        (new WebDriverWait(driver, 10)).until(new ExpectedCondition<Boolean>() {
	            public Boolean apply(WebDriver d) {
	                System.out.println(new Date() + "  BasicLoadTester " + Thread.currentThread().getId() + ": Waiting for " + name);
	                WebElement e = null;
	                try {
	                	e = d.findElement(By.name(name));
	                } catch(Exception ignore) {}
	                return e != null;
	            }
	        });
		}

		/**
		 * Wait until element with given id is found on page.
		 * 
		 * @param driver
		 * @param id
		 */
		void waitForPageById(
			WebDriver driver,
			final String id
		) {
			try {
				Thread.sleep(500);
			} catch(Exception ignore) {}
	        (new WebDriverWait(driver, 10)).until(new ExpectedCondition<Boolean>() {
	            public Boolean apply(WebDriver d) {
	                System.out.println(new Date() + "  BasicLoadTester " + Thread.currentThread().getId() + ": Waiting for " + id);
	                WebElement e = null;
	                try {
	                	e = d.findElement(By.id(id));
	                } catch(Exception ignore) {}
	                return e != null;
	            }
	        });
		}

		/**
		 * Delay between user clicks.
		 * 
		 */
		void userDelay(
		) {
			try {
				Thread.sleep(7000);
			} catch(Exception ignore) {}
		}

		/* (non-Javadoc)
		 * @see java.lang.Thread#run()
		 */
		@Override
		public void run(
		) {
			final int N_RUNS = 20;
	    	WebDriver driver = new FirefoxDriver();
	    	try {
		        // Load login page
		        {
		        	driver.get("http://localhost:8080/opencrx-core-CRX");
		        	waitForPageByName(driver, "j_username");
		        }
		        // Login
		        {
			        WebElement eusername = driver.findElement(By.name("j_username"));
			        eusername.clear();
			        eusername.sendKeys("guest");
			        WebElement epassword = driver.findElement(By.name("j_password"));
			        epassword.clear();
			        epassword.sendKeys("guest");
			        WebElement loginForm = driver.findElement(By.tagName("form"));
			        loginForm.submit();
			        waitForPageById(driver, "G_0");
			        userDelay();
		        }
		        long totalDuration = 0;
		        for(int run = 0; run < N_RUNS; run++) {
	                System.out.println(new Date() + "  BasicLoadTester: " + Thread.currentThread().getId() + ": run " + run + " starting");
	                long startAt = System.currentTimeMillis();
		        	try {
				        // Root objects
				        for(int i = 1; i < 6; i++) {
					        WebElement eAnchorRoot = driver.findElement(By.xpath("/html/body/div[7]/div/div[2]/ul[2]/li[" + i + "]/a"));
					        eAnchorRoot.click();
					        waitForPageById(driver, "G_0");
					        userDelay();
					        // Search accounts
					        if(driver.getTitle().endsWith(" - Manage Accounts")) {
					        	WebElement gridSearchButton = driver.findElement(By.xpath("/html/body/div[8]/div/div[2]/div[1]/div[2]/div[4]/div[1]/table/tbody/tr/td[1]/ul/li[6]/a"));
					        	gridSearchButton.click();
					        	WebElement emailAddressSearchField = driver.findElement(By.id("searchFormG_0_0.address*Business!emailAddress"));
					        	emailAddressSearchField.clear();
					        	emailAddressSearchField.sendKeys(".com");
					        	WebElement searchFormOkButton = driver.findElement(By.xpath("/html/body/div[8]/div/div[2]/div[1]/div[2]/div[4]/div[2]/div[1]/div[2]/table/tbody/tr/td/form/fieldset/table/tbody/tr[2]/td/div/div/button[2]"));
					        	searchFormOkButton.click();
						        waitForPageById(driver, "G_0");
						        userDelay();
					        }
					        // Search products
					        if(driver.getTitle().endsWith(" - Products")) {
					        	WebElement gridSearchButton = driver.findElement(By.xpath("/html/body/div[8]/div/div[2]/div[1]/div[2]/div[4]/div[1]/table/tbody/tr/td[1]/ul/li[6]/a"));
					        	gridSearchButton.click();
					        	WebElement nameSearchField = driver.findElement(By.id("searchFormG_0_0.name"));
					        	nameSearchField.clear();
					        	nameSearchField.sendKeys("te");
					        	WebElement searchFormOkButton = driver.findElement(By.xpath("/html/body/div[8]/div/div[2]/div[1]/div[2]/div[4]/div[2]/div[1]/div[2]/table/tbody/tr/td/form/fieldset/table/tbody/tr[2]/td/div/div/button[2]"));
					        	searchFormOkButton.click();
						        waitForPageById(driver, "G_0");
						        userDelay();
					        }
					        // Page next on first grid
					        for(int j = 0; j < 3; j++) {
						        WebElement pageNext = driver.findElement(By.xpath("/html/body/div[8]/div/div[2]/div[1]/div[2]/div[4]/div[1]/table/tbody/tr/td[1]/ul/li[4]/a"));
						        pageNext.click();
						        waitForPageById(driver, "G_0");
						        userDelay();
					        }
				        }
		        	} catch(Exception ignore) {}
			        long endAt = System.currentTimeMillis();
			        long duration = endAt - startAt;
	                System.out.println(new Date() + "  BasicLoadTester: " + Thread.currentThread().getId() + ": run " + run + " duration=" + duration);
			        totalDuration += duration;
		        }
		        // Logoff
		        {
		        	WebElement userButton = driver.findElement(By.xpath("/html/body/div[7]/div/div[2]/ul[3]/li/a"));
		        	userButton.click();  	
		        	WebElement logoffButton = driver.findElement(By.xpath("/html/body/div[7]/div/div[2]/ul[3]/li/ul/li[1]/a"));
		        	logoffButton.click();
		        	waitForPageByName(driver, "j_username");
		        }
	            System.out.println(new Date() + "  BasicLoadTester: " + Thread.currentThread().getId() + ": duration.total=" + totalDuration + "; duration.average=" + (totalDuration / N_RUNS));
	    	} catch(Exception e) {
	            System.out.println(new Date() + "  BasicLoadTester: " + Thread.currentThread().getId() + ": excetpion. Reason is " + e.getMessage());
	            new ServiceException(e).log();
	    	} finally {
	    		if(driver != null) {
	    			driver.quit();
	    		}
	    	}
		}
	}

    /**
     * @param args
     */
    public static void main(
    	String[] args
    ) {
    	final int N_THREADS = 20;
    	Thread[] loadTester = new Thread[N_THREADS];
    	for(int i = 0; i < loadTester.length; i++) {
    		loadTester[i] = new LoadTesterThread();
    		loadTester[i].start();
    		try {
    			Thread.sleep(10000);
    		} catch(Exception ignore) {}
    	}
    	for(int i = 0; i < loadTester.length; i++) {
    		try {
    			loadTester[i].join();
    		} catch(Exception ignore) {}
    	}
    }

}
