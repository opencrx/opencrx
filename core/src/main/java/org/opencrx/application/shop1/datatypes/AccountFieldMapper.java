/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: AccountFieldMapper
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
package org.opencrx.application.shop1.datatypes;

import java.util.List;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;


public class AccountFieldMapper {

    public String getAccountNumber(
        Account account
    ) {
        return account.getAliasName();
    }
    
    public List<String> getStateHistory(
        Account account
    ) {
        return account.getUserString4();
    }

    public Boolean isTestAccount(
        Account account
    ) {
        return account.isUserBoolean0(); 
    }
    
    public String getUserName(
        Account account
    ) {
        return account.getUserString0();
    }
    
    public String getPasswordMd5(
        Account account
    ) {
        return account.getUserString1();
    }
    
    public String getResetPasswordChallenge(
        Account account
    ) {
        return account.getUserString2();    
    }
    
    public String getResetPasswordResponse(
        Account account
    ) {
        return account.getUserString3();        
    }

    public Integer getTitle(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode9());
    }
    
    public String getPlaceOfBirth(
        Account account
    ) {
        return account.getExtString1();
    }
    
    public Integer getBirthDateIsValidated(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode10());
    }
    
    public Integer getJobRole(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode11());
    }
        
    public Integer getAnnualIncomeAmount(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode12());
    }
            
    public Integer getAnnualIncomeCurrency(
        Account account
    ) {
        return DatatypeMappers.toInteger(((Contact)account).getAnnualIncomeCurrency());
    }
                
    public Integer getMonthlyIncomeAmount(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode13());
    }
                
    public Integer getMonthlyIncomeCurrency(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode14());
    }
                    
    public Integer getNativeLanguage(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode0());
    }
    
    public Integer getCommunityStatus(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode1());
    }
    
    public Integer getCommerceStatus(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode2());
    }
    
    public Integer getPersonsInHousehold(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode3());
    }
    
    public Integer getProfessionalSkills(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode4());
    }
    
    public Integer getInternetUsage(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode5());
    }
    
    public Integer getInternetProvider(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode6());
    }
    
    public List<Integer> getPcUsage(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode29());
    }
    
    public Integer getPortalRating(
        Account account
    ) {
        return DatatypeMappers.toInteger(account.getExtCode8());
    }
    
    public List<Integer> getSports(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode20());
    }
    
    public List<Integer> getTravel(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode21());
    }
    
    public List<Integer> getFinance(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode22());
    }
    
    public List<Integer> getComputerInternet(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode23());
    }
    
    public List<Integer> getTelecommunication(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode24());
    }
    
    public List<Integer> getEntertainment(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode25());
    }
    
    public List<Integer> getMusic(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode26());
    }
    
    public List<Integer> getLifestyle(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode27());
    }
    
    public List<Integer> getOther(
        Account account
    ) {
        return DatatypeMappers.toIntegerList(account.getExtCode28());
    }
    
    public void setUserName(
        Account account,
        String newValue
    ) {
        account.setUserString0(newValue);
    }
    
    public void setPasswordMd5(
        Account account,
        String newValue
    ) {
        account.setUserString1(newValue);
    }
    
    public void setResetPasswordChallenge(
        Account account,
        String newValue
    ) {
        account.setUserString2(newValue);    
    }
    
    public void setResetPasswordResponse(
        Account account,
        String newValue
    ) {
        account.setUserString3(newValue);        
    }

    public void setSports(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode20(DatatypeMappers.toShortList(newValue));
    }
    
    public void setTravel(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode21(DatatypeMappers.toShortList(newValue));
    }
    
    public void setFinance(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode22(DatatypeMappers.toShortList(newValue));
    }
    
    public void setComputerInternet(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode23(DatatypeMappers.toShortList(newValue));
    }
    
    public void setTelecommunication(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode24(DatatypeMappers.toShortList(newValue));
    }
    
    public void setEntertainment(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode25(DatatypeMappers.toShortList(newValue));
    }
    
    public void setMusic(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode26(DatatypeMappers.toShortList(newValue));
    }
    
    public void setLifestyle(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode27(DatatypeMappers.toShortList(newValue));
    }
    
    public void setOther(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode28(DatatypeMappers.toShortList(newValue));
    }

    public void setTitle(
        Account account,
        Integer newValue
    ) {
        account.setExtCode9(DatatypeMappers.toShort(newValue));
    }
    
    public void setPlaceOfBirth(
        Account account,
        String newValue
    ) {
        account.setExtString1(newValue);
    }
    
    public void setBirthDateIsValidated(
        Account account,
        Integer newValue
    ) {
        account.setExtCode10(DatatypeMappers.toShort(newValue));
    }
    
    public void setJobRole(
        Account account,
        Integer newValue
    ) {
        account.setExtCode11(DatatypeMappers.toShort(newValue));
    }
        
    public void setAnnualIncomeAmount(
        Account account,
        Integer newValue
    ) {
        account.setExtCode12(DatatypeMappers.toShort(newValue));
    }
            
    public void setAnnualIncomeCurrency(
        Account account,
        Integer newValue
    ) {
        ((Contact)account).setAnnualIncomeCurrency(DatatypeMappers.toShort(newValue));
    }
                
    public void setMonthlyIncomeAmount(
        Account account,
        Integer newValue
    ) {
        account.setExtCode13(DatatypeMappers.toShort(newValue));
    }
                
    public void setMonthlyIncomeCurrency(
        Account account,
        Integer newValue
    ) {
        account.setExtCode14(DatatypeMappers.toShort(newValue));
    }
                            
    public void setNativeLanguage(
        Account account,
        Integer newValue
    ) {
        account.setExtCode0(DatatypeMappers.toShort(newValue));
    }
    
    public void setCommunityStatus(
        Account account,
        Integer newValue
    ) {
        account.setExtCode1(DatatypeMappers.toShort(newValue));
    }
    
    public void setCommerceStatus(
        Account account,
        Integer newValue
    ) {
        account.setExtCode2(DatatypeMappers.toShort(newValue));
    }
    
    public void setPersonsInHousehold(
        Account account,
        Integer newValue
    ) {
        account.setExtCode3(DatatypeMappers.toShort(newValue));
    }
    
    public void setProfessionalSkills(
        Account account,
        Integer newValue
    ) {
        account.setExtCode4(DatatypeMappers.toShort(newValue));
    }
    
    public void setInternetUsage(
        Account account,
        Integer newValue
    ) {
        account.setExtCode5(DatatypeMappers.toShort(newValue));
    }
    
    public void setInternetProvider(
        Account account,
        Integer newValue
    ) {
        account.setExtCode6(DatatypeMappers.toShort(newValue));
    }
    
    public void setPcUsage(
        Account account,
        List<Integer> newValue
    ) {
        account.setExtCode29(DatatypeMappers.toShortList(newValue));
    }
    
    public void setPortalRating(
        Account account,
        Integer newValue
    ) {
        account.setExtCode8(DatatypeMappers.toShort(newValue));
    }
    
    public String getBlogAddress(
        Account account
    ) {
        return account.getExtString2();
    }

    public void setBlogAddress(
        Account account,
        String newValue
    ) {
        account.setExtString2(newValue);
    }
    
}
