@ECHO %CD%
REM @ECHO OFF
SET JAVA_13_HOME=e:\pgm\jdk1.3.1
SET PATH=%JAVA_13_HOME%\jre\bin;%JAVA_13_HOME%\jre\bin\hotspot;C:\Xalan;%PATH%

SET LC_ALL=EN_EN

echo %time%
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/accountcategory.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/accountcategory.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/accountstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/accountstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/accounttype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/accounttype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/alertstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/alertstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/businesstype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/businesstype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/casetype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/casetype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/catchallundefined.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/catchallundefined.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/contactmethod.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/contactmethod.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/contractstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/contractstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/currency.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/currency.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/documentstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/documentstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/education.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/education.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/emailactivitystate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/emailactivitystate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/emailformat.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/emailformat.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/emailtype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/emailtype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/employmentposition.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/employmentposition.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/exportimport.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/exportimport.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/familystatus.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/familystatus.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/freightterms.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/freightterms.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/gender.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/gender.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/groupmemberrole.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/groupmemberrole.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/incidentstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/incidentstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/industry.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/industry.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/informationsource.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/informationsource.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/invoicestate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/invoicestate.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/language.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/language.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/leadstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/leadstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/literaturetype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/literaturetype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/locale.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/locale.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/meetingstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/meetingstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/numberofemployeescategory.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/numberofemployeescategory.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/objectstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/objectstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/opportunitystate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/opportunitystate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/partytype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/partytype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/paymentterms.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/paymentterms.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/phonecountryprefix.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/phonecountryprefix.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/postalcountry.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/postalcountry.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/priority.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/priority.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/productstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/productstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/producttype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/producttype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/rating.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/rating.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/ratingtype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/ratingtype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/relationshiptype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/relationshiptype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/requestedmimetype.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/requestedmimetype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/salutationcode.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/salutationcode.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/satisfaction.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/satisfaction.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/severity.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/severity.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/shippingmethod.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/shippingmethod.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/status.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/status.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/taskstate.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/taskstate.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/usageaddress.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/usageaddress.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/usageaddress_ext_email.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/usageaddress_ext_email.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/usageaddress_ext_phone.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/usageaddress_ext_phone.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/usageaddress_ext_postal.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/usageaddress_ext_postal.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/usageaddress_ext_web.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/usageaddress_ext_web.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/usageproductbaseprice.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/usageproductbaseprice.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/utcoffset.xml -XSL reduceCodeToOneLanguage.xsl -OUT out/utcoffset.xml
echo %time%
