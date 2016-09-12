@ECHO %CD%
REM @ECHO OFF
SET JAVA_13_HOME=e:\pgm\jdk1.3.1
SET PATH=%JAVA_13_HOME%\jre\bin;%JAVA_13_HOME%\jre\bin\hotspot;C:\Xalan;%PATH%

SET LC_ALL=EN_EN

echo %time%
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/abstractcontract.xml -XSL reduceToOneLanguage.xsl -OUT out/abstractcontract.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/account.xml -XSL reduceToOneLanguage.xsl -OUT out/account.xml  
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/activity.xml -XSL reduceToOneLanguage.xsl -OUT out/activity.xml  
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/address.xml -XSL reduceToOneLanguage.xsl -OUT out/address.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/addressgroup.xml -XSL reduceToOneLanguage.xsl -OUT out/addressgroup.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/budget.xml -XSL reduceToOneLanguage.xsl -OUT out/budget.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/building.xml -XSL reduceToOneLanguage.xsl -OUT out/building.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/code.xml -XSL reduceToOneLanguage.xsl -OUT out/code.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/common.xml -XSL reduceToOneLanguage.xsl -OUT out/common.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/competitor.xml -XSL reduceToOneLanguage.xsl -OUT out/competitor.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/contact.xml -XSL reduceToOneLanguage.xsl -OUT out/contact.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/contactrelationship.xml -XSL reduceToOneLanguage.xsl -OUT out/contactrelationship.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/contractoverview.xml -XSL reduceToOneLanguage.xsl -OUT out/contractoverview.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/contractposition.xml -XSL reduceToOneLanguage.xsl -OUT out/contractposition.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/deliveryrequest.xml -XSL reduceToOneLanguage.xsl -OUT out/deliveryrequest.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/discounttype.xml -XSL reduceToOneLanguage.xsl -OUT out/discounttype.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/document.xml -XSL reduceToOneLanguage.xsl -OUT out/document.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/emailactivity.xml -XSL reduceToOneLanguage.xsl -OUT out/emailactivity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/faxactivity.xml -XSL reduceToOneLanguage.xsl -OUT out/faxactivity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/forecast.xml -XSL reduceToOneLanguage.xsl -OUT out/forecast.xml
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/group.xml -XSL reduceToOneLanguage.xsl -OUT out/group.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/hidden.xml -XSL reduceToOneLanguage.xsl -OUT out/hidden.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/incident.xml -XSL reduceToOneLanguage.xsl -OUT out/incident.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/invoice.xml -XSL reduceToOneLanguage.xsl -OUT out/invoice.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/lead.xml -XSL reduceToOneLanguage.xsl -OUT out/lead.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/legalentity.xml -XSL reduceToOneLanguage.xsl -OUT out/legalentity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/mailingactivity.xml -XSL reduceToOneLanguage.xsl -OUT out/mailingactivity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/meeting.xml -XSL reduceToOneLanguage.xsl -OUT out/meeting.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/mmsactivity.xml -XSL reduceToOneLanguage.xsl -OUT out/mmsactivity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/opportunity.xml -XSL reduceToOneLanguage.xsl -OUT out/opportunity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/organization.xml -XSL reduceToOneLanguage.xsl -OUT out/organization.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/phoneactivity.xml -XSL reduceToOneLanguage.xsl -OUT out/phoneactivity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/pricelevel.xml -XSL reduceToOneLanguage.xsl -OUT out/pricelevel.xml  
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/product.xml -XSL reduceToOneLanguage.xsl -OUT out/product.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/quote.xml -XSL reduceToOneLanguage.xsl -OUT out/quote.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/salesorder.xml -XSL reduceToOneLanguage.xsl -OUT out/salesorder.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/salestaxtype.xml -XSL reduceToOneLanguage.xsl -OUT out/salestaxtype.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/searchindexentry.xml -XSL reduceToOneLanguage.xsl -OUT out/searchindexentry.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/shippingdetail.xml -XSL reduceToOneLanguage.xsl -OUT out/shippingdetail.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/smsactivity.xml -XSL reduceToOneLanguage.xsl -OUT out/smsactivity.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/task.xml -XSL reduceToOneLanguage.xsl -OUT out/task.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/unspecifiedaccount.xml -XSL reduceToOneLanguage.xsl -OUT out/unspecifiedaccount.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/uom.xml -XSL reduceToOneLanguage.xsl -OUT out/uom.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/userhome.xml -XSL reduceToOneLanguage.xsl -OUT out/userhome.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/warehouse.xml -XSL reduceToOneLanguage.xsl -OUT out/warehouse.xml 
java -Xmx300M -classpath xalan.jar;xml-apis.jar;xercesImpl.jar org.apache.xalan.xslt.Process -IN in/wf.xml -XSL reduceToOneLanguage.xsl -OUT out/wf.xml 
echo %time%
