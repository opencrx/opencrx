<?xml version="1.0" encoding="UTF-8"?>
<!-- copy XML file EXCEPT label/toolTip elements -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xalan="http://xml.apache.org/xslt">
    <xsl:output method="xml" encoding="UTF-8" indent="yes" xalan:indent-amount="2"/>
    <xsl:variable name="localeId">2</xsl:variable>                       <!-- index if the <_item>-Tag to select -->
    <xsl:variable name="locale"></xsl:variable>                 <!-- local Attribute to insert -->
    <xsl:variable name="removeLangNeutral">false</xsl:variable>  <!-- all language neutral Tags are removed if set to true -->
    <xsl:template match="@* | node()">
        <xsl:choose>

            <xsl:when test="name() = 'label'">
                <xsl:element name="label">
                    <xsl:element name="_item">
                        <xsl:if test="$locale!=''">
                            <xsl:attribute name="locale"><xsl:value-of select="$locale"/></xsl:attribute>
                        </xsl:if>
                        <xsl:value-of select="* [position()=$localeId]"/>
                    </xsl:element>
                </xsl:element>
            </xsl:when>

            <xsl:when test="name() = 'toolTip'">
                <xsl:element name="toolTip">
                    <xsl:element name="_item">
                        <xsl:if test="$locale!=''">
                            <xsl:attribute name="locale"><xsl:value-of select="$locale"/></xsl:attribute>
                        </xsl:if>
                        <xsl:value-of select="* [position()=$localeId]"/>
                    </xsl:element>
                </xsl:element>
            </xsl:when>

            <xsl:when test="name() = 'shortText'">
                <xsl:element name="shortText">
                    <xsl:element name="_item">
                        <xsl:if test="$locale!=''">
                            <xsl:attribute name="locale"><xsl:value-of select="$locale"/></xsl:attribute>
                        </xsl:if>
                        <xsl:value-of select="* [position()=$localeId]"/>
                    </xsl:element>
                </xsl:element>
            </xsl:when>

            <xsl:when test="name() = 'longText'">
            	<xsl:element name="longText">
                    <xsl:element name="_item">
                        <xsl:if test="$locale!=''">
                            <xsl:attribute name="locale"><xsl:value-of select="$locale"/></xsl:attribute>
                        </xsl:if>
                        <xsl:value-of select="* [position()=$localeId]"/>
                    </xsl:element>
            	</xsl:element>
            </xsl:when>

            <xsl:otherwise>

                <xsl:choose>
                    <xsl:when test="$removeLangNeutral='true'">
                        <!-- do not process tags that are language-specific -->
                        <xsl:choose>
                            <xsl:when test="name() = 'active'"></xsl:when>
                            <xsl:when test="name() = 'changeable' "></xsl:when>
                            <xsl:when test="name() = 'displayValueExpr' "></xsl:when>
                            <xsl:when test="name() = 'iconKey' "></xsl:when>
                            <xsl:when test="name() = 'filterable' "></xsl:when>
                            <xsl:when test="name() = 'order' "></xsl:when>
                            <xsl:when test="name() = 'orderFieldGroup' "></xsl:when>
                            <xsl:when test="name() = 'orderObjectContainer' "></xsl:when>
                            <xsl:when test="name() = 'sizeXWeight' "></xsl:when>
                            <xsl:when test="name() = 'columnBreak' "></xsl:when>
                            <xsl:when test="name() = 'spanRow' "></xsl:when>
                            <xsl:when test="name() = 'maxLength' "></xsl:when>
                            <xsl:when test="name() = 'minValue' "></xsl:when>
                            <xsl:when test="name() = 'maxValue' "></xsl:when>
                            <xsl:when test="name() = 'decimalPlaces' "></xsl:when>
                            <xsl:when test="name() = 'increment' "></xsl:when>
                            <xsl:when test="name() = 'hasThousandsSeparator' "></xsl:when>
                            <xsl:when test="name() = 'titleIndex' "></xsl:when>
                            <xsl:when test="name() = 'inPlace' "></xsl:when>
                            <xsl:when test="name() = 'format' "></xsl:when>
                            <xsl:when test="name() = 'minDate' "></xsl:when>
                            <xsl:when test="name() = 'maxDate' "></xsl:when>
                            <xsl:when test="name() = 'valueContainer' "></xsl:when>
                            <xsl:when test="name() = 'titleIndex' "></xsl:when>
                            <xsl:when test="name() = 'multiplicity' "></xsl:when>
                            <xsl:when test="name() = 'featureName' "></xsl:when>
                            <xsl:when test="name() = 'qualifiedFeatureName' "></xsl:when>
                            <xsl:when test="name() = 'wordWrap' "></xsl:when>
                            <xsl:when test="name() = 'tabStop' "></xsl:when>
                            <xsl:when test="name() = 'multiline' "></xsl:when>
                            <xsl:when test="name() = 'autoSize' "></xsl:when>
                            <xsl:when test="name() = 'acceptsTab'"></xsl:when>
                            <xsl:when test="name() = 'textAlign' "></xsl:when>
                            <xsl:when test="name() = 'textContentType' "></xsl:when>
                            <xsl:when test="name() = 'contentType' "></xsl:when>
                            <xsl:when test="name() = 'threeState' "></xsl:when>
                            <xsl:otherwise>
                                <!-- process all elements -->
                                <xsl:copy>
                                    <xsl:apply-templates select="@* | node()"/>
                                </xsl:copy>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:when>
                    <xsl:otherwise>
                        <!-- process all elements -->
                        <xsl:copy>
                            <xsl:apply-templates select="@* | node()"/>
                        </xsl:copy>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
</xsl:stylesheet>
