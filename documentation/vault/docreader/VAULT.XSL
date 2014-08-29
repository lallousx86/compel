<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">

<xsl:template match="/">
	<HTML>
		<HEAD>
			<TITLE>Vault File</TITLE>
		</HEAD>
		<BODY>
			<xsl:apply-templates select="//ITEM" />
		</BODY>
	</HTML>
</xsl:template>

<xsl:template match="ITEM">
	<P>
	<xsl:apply-templates select="TITLE" />
	<xsl:apply-templates select="TEXT" />
	</P>
</xsl:template>

<xsl:template match="TITLE">
	<P><B><xsl:value-of /></B></P>
</xsl:template>

<xsl:template match="TEXT">
	<xsl:if match="TEXT[@type='text']"><xsl:value-of /></xsl:if>
	<xsl:if match="TEXT[@type='newline']"><BR /></xsl:if>
</xsl:template>

</xsl:stylesheet>