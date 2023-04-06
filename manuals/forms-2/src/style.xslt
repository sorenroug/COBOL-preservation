<?xml version='1.0'?>
<xsl:stylesheet
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:d="http://docbook.org/ns/docbook"
        xmlns:fo="http://www.w3.org/1999/XSL/Format"
        xmlns:xlink='http://www.w3.org/1999/xlink'
        xmlns:cbl="http://www.roug.org/cobol/v1.0"
        xmlns:fox="http://xmlgraphics.apache.org/fop/extensions"
        exclude-result-prefixes="xlink d"
        version="1.0">

  <xsl:import href="urn:docbkx:stylesheet" />

  <xsl:param name="arg.choice.ansi.close.str"><xsl:text>]</xsl:text></xsl:param>
  <xsl:param name="arg.choice.ansi.open.str"><xsl:text>[</xsl:text></xsl:param>
  <xsl:param name="arg.choice.def.close.str"><xsl:text> </xsl:text></xsl:param>
  <xsl:param name="arg.choice.def.open.str"><xsl:text> </xsl:text></xsl:param>
  <xsl:param name="chapter.autolabel" select="1"/>
  <xsl:param name="double.sided" select="1"/>
  <xsl:param name="generate.toc" select="'book toc,title,figure,table'"/>
  <xsl:param name="header.column.widths">1 4 1</xsl:param>
  <xsl:param name="paper.type" select="'A4'"/>
  <xsl:param name="toc.section.depth" select="5"/>

<xsl:attribute-set name="toc.line.properties">
  <xsl:attribute name="font-weight">
   <xsl:choose>
    <xsl:when test="self::d:chapter | self::d:preface | self::d:appendix">bold</xsl:when>
    <xsl:otherwise>normal</xsl:otherwise>
   </xsl:choose>
  </xsl:attribute>
</xsl:attribute-set>

  <xsl:template name="inline.keycapseq">
    <xsl:param name="content">
      <xsl:apply-templates/>
    </xsl:param>

    <xsl:param name="contentwithlink">
      <xsl:call-template name="simple.xlink">
        <xsl:with-param name="content" select="$content"/>
      </xsl:call-template>
    </xsl:param>

    <fo:inline font-size=".85em" border-style="outset" border-width="1pt" padding-top=".1em">
      <xsl:if test="@dir">
        <xsl:attribute name="direction">
          <xsl:choose>
            <xsl:when test="@dir = 'ltr' or @dir = 'lro'">ltr</xsl:when>
            <xsl:otherwise>rtl</xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
      </xsl:if>
      <xsl:copy-of select="$contentwithlink"/>
    </fo:inline>
  </xsl:template>

  <xsl:template match="d:keycap">
    <xsl:call-template name="inline.keycapseq"/>
  </xsl:template>

<xsl:template match="d:phrase">
  <xsl:choose>
    <xsl:when test="@role='extension'">
      <xsl:call-template name="inline.extension"/>
    </xsl:when>
    <xsl:otherwise>
      <fo:inline>
        <xsl:call-template name="anchor"/>
        <xsl:call-template name="inline.charseq"/>
      </fo:inline>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="inline.extension">
  <xsl:param name="content">
    <xsl:apply-templates/>
  </xsl:param>

  <xsl:param name="contentwithlink">
    <xsl:call-template name="simple.xlink">
      <xsl:with-param name="content" select="$content"/>
    </xsl:call-template>
  </xsl:param>

  <fo:inline background-color="#d0d0d0">
    <xsl:if test="@dir">
      <xsl:attribute name="direction">
        <xsl:choose>
          <xsl:when test="@dir = 'ltr' or @dir = 'lro'">ltr</xsl:when>
          <xsl:otherwise>rtl</xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
    </xsl:if>
    <xsl:copy-of select="$contentwithlink"/>
  </fo:inline>
</xsl:template>

<xsl:template match="cbl:synopsis">
  <fo:block xsl:use-attribute-sets="normal.para.spacing">
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<xsl:template match="d:group|d:arg" name="group-or-arg">
  <xsl:variable name="choice" select="@choice"/>
  <xsl:variable name="rep" select="@rep"/>
  <xsl:variable name="sepchar">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::*/@sepchar">
        <xsl:value-of select="ancestor-or-self::*/@sepchar"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text> </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="preceding-sibling::*">
    <xsl:value-of select="$sepchar"/>
  </xsl:if>
  <xsl:choose>
    <xsl:when test="$choice='ansi'">
      <fo:inline text-decoration="line-through">
        <xsl:value-of select="$arg.choice.ansi.open.str"/>
      </fo:inline>
    </xsl:when>
    <xsl:when test="$choice='plain'">
      <xsl:value-of select="$arg.choice.plain.open.str"/>
    </xsl:when>
    <xsl:when test="$choice='req'">
      <xsl:value-of select="$arg.choice.req.open.str"/>
    </xsl:when>
    <xsl:when test="$choice='opt'">
      <xsl:value-of select="$arg.choice.opt.open.str"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$arg.choice.def.open.str"/>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:apply-templates/>

  <xsl:choose>
    <xsl:when test="$choice='ansi'">
      <fo:inline text-decoration="line-through">
        <xsl:value-of select="$arg.choice.ansi.close.str"/>
      </fo:inline>
    </xsl:when>
    <xsl:when test="$choice='plain'">
      <xsl:value-of select="$arg.choice.plain.close.str"/>
    </xsl:when>
    <xsl:when test="$choice='req'">
      <xsl:value-of select="$arg.choice.req.close.str"/>
    </xsl:when>
    <xsl:when test="$choice='opt'">
      <xsl:value-of select="$arg.choice.opt.close.str"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$arg.choice.def.close.str"/>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:choose>
    <xsl:when test="$rep='repeat'">
      <xsl:value-of select="$arg.rep.repeat.str"/>
    </xsl:when>
    <xsl:when test="$rep='norepeat'">
      <xsl:value-of select="$arg.rep.norepeat.str"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$arg.rep.def.str"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="d:group/d:arg">
  <xsl:variable name="choice" select="@choice"/>
  <xsl:variable name="rep" select="@rep"/>
  <xsl:if test="preceding-sibling::*">
    <xsl:value-of select="$arg.or.sep"/>
  </xsl:if>
  <xsl:call-template name="group-or-arg"/>
</xsl:template>

<xsl:template name="header.content">
  <xsl:param name="pageclass" select="''"/>
  <xsl:param name="sequence" select="''"/>
  <xsl:param name="position" select="''"/>
  <xsl:param name="gentext-key" select="''"/>

<!--
  <fo:block>
    <xsl:value-of select="$pageclass"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$sequence"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$position"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$gentext-key"/>
  </fo:block>
-->

  <fo:block>

    <!-- sequence can be odd, even, first, blank -->
    <!-- position can be left, center, right -->
    <xsl:choose>
      <xsl:when test="$sequence = 'blank'">
        <!-- nothing -->
      </xsl:when>

      <xsl:when test="$position='left'">
        <!-- Same for odd, even, empty, and blank sequences -->
        <xsl:call-template name="draft.text"/>
      </xsl:when>

      <xsl:when test="($sequence='odd' or $sequence='even') and $position='center'">
        <xsl:if test="$pageclass != 'titlepage'">
          <xsl:apply-templates select="." mode="object.title.markup"/>
        </xsl:if>
      </xsl:when>

      <xsl:when test="$position='center'">
        <!-- nothing for empty and blank sequences -->
      </xsl:when>

      <xsl:when test="$position='right'">
        <!-- Same for odd, even, empty, and blank sequences -->
        <xsl:call-template name="draft.text"/>
      </xsl:when>

      <xsl:when test="$sequence = 'first'">
        <!-- nothing for first pages -->
      </xsl:when>

      <xsl:when test="$sequence = 'blank'">
        <!-- nothing for blank pages -->
      </xsl:when>
    </xsl:choose>
  </fo:block>
</xsl:template>

<xsl:attribute-set name="monospace.verbatim.properties">
  <xsl:attribute name="border">1px solid #c6c8cb</xsl:attribute>
  <xsl:attribute name="background-color">#f0f0f0</xsl:attribute>
  <!--
  <xsl:attribute name="border-collapse">separate</xsl:attribute>
  <xsl:attribute name="fox:border-radius">10px</xsl:attribute>
  <xsl:attribute name="padding">10px</xsl:attribute>
  -->
</xsl:attribute-set>

</xsl:stylesheet>
