# Data source

date: 20250316
variant_summary.txt
<https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/>
With more information on the downloads page <https://www.ncbi.nlm.nih.gov/clinvar/docs/maintenance_use/#download>

# Size
The variant summary dataset contained 6,845,091 entries.
We preocessed into 91,319 gene classification groups. For instance, the gene A1BG cotains 4 variants classified as Likely benign.
There were 38983 total gene classifications. For instance, the gene A1BG contains 102 total entries. In some cases multiple genes overlap. 


--------------------------------------------------------------------------------
2. variant_summary.txt
--------------------------------------------------------------------------------
Generated weekly
Archived monthly (first Thurday of each month)


A tab-delimited report based on each variant at a location on the genome for which data have been submitted to ClinVar.  
The data for the variant are reported for each assembly, so most variants have a line for GRCh37 (hg19) and another line for GRCh38 (hg38).
 



AlleleID               integer value as stored in the AlleleID field in ClinVar  (//Measure/@ID in the XML)
Type                   character, the type of variant represented by the AlleleID
Name                   character, ClinVar's preferred name for the record with this AlleleID
GeneID                 integer, GeneID in NCBI's Gene database, reported if there is a single gene, otherwise reported as -1.
GeneSymbol             character, comma-separated list of GeneIDs overlapping the variant
HGNC_ID                string, of format HGNC:integer, reported if there is a single GeneID. Otherwise reported as '-'
ClinicalSignificance   character, semicolon-separated list of aggregate values of germline classification calculated for this variant.
                       Because the aggregate values of germline classification gives precedence to records with assertion criteria 
                       and evidence, the values in this column may appear to be in conflict with the value reported in ClinSigSimple. 
ClinSigSimple          integer, 0 = no current value of Likely pathogenic; Pathogenic; Likely pathogenic, low penetrance;
                                    Pathogenic, low penetrance; Likely risk allele; or Risk allele
                                1 = at least one current record submitted with an interpretation of Likely pathogenic; Pathogenic;
                                    Likely pathogenic, low penetrance; Pathogenic, low penetrance; Likely risk allele; 
                                    or Risk allele (independent of whether that record includes assertion criteria and evidence).
                               -1 = no values for clinical significance at all for this variant or set of variants; used for
                                    the "included" variants that are only in ClinVar because they are included in a
                                    haplotype or genotype with an interpretation
                       NOTE: Now that the aggregate values of clinical significance give precedence to records with
                             assertion criteria and evidence, the values in this column may appear to be in conflict with the
                             value reported in ClinicalSignificance.  In other words, if a submission without assertion criteria and
                             evidence interpreted an allele as pathogenic, and those with assertion criteria and evidence interpreted
                             as benign, then ClinicalSignificance would be reported as Benign and ClinSigSimple as 1.
LastEvaluated          date, the latest "date last evaluated" on submissions with a germline classification for the variant
RS# (dbSNP)            integer, rs# in dbSNP, reported as -1 if missing
nsv/esv (dbVar)        character, the NSV identifier for the region in dbVar
RCVaccession           character, list of RCV accessions that report this variant, separated by a pipe.
PhenotypeIDs           character, list of identifiers for condition(s) this variant was classified for. If there are more than one RCV
                       in the RCVaccession column, the sets of identifiers per RCV are separated by a pipe. Multiple identifiers for
                       the same condition are separated by a comma. If an RCV has more than one condition, the set of identifiers for
                       each condition are separated by a semi-colon. If an RCV has more than 5 conditions, the number of conditions 
                       is reported instead all of the identifiers.                       
PhenotypeList          character, list of condition names corresponding to PhenotypeIDs. If there are more than one RCV in the RCVaccession
                       column, the sets of names per RCV are separated by a pipe. If an RCV has more than one condition, the set of identifiers
                       for each condition are separated by a semi-colon. If an RCV has more than 5 conditions, the number of conditions is
                       reported instead all of the identifiers.                       
Origin                 character, list of all allelic origins for this variant
OriginSimple           character, processed from Origin to make it easier to distinguish between germline and somatic
Assembly               character, name of the assembly on which locations are based  
ChromosomeAccession    Accession and version of the RefSeq sequence defining the position reported in the start and stop columns. 
                       Please note some of these accessions may be for sub-chromosomal regions.
Chromosome             character, chromosomal location
Start                  integer, starting location, right-shifted, in pter->qter orientation
Stop                   integer, end location, right-shifted, in pter->qter orientation
ReferenceAllele        The reference allele using the right-shifted location in Start and Stop.
AlternateAllele        The alternate allele using the right-shifted location in Start and Stop.
Cytogenetic            character, ISCN band
ReviewStatus           character, review status for the aggregate germline classification for this variant. For the key to the terms, 
                       and the stars displayed on ClinVar web pages, see http://www.ncbi.nlm.nih.gov/clinvar/docs/review_status/
NumberSubmitters       integer, number of submitters describing this variant
Guidelines             character, ACMG only right now, for the reporting of incidental variation in a Gene 
                       enumerates whether the guideline is from 2013 (ACMG2013, PubMed 23788249) or 2016 (ACMG2016, PubMed 27854360)
                       (NOTE: if ACMG, not specific to the AlleleID but to the Gene in which the AlleleID is found)
TestedInGTR            character, Y/N for Yes/No if there is a test registered as specific to this variant 
                       in the NIH Genetic Testing Registry (GTR)
OtherIDs               character, list of other identifiers or sources of information about this variant
SubmitterCategories    coded value to indicate whether data were submitted by another resource (1), any other type of source (2), both
		       (3), or none (4)
VariationID            The identifier ClinVar uses specific to the AlleleID.  Not all VariationIDS that may be related to
                       the AlleleID are reported in this file. For a comprehensive mapping of AlleleID to VariationID,
		       please use ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variation_allele.txt.gz.
PositionVCF	       integer, starting location, left-shifted, in pter->qter orientation
ReferenceAlleleVCF     The reference allele using the left-shifted location in vcf_pos.
AlternateAlleleVCF     The alternate allele using the left-shifted location in vcf_pos.
SomaticClinicalImpact			character, aggregate value for somatic classification of clinical impact calculated for this variant.
SomaticClinicalImpactLastEvaluated	date, the latest "date last evaluated" on submissions with a somatic classification of clinical impact for the variant
ReviewStatusClinicalImpact		character, review status for the aggregate somatic classification of clinical impact for this variant. For the key to the terms, 
                       and the stars displayed on ClinVar web pages, see http://www.ncbi.nlm.nih.gov/clinvar/docs/review_status/
Oncogenicity				character, aggregate value of oncenicity classification calculated for this variant.
OncogenicityLastEvaluated		date, the latest "date last evaluated" on submissions with an oncogenicity classification for the variant
ReviewStatusOncogenicity		character, review status for the aggregate oncogenicity classification for this variant. For the key to the terms, 
                       and the stars displayed on ClinVar web pages, see http://www.ncbi.nlm.nih.gov/clinvar/docs/review_status/
							  
                     
