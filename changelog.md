# Version 1
* Initial release based on the Lawless Genomics project (mid-2022).
First public commit on Jun 25, 2022 <https://github.com/DylanLawless/genomics_tools/tree/master/iuis_iei_table>.
* Incorporated foundational code for data cleaning, gene splitting, and merging with external annotations.

# Version 2
* Developed with the new IEI genetics page in mind and deployed as the v1 table on the homepage.

# Version 3
**Date:** 2025-03-16
* Done - Greek letters: use labels with Greek letters in cells while making them searchable by their Latin counterparts (e.g. "α" searchable as "a"); add a comment explaining this functionality.
* Done and quite a pain - Major categories dropdown: enable selection of major categories from a dropdown list, showing original names in the dropdown and abbreviations in the table - remain in global search with hidden column.
* Done - Major categories abbreviations: consider using abbreviations from the paper figure (CID, CID+, PAD, PIRD, PD, IID, AID, CD, BMF) for the first column.
* Done - Implement the abbreviations hover.
* Done - Inheritance adjustments: remove the "sporadic/toxin" category (use "sporadic" for DiGeorge as per IUIS tables) and delete the "variable" inheritance category in the CVID phenotype since it appears only once.
* Done - Immunophenotype presentation: standardise column names to avoid ambiguity (e.g. avoid using "count" where qualitative descriptors such as "defective" are used).
* Done - Systematic conversion errors: correct "Wiedemann-Steiner" (previously misconverted to "Widemann-Steiner"), fix "ICOSL deficiency" (erroneously shown as "ICOSLG deficiency") to reflect correct protein/gene naming, and update "IKZF2 deficiency" to "HELIOS deficiency". And several other trivial errors.
* Terminology definitions: include a clear definition of terminology for cleaned labels, outlining which words were changed and their replacements.

* IRF4 disease entries: resolve the discrepancy of three diseases in the 2024 classification versus only two in the table; review the differentiation between "agamma" and "hypogamma" patients and confirm naming as "IRF4 multimorphic (IRF4 R95T)" and "Primary Antibody deficiency/CI D due to IRF4".
    - There are differences between the 2024 PDF report and the linked xlsx table. Ask Isabelle and Stuart for the most updated version of xlsx. Otherwise we must go with the xlsx.
    - Example: here the PDF manuscript lists three IRF4 - the third IRF4 in Table 6.9 is missing in the xlsx. I have to go with the xlsx version.
    - Agamma and hypogamma label? I would report the IUIS data rather than subjectively change which label is used since they both have meaning.
    - Disease naming: I will clean "IRF4 multimorphic R95T" to "IRF4 multimorphic" but otherwise report IUIS.
* AlphaFold and AlphaMissense data and links.
* Further integration with external databases (OMIM, HPO) to enrich gene-disease associations.  
* CliVar whole genome summary and merge with IEI to give spark lines link to clinvar.
* Expanded error checking and handling for ambiguous or missing entries in the raw data.  
* Optimisations for large-scale data processing and improved interactivity of the web-based table interface.

