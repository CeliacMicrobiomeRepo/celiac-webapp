# Metadata

---

## Acronyms
- **16S:** 16S ribosomal RNA gene sequencing
- **SG:** Shotgun metagenomics
- **CD:** Celiac disease
- **GFD:** Gluten-free diet
- **ACD:** Active Celiac Disease (not on GFD)
- **TCD:** Treated Celiac Disease (on GFD)
- **HC:** Healthy Control
- **HC_GFD:** Healthy Control on a Gluten-Free Diet
- **PCD:** Prospective Celiac Disease
- **PHC:** Prospective Healthy Control

---

## Samples Metadata Dictionary
Contains metadata for every sample included in CMR v{{VERSION}}. The file contains the following columns:
  - `Sample_ID`: Unique identifier assigned to each sample within the CMR.
  - `Dataset_ID`: Unique identifier for the dataset the sample belongs to.
  - `SRA_Run_ID`: Sequence Read Archive (SRA) run accession number for the sample. 'NA' if not on SRA.
  - `SRA_Project_ID`: SRA BioProject accession number for the study. 'NA' if not on SRA.
  - `Month_of_Publication`: Month and year the associated study was published (e.g. Feb-19).
  - `Publication_DOI`: Digital Object Identifier (DOI) for the associated publication.
  - `Sequencing_Type`: The sequencing method used ('16S' or 'SG').
  - `Amplicon_Region`: The targeted 16S rRNA variable region(s) (e.g., 'V3-V4', 'V4'). 'NA' for shotgun data.
  - `Num_Reads_Input`: The number of raw reads in the input fastq file(s) to DADA2. 'NA' for shotgun data.
  - `Num_Reads_Filtered`: The number of reads passing the quality filtering and trimming step (filterAndTrim) in DADA2. 'NA' for shotgun data.
  - `Num_Reads_DenoisedF`: The number of reads remaining after denoising (error correction) the forward reads in DADA2. 'NA' for shotgun data.
  - `Num_Reads_Nonchim`: The number of sequencing reads remaining after DADA2 processing (quality filtering, chimera removal, etc.). This is the number of observed ASV counts per sample. 'NA' for shotgun data.
  - `Total_Pairs_Pre_Host_Removal`: The number of read pairs in the raw sequencing data before running Bowtie2 for host read removal. 'NA' for 16S data.
  - `Percent_Host_Reads_Removed`: The percent of read pairs removed from the raw sequencing data by running Bowtie2 for host read removal. 'NA' for 16S data.
  - `Percent_Unclassified_Reads`: The percentage of reads inputted into MetaPhlAn that were unclassified. 'NA' for 16S data.
  - `Num_SGBs`: The number of unique SGBs in the sample. 'NA' for 16S data.
  - `Num_ASVs`: The number of unique ASVs in the sample. 'NA' for shotgun data.
  - `Seq_Tech`: The specific sequencing technology used (e.g., 'Illumina MiSeq', 'Pyrosequencing').
  - `DNA_Ext_Kit`: The DNA extraction kit used, if reported. 'unavailable/unclear' otherwise.
  - `DNA_Extraction_Is_Mechanical`: TRUE if the DNA extraction method used bead beating or another mechanical method such as 'lysing matrix particles' in 'FastDNA Spin Kit for Soil'. Note that while some studies employed a non-mechanical kit, they performed a additional lysis step using a bead mill such as MagNA Lyser or FastPrep.
  - `Paired_Reads`: Indicates if sequencing was paired-end ('TRUE') or single-end ('FALSE').
  - `Sample_Site`: The body site from which the sample was collected (e.g., 'stool', 'duodenum').
  - `Diagnosed_Celiac`: Indicates if the individual was diagnosed with celiac disease at the time of sampling ('TRUE', 'FALSE'). 'NA' if prospective study.
  - `Gluten_Free_Diet`: Indicates if the individual was adhering to a gluten-free diet (GFD) ('TRUE' or 'FALSE'). 'NA' if prospective study.
  - `Will_Develop_Celiac`: In prospective studies, indicates if the individual later developed celiac disease ('TRUE', 'FALSE', 'NA').
  - `Group`: A simplified grouping based on disease status and diet ('TCD' for treated celiac, 'ACD' for active celiac, 'HC' for Healthy Control, 'HC_GFD' for Healthy Control on a GFD, 'PCD' for Prospective Celiac Disease, 'PHC' for Prospective Healthy Control).
  - `Short_term_Gluten_Challenge`: Indicates if the sample was taken during or shortly after a short-term gluten challenge ('TRUE', 'FALSE').
  - `NCGS`: Indicates if the individual was diagnosed with Non-Celiac Gluten Sensitivity ('TRUE', 'FALSE').
  - `Other_Autoimmune`: Indicates if the individual had other reported autoimmune diseases ('TRUE', 'FALSE').
  - `Hookworm`: Indicates if the individual had a recent or current hookworm infection ('TRUE', 'FALSE').
  - `Possible_Celiac`: Indicates if the diagnosis was reported as 'possible' or 'potential' celiac disease ('TRUE', 'FALSE').
  - `Any_Significant_Factor`: A summary flag ('TRUE'/'FALSE') indicating the presence of any potentially confounding factors (Gluten Challenge, NCGS, Other Autoimmune, Hookworm, Possible Celiac).
  - `Country`: The country where the sample was collected.
  - `Age`: The age of the individual at the time of sampling, if reported. If specific age is unknown, an age range (e.g. '19-65') may be provided, 'unknown' otherwise.
  - `Sex`: The sex of the individual, if reported ('Male', 'Female', 'unknown').

---

## Included Datasets Metadata Dictionary
Lists all datasets included in this version, along with key information like publication details, SRA references, sample counts, country, and sample site(s). It contains the following columns:
  - `Dataset_ID`: Unique identifier assigned to the dataset within the CMR.
  - `Bioproject_ID`: NCBI BioProject ID associated with the dataset, if available.
  - `Record_Link`: URL link to the primary data record (e.g., NCBI BioProject, specific database).
  - `Publication_Title`: Title of the associated peer-reviewed publication.
  - `Publication_Link`: URL link to the publication page.
  - `Month_Of_Publication`: Month and year the study was published (e.g., Dec-21).
  - `DOI`: Digital Object Identifier for the publication.
  - `Used_In_Previous_Meta_Analysis`: Indicates if the dataset was included in known prior meta-analyses (TRUE/FALSE).
  - `Lit_Search_Source`: The database where the study was initially identified (e.g., 'NCBI SRA', 'Scopus').
  - `Data_Source`: How the raw sequencing data was obtained (e.g., 'NCBI SRA', 'Shared via email').
  - `Sequencing_Type`: High-throughput sequencing method used ('16S' or 'SG').
  - `Sequencing_Technology`: Specific sequencing platform used (e.g., 'Illumina MiSeq', 'Ion Torrent').
  - `Prospective_Study`: Indicates if the study design was prospective (TRUE/FALSE).
  - `Sample_Sites`: Body site(s) from which samples were collected (e.g., 'stool', 'duodenum', 'stool|duodenum').
  - `Amplicon_Region`: Targeted 16S rRNA variable region(s) (e.g., 'V3-V4', 'V4'). 'NA' for shotgun data.
  - `Forward_Primer`: Forward primer sequence used for amplification, if reported.
  - `Reverse_Primer`: Reverse primer sequence used for amplification, if reported.
  - `DNA_Extraction_Kit`: DNA extraction kit/method used, if reported.
  - `DNA_Extraction_Is_Mechanical`: TRUE if the DNA extraction method used bead beating or another mechanical method such as 'lysing matrix particles' in 'FastDNA Spin Kit for Soil'. Note that while some studies employed a non-mechanical kit, they performed an additional lysis step using a bead mill such as MagNA Lyser or FastPrep.
  - `Read_Pairing`: Indicates if sequencing reads were paired-end ('paired') or single-end ('single').
  - `Trimming_Of_Reads_After_Acquisition`: Indicates if any trimming was performed after data acquisition but before CMR processing (TRUE/FALSE or specific tool).
  - `Bowtie2_Alignment_Sensitivity`: Sensitivity preset used for Bowtie2 alignment in host read removal for shotgun data.
  - `Host_Genome_Index`: Reference genome index used for host read removal.
  - `MetaPhlAn_Database`: Specific database version used for taxonomic profiling with MetaPhlAn in shotgun data.
  - `Fw_Read_Trim_Position`: Forward read trimming length/position used in DADA2, if applicable.
  - `Rv_Read_Trim_Position`: Reverse read trimming length/position used in DADA2, if applicable.
  - `ASV_Table_Length_Filter`: Specific length filter applied to the ASV table post-DADA2, if any.
  - `Notes_From_Processing`: Any relevant notes made during data processing.
  - `Median_Num_SGBs`: Median number of unique SGBs per sample in the dataset.
  - `Total_Num_SGBs`: Total number of unique SGBs in the dataset.
  - `Median_Num_ASVs`: Median number of unique ASVs per sample in the dataset.
  - `Total_Num_ASVs`: Total number of unique ASVs in the dataset.
  - `Age_Range`: Age range of participants in the study, if reported.
  - `Has_Sex_Metadata`: Indicates if sex metadata is available for all (TRUE) or no (FALSE) samples in the dataset.
  - `Has_Age_Metadata`: Indicates if age metadata is available for all (TRUE) or no (FALSE) samples in the dataset.
  - `Num_Samples`: Total number of samples from this dataset included in the final CMR (processed and with metadata).
  - `Num_Individuals`: Total number of unique individuals represented by the included samples (processed and with metadata).
  - `Num_Celiac_Samples`: Number of samples from diagnosed celiac individuals (processed and with metadata).
  - `Num_GFD_Samples`: Number of samples from individuals on a gluten-free diet (processed and with metadata).
  - `Num_Prospective_Celiac_Samples`: Number of samples from individuals who later developed celiac disease (in prospective studies) (processed and with metadata).
  - `Longitudinal_Study`: Indicates if the study involved sampling the same individuals at multiple time points (TRUE/FALSE).
  - `Country`: Country where the study was conducted/samples collected.
  - `Samples_With_Significant_Factors`: Lists potential confounding factors present in some samples (e.g., 'gluten challenge', 'NCWS'). FALSE if no significant factors were present.
  - `Prospective_Studies`: Boolean flag indicating a prospective study design (TRUE/FALSE).
  - `Shotgun_Studies`: Boolean flag indicating a shotgun metagenomics dataset (TRUE/FALSE).
  - `Study_Design_Description`: Brief textual description of the study's design and comparison groups.
  - `Multiple_Publications`: If a dataset is split across multiple publications it is noted here. FALSE if only one publication.

---

## Excluded Datasets Metadata Dictionary
Lists datasets that were identified as eligible but excluded, along with the primary reason for exclusion. It contains the following columns:
  - `Publication_Title`: Title of the associated peer-reviewed publication.
  - `Publication_Link`: A URL link to the publication page.
  - `Month_Of_Publication`: The month and year the study was published (e.g., Jun-23).
  - `DOI`: The Digital Object Identifier for the publication.
  - `Record_Link`: A URL linking to the dataset record in a public repository (e.g., SRA), if applicable.
  - `Project_ID`: The unique identifier for the project in a public repository (e.g., SRA BioProject ID like PRJNA######), if applicable.
  - `Lit_Search_Source`: The database where the study was initially identified (e.g., 'NCBI SRA', 'Scopus').
  - `Claim_Of_Data_Availabile_Upon_Request`: If within the publication there was a statement equivalent to "the data from this study will be made available from the corresponding authors upon reasonable request" this is TRUE, otherwise FALSE.
  - `Data_Availability`: Status indicating whether the raw sequencing data was accessible (e.g., 'unavailable', 'listed on SRA', 'available on SRA').
  - `Reason_For_Exclusion`: The specific reason why the dataset was not included in the CMR (e.g., 'no email response', 'privacy controls', 'large fees').
  - `Sequencing_Type`: The high-throughput sequencing method used ('16S' or 'SG').

---

## Dataset Naming
Datasets are given a unique ID of the format: 

`<SequencingType>_<NSamples>_<AuthorLastName>`

Where:
- **`<SequencingType>`:** Indicates the sequencing method used ('16S' for 16S rRNA or 'SG' for Whole Genome Shotgun metagenomics).
- **`<NSamples>`:** A unique numerical identifier assigned to the dataset within the CMR.
- **`<AuthorLastName>`:** The last name of the first author of the associated publication.

---
