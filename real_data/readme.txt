Since access to MIMIC-III requires approval, we do not include the original data files in this repository. Instead, we provide the code used to construct the analysis dataset.

Step 1: Set up and configure MIMIC III database from https://mimic.physionet.org/.

Step 2: Prepare MIMIC sepsis dataset according to https://github.com/microsoft/mimic_sepsis.

Step 3: Run load_data_new_comp.ipynb to obtain the final data file.

Step 4: Run main.r. and main_add.r to get the results.
