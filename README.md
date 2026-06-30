# Sex Differences in Adult Zebrafish Novel Tank Test (NTT)

This repository contains the datasets supporting the analyses presented in our study investigating sex differences in adult zebrafish during the Novel Tank Test (NTT). The repository is intended to promote transparency, reproducibility, and data reuse.

## Repository Structure

### 📁 ML_training_validation/

Contains all datasets used for machine learning development and evaluation, including:

- Training datasets
- Validation datasets
- Model development data
- Ground-truth behavioral annotations used for supervised learning
- And all scripts necessary to replicate the model

These files correspond to the machine learning pipeline described in the manuscript.

---

### 📁 NTT_5min_data/

Contains the processed datasets used for the primary behavioral analyses across the full 5-minute Novel Tank Test.

These data were used to generate the statistical analyses and figures based on the complete test duration.

---

### 📁 NTT_30sec_data/

Contains datasets summarized into 30-second time bins.

These files were used for the temporal analyses presented in the manuscript, allowing behavioral changes to be evaluated throughout the Novel Tank Test.

---

### 📄 ID.csv

Contains the metadata linking each individual fish to its biological sex.

Columns:

| Column | Description |
|---------|-------------|
| ID | Unique identifier for each fish |
| Sex | Biological sex (Male or Female) |

The ID values correspond to those used throughout all datasets in this repository.


OBS: Researchers interested in reproducing, extending, or reanalyzing the data are encouraged to use these datasets directly.

## Citation

If you use these data, please cite:

*Citation will be added upon publication.*

## Contact

For questions regarding the datasets or repository, please contact the corresponding author.
